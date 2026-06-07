;;; mu4e-gnaw.el --- Highlight BONE reports in mu4e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry
;;
;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: mail
;; URL: https://codeberg.org/bzg/mu4e-gnaw
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (gnaw "0.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; M-x mu4e-gnaw RET           -- search for open BONE reports and highlight them
;; M-x mu4e-gnaw-topic RET     -- same, filtered by topic
;; M-x mu4e-gnaw-highlight RET -- highlight matches in the current headers buffer
;; M-x mu4e-gnaw-clear RET     -- remove highlights
;; M-x gnaw-update RET         -- force update of the remote reports cache
;;
;; The following commands toggle gnaw's local marks (kept in
;; ~/.config/gnaw/state.edn so they are shared with the gnaw CLI):
;;
;; M-x mu4e-gnaw-mark-sticky RET -- toggle the sticky mark (keep visible)
;; M-x mu4e-gnaw-mark-skip RET -- toggle the skip mark (hide)
;;
;; The annotation gains a leading mark column: '*' = sticky, '_' = skip.
;;
;; mu4e-gnaw builds on the `gnaw' library for the shared data layer
;; (configuration, report sources, cache and state.edn); this file only
;; provides the mu4e presentation and commands.
;;
;;; Code:

(require 'gnaw)
(require 'cl-lib)
(require 'subr-x)
(require 'time-date)
(require 'mu4e)

(declare-function mu4e-message-at-point "mu4e-message")
(declare-function mu4e-message-field "mu4e-message")
(declare-function mu4e-headers-for-each "mu4e-headers")
(declare-function mu4e-headers-search "mu4e-headers")

(defgroup mu4e-gnaw nil
  "Highlight BONE reports in mu4e headers."
  :group 'mu4e)

(defface mu4e-gnaw-face
  '((((background light)) :background "#e8e8e8")
    (((background dark))  :background "#333333"))
  "Subtle highlight for BONE reports in mu4e headers."
  :group 'mu4e-gnaw)

(defface mu4e-gnaw-annotation-face
  '((t :inherit shadow))
  "Face for right-margin annotations."
  :group 'mu4e-gnaw)

(defvar mu4e-gnaw-votes-width 7
  "Fixed width for the votes column.")

(defvar mu4e-gnaw-deadline-width 5
  "Fixed width for the deadline column.")

(defvar mu4e-gnaw-expiry-width 5
  "Fixed width for the expiry column.")

;; --- Message-id helpers ---------------------------------------------------

(defun mu4e-gnaw--bare-mid (mid)
  "Strip surrounding angle brackets from MID."
  (let ((s (or mid "")))
    (if (and (string-prefix-p "<" s) (string-suffix-p ">" s))
        (substring s 1 -1)
      s)))

;; --- Annotation formatting ------------------------------------------------

(defun mu4e-gnaw--mark-prefix (entry)
  "Get mark char for state ENTRY."
  (let ((flag (cdr (assq :flag entry)))
        (skip (cdr (assq :skip-since entry))))
    (cond
     ((eq flag :sticky) "*")
     (skip "_")
     (t " "))))

(defun mu4e-gnaw--type-letter (type)
  "Get letter abbreviation for TYPE."
  (pcase type
    ("bug"          "B")
    ("patch"        "P")
    ("request"      "?")
    ("announcement" "A")
    ("release"      "R")
    ("change"       "C")
    (_              "·")))

(defun mu4e-gnaw--deadline-days (deadline)
  "Days until YYYY-MM-DD DEADLINE."
  (when deadline
    (let* ((dl (date-to-time (concat deadline " 00:00:00")))
           (diff (float-time (time-subtract dl (current-time)))))
      (ceiling (/ diff 86400.0)))))

(defun mu4e-gnaw--annotation (info &optional entry)
  "Build annotation string for report INFO and state ENTRY."
  (let* ((mark     (mu4e-gnaw--mark-prefix entry))
         (type     (mu4e-gnaw--type-letter (plist-get info :type)))
         (flags    (plist-get info :flags))
         (priority (plist-get info :priority))
         (votes    (plist-get info :votes))
         (deadline (plist-get info :deadline))
         (expiry   (plist-get info :expiry))
         (dl-days  (mu4e-gnaw--deadline-days deadline))
         (ex-days  (mu4e-gnaw--deadline-days expiry))
         (pri-str  (pcase priority (3 "A") (2 "B") (1 "C") (_ " ")))
         (dl-str   (if dl-days (format "D%+d" dl-days) ""))
         (dl-pad   (string-pad dl-str mu4e-gnaw-deadline-width))
         (ex-str   (if ex-days (format "E%+d" ex-days) ""))
         (ex-pad   (string-pad ex-str mu4e-gnaw-expiry-width))
         (votes-str (if votes (format "[%s]" votes) ""))
         (votes-pad (string-pad votes-str mu4e-gnaw-votes-width)))
    (concat mark " " type " " flags " " pri-str " " dl-pad ex-pad votes-pad)))

;; --- Query building -------------------------------------------------------

(defun mu4e-gnaw--build-query (reports)
  "Build query string for REPORTS."
  (mapconcat (lambda (r)
               (format "msgid:%s" (mu4e-gnaw--bare-mid (car r))))
             reports
             " OR "))

(defun mu4e-gnaw--build-mid-map (reports)
  "Build mapping from bare message-id to info for REPORTS."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (r reports)
      (puthash (mu4e-gnaw--bare-mid (car r)) (cdr r) ht))
    ht))

;; --- Overlay highlighting -------------------------------------------------

(defvar-local mu4e-gnaw--active-reports nil
  "Buffer-local cache of BONE reports for auto-rehighlighting.")

(defvar mu4e-gnaw--pending-reports nil
  "Global pending reports awaiting next search finish.")

(defun mu4e-gnaw--apply-overlays (reports)
  "Apply overlays for REPORTS in the current `mu4e-headers-mode' buffer."
  (remove-overlays (point-min) (point-max) 'mu4e-gnaw t)
  (when (derived-mode-p 'mu4e-headers-mode)
    (let ((id-map (mu4e-gnaw--build-mid-map reports))
          (state  (gnaw-read-state)))
      (mu4e-headers-for-each
       (lambda (msg)
         (when-let* ((raw  (mu4e-message-field msg :message-id))
                     (mid  (mu4e-gnaw--bare-mid raw))
                     (info (gethash mid id-map)))
           (let* ((entry   (cdr (assoc (gnaw-normalize-mid mid) state)))
                  (bol     (line-beginning-position))
                  (eol     (line-end-position))
                  (ann-str (mu4e-gnaw--annotation info entry))
                  (p3      (= 3 (plist-get info :priority)))
                  (face    (if p3 '(mu4e-gnaw-face bold) 'mu4e-gnaw-face))
                  (ov      (make-overlay bol eol)))
             (overlay-put ov 'face face)
             (overlay-put ov 'mu4e-gnaw t)
             (overlay-put ov 'before-string
                          (propertize (concat ann-str " ")
                                      'face 'mu4e-gnaw-annotation-face)))))))))

(defun mu4e-gnaw--rehighlight ()
  "Re-apply overlays on search update."
  (when mu4e-gnaw--active-reports
    (mu4e-gnaw--apply-overlays mu4e-gnaw--active-reports)))

(defun mu4e-gnaw--install-pending ()
  "Install pending reports as active cache in this buffer."
  (remove-hook 'mu4e-headers-found-hook #'mu4e-gnaw--install-pending)
  (when-let* ((reports mu4e-gnaw--pending-reports))
    (setq mu4e-gnaw--pending-reports nil)
    (when (derived-mode-p 'mu4e-headers-mode)
      (setq mu4e-gnaw--active-reports reports)
      (add-hook 'mu4e-headers-found-hook #'mu4e-gnaw--rehighlight nil t)
      (mu4e-gnaw--apply-overlays reports))))

(defun mu4e-gnaw--search-and-watch (reports label)
  "Search mu for REPORTS and install overlays once done.
LABEL annotates the status message."
  (setq mu4e-gnaw--pending-reports reports)
  (add-hook 'mu4e-headers-found-hook #'mu4e-gnaw--install-pending)
  (mu4e-headers-search (mu4e-gnaw--build-query reports))
  (message "Searching %d BONE reports%s." (length reports) label))

;; --- Interactive commands -------------------------------------------------

;;;###autoload
(defun mu4e-gnaw ()
  "Search mu4e for open BONE reports and highlight them."
  (interactive)
  (let ((reports (gnaw-reports)))
    (if (null reports)
        (message "No open BONE reports found.")
      (mu4e-gnaw--search-and-watch reports ""))))

;;;###autoload
(defun mu4e-gnaw-highlight ()
  "Highlight BONE reports in current headers buffer."
  (interactive)
  (unless (derived-mode-p 'mu4e-headers-mode)
    (user-error "Not in a mu4e-headers buffer"))
  (let ((reports (gnaw-reports)))
    (if (null reports)
        (message "No open BONE reports found.")
      (setq mu4e-gnaw--active-reports reports)
      (add-hook 'mu4e-headers-found-hook #'mu4e-gnaw--rehighlight nil t)
      (mu4e-gnaw--apply-overlays reports)
      (message "Highlighted %d BONE reports." (length reports)))))

(defun mu4e-gnaw--collect-topics (reports)
  "Sorted list of topics in REPORTS."
  (let ((topics nil))
    (dolist (r reports)
      (let ((topic (plist-get (cdr r) :topic)))
        (when topic
          (cl-pushnew topic topics :test #'equal))))
    (sort (copy-sequence topics) #'string<)))

(defun mu4e-gnaw--filter-by-topic (reports topic)
  "Return REPORTS matching TOPIC."
  (cl-remove-if-not (lambda (r) (equal (plist-get (cdr r) :topic) topic))
                    reports))

;;;###autoload
(defun mu4e-gnaw-topic ()
  "Search BONE reports filtered by topic."
  (interactive)
  (let* ((reports (gnaw-reports))
         (topics  (mu4e-gnaw--collect-topics reports)))
    (cond
     ((null reports) (message "No open BONE reports found."))
     ((null topics)  (message "No topics in any report."))
     (t
      (let* ((topic    (completing-read "BONE topic: " topics nil t))
             (filtered (and (not (string= topic ""))
                            (mu4e-gnaw--filter-by-topic reports topic))))
        (cond
         ((or (string= topic "") (null filtered))
          (message "No reports for topic \"%s\"." topic))
         (t
          (mu4e-gnaw--search-and-watch
           filtered (format " for topic \"%s\"" topic)))))))))

;;;###autoload
(defun mu4e-gnaw-clear ()
  "Remove overlays and disable re-highlighting."
  (interactive)
  (remove-overlays (point-min) (point-max) 'mu4e-gnaw t)
  (setq mu4e-gnaw--active-reports nil)
  (remove-hook 'mu4e-headers-found-hook #'mu4e-gnaw--rehighlight t))

;; --- Marking commands -----------------------------------------------------

(defun mu4e-gnaw--current-mid ()
  "Current header line's bare message-id, or nil."
  (when-let* ((msg (ignore-errors (mu4e-message-at-point)))
              (mid (mu4e-message-field msg :message-id)))
    (mu4e-gnaw--bare-mid mid)))

(defun mu4e-gnaw--info-for-mid (mid reports)
  "Return info plist for MID in REPORTS."
  (cdr (assoc (gnaw-normalize-mid mid) reports)))

(defun mu4e-gnaw--mark (action on-msg off-msg)
  "Toggle ACTION mark, showing ON-MSG or OFF-MSG."
  (let* ((reports (or mu4e-gnaw--active-reports (gnaw-reports)))
         (mid     (and reports (mu4e-gnaw--current-mid)))
         (info    (and mid (mu4e-gnaw--info-for-mid mid reports))))
    (cond
     ((null reports) (user-error "No BONE reports loaded"))
     ((null mid)     (user-error "No message-id on current line"))
     ((null info)    (user-error "Current message is not a BONE report: %s" mid))
     (t
      (let ((on (gnaw-toggle-mark (gnaw-normalize-mid mid) info action)))
        (mu4e-gnaw--rehighlight)
        (message "%s" (if on on-msg off-msg)))))))

;;;###autoload
(defun mu4e-gnaw-mark-sticky ()
  "Toggle the sticky mark (keep visible) for the current report."
  (interactive)
  (mu4e-gnaw--mark :sticky "Marked sticky" "Unmarked sticky"))

;;;###autoload
(defun mu4e-gnaw-mark-skip ()
  "Toggle the skip mark (hide) for the current report."
  (interactive)
  (mu4e-gnaw--mark :skip "Skipped" "Unskipped"))

;; --- Cache update hooks ----------------------------------------------------

(defun mu4e-gnaw--refresh-all-buffers ()
  "Refresh mu4e-gnaw overlays in all headers buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'mu4e-headers-mode)
                 mu4e-gnaw--active-reports)
        (mu4e-gnaw--apply-overlays mu4e-gnaw--active-reports)))))

(add-hook 'gnaw-after-update-hook #'mu4e-gnaw--refresh-all-buffers)

(provide 'mu4e-gnaw)
;;; mu4e-gnaw.el ends here
