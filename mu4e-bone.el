;;; mu4e-bone.el --- Highlight BARK reports in mu4e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry
;;
;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: mail
;; URL: https://codeberg.org/bzg/mu4e-bone
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (bone "0.1"))

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
;; M-x mu4e-bone RET           -- search for open BARK reports and highlight them
;; M-x mu4e-bone-topic RET     -- same, filtered by topic
;; M-x mu4e-bone-highlight RET -- highlight matches in the current headers buffer
;; M-x mu4e-bone-clear RET     -- remove highlights
;; M-x bone-update RET         -- force update of the remote reports cache
;;
;; The following commands toggle bone's local marks (kept in
;; ~/.config/bone/state.edn so they are shared with the bone CLI):
;;
;; M-x mu4e-bone-mark-sticky RET -- toggle the sticky mark (keep visible)
;; M-x mu4e-bone-mark-skip RET -- toggle the skip mark (hide)
;;
;; The annotation gains a leading mark column: '*' = sticky, '_' = skip.
;;
;; mu4e-bone builds on the `bone' library for the shared data layer
;; (configuration, report sources, cache and state.edn); this file only
;; provides the mu4e presentation and commands.
;;
;;; Code:

(require 'bone)
(require 'cl-lib)
(require 'subr-x)
(require 'time-date)
(require 'mu4e)

(declare-function mu4e-message-at-point "mu4e-message")
(declare-function mu4e-message-field "mu4e-message")
(declare-function mu4e-headers-for-each "mu4e-headers")
(declare-function mu4e-headers-search "mu4e-headers")

(defgroup mu4e-bone nil
  "Highlight BARK reports in mu4e headers."
  :group 'mu4e)

(defface mu4e-bone-face
  '((((background light)) :background "#e8e8e8")
    (((background dark))  :background "#333333"))
  "Subtle highlight for BARK reports in mu4e headers."
  :group 'mu4e-bone)

(defface mu4e-bone-annotation-face
  '((t :inherit shadow))
  "Face for right-margin annotations."
  :group 'mu4e-bone)

(defvar mu4e-bone-votes-width 7
  "Fixed width for the votes column.")

(defvar mu4e-bone-deadline-width 5
  "Fixed width for the deadline column.")

(defvar mu4e-bone-expiry-width 5
  "Fixed width for the expiry column.")

;; --- Message-id helpers ---------------------------------------------------

(defun mu4e-bone--bare-mid (mid)
  "Strip surrounding angle brackets from MID."
  (let ((s (or mid "")))
    (if (and (string-prefix-p "<" s) (string-suffix-p ">" s))
        (substring s 1 -1)
      s)))

;; --- Annotation formatting ------------------------------------------------

(defun mu4e-bone--mark-prefix (entry)
  "Get mark char for state ENTRY."
  (let ((flag (cdr (assq :flag entry)))
        (skip (cdr (assq :skip-since entry))))
    (cond
     ((eq flag :sticky) "*")
     (skip "_")
     (t " "))))

(defun mu4e-bone--type-letter (type)
  "Get letter abbreviation for TYPE."
  (pcase type
    ("bug"          "B")
    ("patch"        "P")
    ("request"      "?")
    ("announcement" "A")
    ("release"      "R")
    ("change"       "C")
    (_              "·")))

(defun mu4e-bone--deadline-days (deadline)
  "Days until YYYY-MM-DD DEADLINE."
  (when deadline
    (let* ((dl (date-to-time (concat deadline " 00:00:00")))
           (diff (float-time (time-subtract dl (current-time)))))
      (ceiling (/ diff 86400.0)))))

(defun mu4e-bone--annotation (info &optional entry)
  "Build annotation string for report INFO and state ENTRY."
  (let* ((mark     (mu4e-bone--mark-prefix entry))
         (type     (mu4e-bone--type-letter (plist-get info :type)))
         (flags    (plist-get info :flags))
         (priority (plist-get info :priority))
         (votes    (plist-get info :votes))
         (deadline (plist-get info :deadline))
         (expiry   (plist-get info :expiry))
         (dl-days  (mu4e-bone--deadline-days deadline))
         (ex-days  (mu4e-bone--deadline-days expiry))
         (pri-str  (pcase priority (3 "A") (2 "B") (1 "C") (_ " ")))
         (dl-str   (if dl-days (format "D%+d" dl-days) ""))
         (dl-pad   (string-pad dl-str mu4e-bone-deadline-width))
         (ex-str   (if ex-days (format "E%+d" ex-days) ""))
         (ex-pad   (string-pad ex-str mu4e-bone-expiry-width))
         (votes-str (if votes (format "[%s]" votes) ""))
         (votes-pad (string-pad votes-str mu4e-bone-votes-width)))
    (concat mark " " type " " flags " " pri-str " " dl-pad ex-pad votes-pad)))

;; --- Query building -------------------------------------------------------

(defun mu4e-bone--build-query (reports)
  "Build query string for REPORTS."
  (mapconcat (lambda (r)
               (format "msgid:%s" (mu4e-bone--bare-mid (car r))))
             reports
             " OR "))

(defun mu4e-bone--build-mid-map (reports)
  "Build mapping from bare message-id to info for REPORTS."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (r reports)
      (puthash (mu4e-bone--bare-mid (car r)) (cdr r) ht))
    ht))

;; --- Overlay highlighting -------------------------------------------------

(defvar-local mu4e-bone--active-reports nil
  "Buffer-local cache of BARK reports for auto-rehighlighting.")

(defvar mu4e-bone--pending-reports nil
  "Global pending reports awaiting next search finish.")

(defun mu4e-bone--apply-overlays (reports)
  "Apply overlays for REPORTS in the current `mu4e-headers-mode' buffer."
  (remove-overlays (point-min) (point-max) 'mu4e-bone t)
  (when (derived-mode-p 'mu4e-headers-mode)
    (let ((id-map (mu4e-bone--build-mid-map reports))
          (state  (bone-read-state)))
      (mu4e-headers-for-each
       (lambda (msg)
         (when-let* ((raw  (mu4e-message-field msg :message-id))
                     (mid  (mu4e-bone--bare-mid raw))
                     (info (gethash mid id-map)))
           (let* ((entry   (cdr (assoc (bone-normalize-mid mid) state)))
                  (bol     (line-beginning-position))
                  (eol     (line-end-position))
                  (ann-str (mu4e-bone--annotation info entry))
                  (p3      (= 3 (plist-get info :priority)))
                  (face    (if p3 '(mu4e-bone-face bold) 'mu4e-bone-face))
                  (ov      (make-overlay bol eol)))
             (overlay-put ov 'face face)
             (overlay-put ov 'mu4e-bone t)
             (overlay-put ov 'before-string
                          (propertize (concat ann-str " ")
                                      'face 'mu4e-bone-annotation-face)))))))))

(defun mu4e-bone--rehighlight ()
  "Re-apply overlays on search update."
  (when mu4e-bone--active-reports
    (mu4e-bone--apply-overlays mu4e-bone--active-reports)))

(defun mu4e-bone--install-pending ()
  "Install pending reports as active cache in this buffer."
  (remove-hook 'mu4e-headers-found-hook #'mu4e-bone--install-pending)
  (when-let* ((reports mu4e-bone--pending-reports))
    (setq mu4e-bone--pending-reports nil)
    (when (derived-mode-p 'mu4e-headers-mode)
      (setq mu4e-bone--active-reports reports)
      (add-hook 'mu4e-headers-found-hook #'mu4e-bone--rehighlight nil t)
      (mu4e-bone--apply-overlays reports))))

(defun mu4e-bone--search-and-watch (reports label)
  "Search mu for REPORTS and install overlays once done.
LABEL annotates the status message."
  (setq mu4e-bone--pending-reports reports)
  (add-hook 'mu4e-headers-found-hook #'mu4e-bone--install-pending)
  (mu4e-headers-search (mu4e-bone--build-query reports))
  (message "Searching %d BARK reports%s." (length reports) label))

;; --- Interactive commands -------------------------------------------------

;;;###autoload
(defun mu4e-bone ()
  "Search mu4e for open BARK reports and highlight them."
  (interactive)
  (let ((reports (bone-reports)))
    (if (null reports)
        (message "No open BARK reports found.")
      (mu4e-bone--search-and-watch reports ""))))

;;;###autoload
(defun mu4e-bone-highlight ()
  "Highlight BARK reports in current headers buffer."
  (interactive)
  (unless (derived-mode-p 'mu4e-headers-mode)
    (user-error "Not in a mu4e-headers buffer"))
  (let ((reports (bone-reports)))
    (if (null reports)
        (message "No open BARK reports found.")
      (setq mu4e-bone--active-reports reports)
      (add-hook 'mu4e-headers-found-hook #'mu4e-bone--rehighlight nil t)
      (mu4e-bone--apply-overlays reports)
      (message "Highlighted %d BARK reports." (length reports)))))

(defun mu4e-bone--collect-topics (reports)
  "Sorted list of topics in REPORTS."
  (let ((topics nil))
    (dolist (r reports)
      (let ((topic (plist-get (cdr r) :topic)))
        (when topic
          (cl-pushnew topic topics :test #'equal))))
    (sort (copy-sequence topics) #'string<)))

(defun mu4e-bone--filter-by-topic (reports topic)
  "Return REPORTS matching TOPIC."
  (cl-remove-if-not (lambda (r) (equal (plist-get (cdr r) :topic) topic))
                    reports))

;;;###autoload
(defun mu4e-bone-topic ()
  "Search BARK reports filtered by topic."
  (interactive)
  (let* ((reports (bone-reports))
         (topics  (mu4e-bone--collect-topics reports)))
    (cond
     ((null reports) (message "No open BARK reports found."))
     ((null topics)  (message "No topics in any report."))
     (t
      (let* ((topic    (completing-read "BARK topic: " topics nil t))
             (filtered (and (not (string= topic ""))
                            (mu4e-bone--filter-by-topic reports topic))))
        (cond
         ((or (string= topic "") (null filtered))
          (message "No reports for topic \"%s\"." topic))
         (t
          (mu4e-bone--search-and-watch
           filtered (format " for topic \"%s\"" topic)))))))))

;;;###autoload
(defun mu4e-bone-clear ()
  "Remove overlays and disable re-highlighting."
  (interactive)
  (remove-overlays (point-min) (point-max) 'mu4e-bone t)
  (setq mu4e-bone--active-reports nil)
  (remove-hook 'mu4e-headers-found-hook #'mu4e-bone--rehighlight t))

;; --- Marking commands -----------------------------------------------------

(defun mu4e-bone--current-mid ()
  "Current header line's bare message-id, or nil."
  (when-let* ((msg (ignore-errors (mu4e-message-at-point)))
              (mid (mu4e-message-field msg :message-id)))
    (mu4e-bone--bare-mid mid)))

(defun mu4e-bone--info-for-mid (mid reports)
  "Return info plist for MID in REPORTS."
  (cdr (assoc (bone-normalize-mid mid) reports)))

(defun mu4e-bone--mark (action on-msg off-msg)
  "Toggle ACTION mark, showing ON-MSG or OFF-MSG."
  (let* ((reports (or mu4e-bone--active-reports (bone-reports)))
         (mid     (and reports (mu4e-bone--current-mid)))
         (info    (and mid (mu4e-bone--info-for-mid mid reports))))
    (cond
     ((null reports) (user-error "No BARK reports loaded"))
     ((null mid)     (user-error "No message-id on current line"))
     ((null info)    (user-error "Current message is not a BARK report: %s" mid))
     (t
      (let ((on (bone-toggle-mark (bone-normalize-mid mid) info action)))
        (mu4e-bone--rehighlight)
        (message "%s" (if on on-msg off-msg)))))))

;;;###autoload
(defun mu4e-bone-mark-sticky ()
  "Toggle the sticky mark (keep visible) for the current report."
  (interactive)
  (mu4e-bone--mark :sticky "Marked sticky" "Unmarked sticky"))

;;;###autoload
(defun mu4e-bone-mark-skip ()
  "Toggle the skip mark (hide) for the current report."
  (interactive)
  (mu4e-bone--mark :skip "Skipped" "Unskipped"))

;; --- Cache update hooks ----------------------------------------------------

(defun mu4e-bone--refresh-all-buffers ()
  "Refresh mu4e-bone overlays in all headers buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'mu4e-headers-mode)
                 mu4e-bone--active-reports)
        (mu4e-bone--apply-overlays mu4e-bone--active-reports)))))

(add-hook 'bone-after-update-hook #'mu4e-bone--refresh-all-buffers)

(provide 'mu4e-bone)
;;; mu4e-bone.el ends here
