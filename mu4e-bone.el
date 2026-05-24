;;; mu4e-bone.el --- highlight BARK reports in mu4e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry
;;
;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: mail
;; URL: https://codeberg.org/bzg/mu4e-bone

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
;; M-x mu4e-bone-update-cache RET -- force update of remote reports
;;
;; The following commands toggle bone's local marks (kept in
;; ~/.config/bone/state.edn so they are shared with the bone CLI):
;;
;; M-x mu4e-bone-mark-read   RET -- toggle :read-at on current message
;; M-x mu4e-bone-mark-todo   RET -- toggle :todo flag on current message
;; M-x mu4e-bone-mark-sticky RET -- toggle :sticky flag on current message
;;
;; The annotation gains a leading mark column: '!' = :todo, '*' = :sticky,
;; 'r' = :read-at (without flag).
;;
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'subr-x)
(require 'time-date)
(require 'mu4e)

(declare-function mu4e-message-at-point "mu4e-message")
(declare-function mu4e-message-field "mu4e-message")
(declare-function mu4e-headers-for-each "mu4e-headers")
(declare-function mu4e-headers-search "mu4e-headers")

(defvar url-http-response-status)

(defgroup mu4e-bone nil
  "Highlight BARK reports in mu4e headers."
  :group 'mu4e)

(defcustom mu4e-bone-reports-source nil
  "Path or URL to a BARK reports.json file.
If nil, load sources configured in config.edn under `mu4e-bone-config-dir'."
  :type '(choice (const :tag "Use config.edn sources" nil)
                 (string :tag "Local path or URL"))
  :group 'mu4e-bone)

(defcustom mu4e-bone-config-dir "~/.config/bone"
  "Directory containing bone configuration and state/cache files."
  :type 'directory
  :group 'mu4e-bone)

(defface mu4e-bone-face
  '((((background light)) :background "#e8e8e8")
    (((background dark))  :background "#333333"))
  "Subtle highlight for BARK reports in mu4e headers."
  :group 'mu4e-bone)

(defface mu4e-bone-annotation-face
  '((t :inherit shadow))
  "Face for right-margin annotations."
  :group 'mu4e-bone)

(defconst mu4e-bone-minimum-bark-format "0.9.1"
  "Minimum supported BONE reports.json bark-format.")

(defvar mu4e-bone-votes-width 7
  "Fixed width for the votes column.")

(defvar mu4e-bone-deadline-width 5
  "Fixed width for the deadline column.")

(defvar mu4e-bone-expiry-width 5
  "Fixed width for the expiry column.")

;;; --- Config / sources loading ---------------------------------------------

(defun mu4e-bone--uri-to-path (uri)
  "Convert file:// URI to local path, otherwise return URI."
  (if (string-prefix-p "file://" uri)
      (url-unhex-string (substring uri 7))
    uri))

(defun mu4e-bone--read-edn-source-urls (text)
  "Extract list of :url strings from :sources in EDN TEXT."
  (when (string-match
         ":sources[[:space:]]*\\[\\(\\(?:[^][]\\|\\[[^][]*\\]\\)*\\)\\]"
         text)
    (let ((body (match-string 1 text))
          (pos 0)
          (acc nil))
      (while (string-match ":url[[:space:]]*\"\\([^\"]+\\)\"" body pos)
        (push (match-string 1 body) acc)
        (setq pos (match-end 0)))
      (nreverse acc))))

(defun mu4e-bone--load-config ()
  "Load config file and return SOURCE-URIS list."
  (let ((file (expand-file-name "config.edn" mu4e-bone-config-dir)))
    (unless (file-readable-p file)
      (error "mu4e-bone: cannot read config %s" file))
    (let* ((text (with-temp-buffer
                   (insert-file-contents file)
                   (goto-char (point-min))
                   (while (re-search-forward "^[ \t]*;.*$" nil t)
                     (replace-match ""))
                   (buffer-string))))
      (mapcar #'mu4e-bone--uri-to-path
              (mu4e-bone--read-edn-source-urls text)))))

(defun mu4e-bone--load-sources ()
  "Return list of reports.json paths or URLs."
  (if mu4e-bone-reports-source
      (list (mu4e-bone--uri-to-path mu4e-bone-reports-source))
    (mu4e-bone--load-config)))

(defun mu4e-bone--http-url-p (source)
  "Return non-nil if SOURCE is an HTTP(S) URL."
  (string-match-p "\\`https?://" source))

(defun mu4e-bone--java-hash (str)
  "Calculate Java String hashCode of STR as an unsigned 32-bit integer."
  (let ((h 0)
        (len (length str)))
    (dotimes (i len)
      (setq h (logand (+ (* h 31) (aref str i)) #xffffffff)))
    h))

(defun mu4e-bone--source-to-cache-file (src)
  "Return cache file path for remote source SRC."
  (let* ((h (format "%08x" (mu4e-bone--java-hash src)))
         (safe (replace-regexp-in-string "[^a-zA-Z0-9._-]" "_" src))
         (prefix (substring safe 0 (min 80 (length safe)))))
    (expand-file-name
     (concat "cache/reports/" prefix "-" h ".json")
     mu4e-bone-config-dir)))

(defun mu4e-bone--fetch-json-from-url (url)
  "Synchronously fetch JSON from URL."
  (let ((buf (url-retrieve-synchronously url t)))
    (unless buf (error "mu4e-bone: failed to fetch %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (when (and (bound-and-true-p url-http-response-status)
                     (>= url-http-response-status 400))
            (error "mu4e-bone: HTTP error %d from %s" url-http-response-status url))
          (unless (re-search-forward "\r?\n\r?\n" nil t)
            (error "mu4e-bone: malformed HTTP response from %s" url))
          (let ((json-object-type 'alist)
                (json-array-type 'list))
            (json-read)))
      (kill-buffer buf))))

(defun mu4e-bone--write-json-to-file (data file)
  "Write JSON DATA to FILE."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert (json-encode data))))

(defun mu4e-bone--read-json (source)
  "Read JSON from SOURCE, using local cache for remote URLs if available."
  (let ((json-object-type 'alist)
        (json-array-type 'list))
    (if (mu4e-bone--http-url-p source)
        (let ((cache-file (mu4e-bone--source-to-cache-file source)))
          (if (file-exists-p cache-file)
              (json-read-file cache-file)
            (let ((data (mu4e-bone--fetch-json-from-url source)))
              (mu4e-bone--write-json-to-file data cache-file)
              data)))
      (json-read-file source))))

(defun mu4e-bone--extract-open-reports (source)
  "Extract open reports from SOURCE."
  (let* ((data (mu4e-bone--read-json source))
         (fv (alist-get 'bark-format data))
         (reports (alist-get 'reports data))
         (result '()))
    (when (and fv (version< fv mu4e-bone-minimum-bark-format))
      (message "mu4e-bone: %s has format %s, min supported is %s"
               source fv mu4e-bone-minimum-bark-format))
    (dolist (r reports result)
      (let ((mid          (alist-get 'message-id r))
            (status       (alist-get 'status r))
            (type         (alist-get 'type r))
            (acked        (alist-get 'acked r))
            (owned        (alist-get 'owned r))
            (closed       (alist-get 'closed r))
            (close-reason (alist-get 'close-reason r))
            (priority     (alist-get 'priority r))
            (votes        (alist-get 'votes r))
            (deadline     (alist-get 'deadline r))
            (expiry       (alist-get 'expiry r))
            (topic        (alist-get 'topic r))
            (subject      (alist-get 'subject r))
            (from         (alist-get 'from r))
            (from-name    (alist-get 'from-name r))
            (date         (alist-get 'date r)))
        (when (and mid (numberp status) (>= status 4))
          (let ((flags (concat (if acked "A" "-")
                               (if owned "O" "-")
                               (pcase close-reason
                                 ("canceled"   "C")
                                 ("resolved"   "R")
                                 ("expired"    "E")
                                 ("superseded" "S")
                                 (_ (if closed "R" "-")))))
                (norm-mid (mu4e-bone--bracketed-mid mid)))
            (push (cons norm-mid (list :type (or type "bug")
                                       :flags flags
                                       :priority (or priority 0)
                                       :votes votes
                                       :deadline deadline
                                       :expiry expiry
                                       :topic topic
                                       :subject subject
                                       :from from
                                       :from-name from-name
                                       :date date))
                  result)))))))

(defun mu4e-bone--load-all-open-reports ()
  "Collect open report pairs from all sources, tolerating failures."
  (let ((result nil))
    (dolist (source (mu4e-bone--load-sources))
      (condition-case err
          (setq result (append result (mu4e-bone--extract-open-reports source)))
        (error
         (message "mu4e-bone: failed loading source %s: %s"
                  source (error-message-string err)))))
    result))

(defun mu4e-bone-update-cache ()
  "Force-refresh the local cache from remote JSON sources."
  (interactive)
  (let ((sources (mu4e-bone--load-sources))
        (count 0))
    (dolist (source sources)
      (when (mu4e-bone--http-url-p source)
        (message "mu4e-bone: updating cache for %s..." source)
        (condition-case err
            (let ((data (mu4e-bone--fetch-json-from-url source))
                  (cache-file (mu4e-bone--source-to-cache-file source)))
              (mu4e-bone--write-json-to-file data cache-file)
              (setq count (1+ count))
              (message "mu4e-bone: cache updated for %s" source))
          (error
           (message "mu4e-bone: failed updating %s: %s"
                    source (error-message-string err))))))
    (message "mu4e-bone: cache update finished (%d updated)." count)))

;; --- EDN reader/writer for ~/.config/bone/state.edn -----------------------

(defun mu4e-bone--edn-skip-ws ()
  (skip-chars-forward " \t\n\r,"))

(defun mu4e-bone--edn-read ()
  "Read one EDN value at point."
  (mu4e-bone--edn-skip-ws)
  (let ((c (char-after)))
    (cond
     ((null c)   (error "mu4e-bone EDN: unexpected EOF"))
     ((eq c ?\") (read (current-buffer)))
     ((eq c ?:)  (mu4e-bone--edn-read-keyword))
     ((eq c ?\{) (mu4e-bone--edn-read-map))
     ((eq c ?\[) (mu4e-bone--edn-read-vector))
     ((or (and (>= c ?0) (<= c ?9))
          (and (eq c ?-) (let ((d (char-after (1+ (point)))))
                            (and d (>= d ?0) (<= d ?9)))))
      (mu4e-bone--edn-read-number))
     (t (mu4e-bone--edn-read-symbol)))))

(defun mu4e-bone--edn-read-keyword ()
  (forward-char 1)
  (let ((start (1- (point))))
    (skip-chars-forward "a-zA-Z0-9._/?!+*<>=&%$-")
    (intern (buffer-substring-no-properties start (point)))))

(defun mu4e-bone--edn-read-symbol ()
  (let ((start (point)))
    (skip-chars-forward "a-zA-Z0-9._/?!+*<>=&%$-")
    (pcase (buffer-substring-no-properties start (point))
      ("nil"   nil)
      ("true"  t)
      ("false" nil)
      (s       (intern s)))))

(defun mu4e-bone--edn-read-number ()
  (let ((start (point)))
    (skip-chars-forward "0-9.eE+-")
    (string-to-number (buffer-substring-no-properties start (point)))))

(defun mu4e-bone--edn-read-map ()
  (forward-char 1)
  (let ((acc nil))
    (mu4e-bone--edn-skip-ws)
    (while (not (eq (char-after) ?\}))
      (let ((k (mu4e-bone--edn-read)))
        (mu4e-bone--edn-skip-ws)
        (push (cons k (mu4e-bone--edn-read)) acc))
      (mu4e-bone--edn-skip-ws))
    (forward-char 1)
    (nreverse acc)))

(defun mu4e-bone--edn-read-vector ()
  (forward-char 1)
  (let ((acc nil))
    (mu4e-bone--edn-skip-ws)
    (while (not (eq (char-after) ?\]))
      (push (mu4e-bone--edn-read) acc)
      (mu4e-bone--edn-skip-ws))
    (forward-char 1)
    (nreverse acc)))

(defun mu4e-bone--edn-write-string (s)
  "Format string S as an EDN string."
  (format "%S" s))

(defun mu4e-bone--edn-write-value (v)
  (cond
   ((stringp v)  (mu4e-bone--edn-write-string v))
   ((keywordp v) (symbol-name v))
   ((eq v t)     "true")
   ((null v)     "nil")
   ((numberp v)  (number-to-string v))
   ((consp v)    (mu4e-bone--edn-write-entry v))
   (t (error "mu4e-bone EDN: cannot serialize %S" v))))

(defun mu4e-bone--edn-write-entry (entry)
  "Format entry as an EDN map."
  (if (null entry) "{}"
    (concat "{"
            (mapconcat (lambda (kv)
                         (concat (mu4e-bone--edn-write-value (car kv))
                                 " "
                                 (mu4e-bone--edn-write-value (cdr kv))))
                       entry ", ")
            "}")))

;; --- State file I/O -------------------------------------------------------

(defun mu4e-bone--read-state ()
  "Read state file."
  (let ((file (expand-file-name "state.edn" mu4e-bone-config-dir)))
    (when (file-readable-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (mu4e-bone--edn-skip-ws)
            (when (eq (char-after) ?{)
              (mu4e-bone--edn-read-map)))
        (error
         (message "mu4e-bone: cannot parse %s: %s"
                  file (error-message-string err))
         nil)))))

(defun mu4e-bone--write-state (state)
  "Write STATE to state file."
  (let ((file (expand-file-name "state.edn" mu4e-bone-config-dir)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (if (null state)
          (insert "{}\n")
        (insert "{")
        (let ((first t))
          (dolist (kv state)
            (if first (setq first nil) (insert "\n "))
            (insert (mu4e-bone--edn-write-string (car kv)))
            (insert " ")
            (insert (mu4e-bone--edn-write-entry (cdr kv)))))
        (insert "}\n")))))

;; --- State transitions ----------------------------------------------------

(defun mu4e-bone--iso-now ()
  (format-time-string "%Y-%m-%dT%H:%M:%S.%6NZ" nil t))

(defun mu4e-bone--author-string (info)
  "Build author string from INFO."
  (let ((n (plist-get info :from-name))
        (e (plist-get info :from)))
    (cond
     ((and n e (not (string= n ""))) (concat n " <" e ">"))
     (e e)
     (n n))))

(defun mu4e-bone--alist-dissoc (alist key)
  "Remove KEY from ALIST copy."
  (assq-delete-all key (copy-alist alist)))

(defun mu4e-bone--alist-assoc (alist key value)
  "Set KEY to VALUE in ALIST copy."
  (let ((e (copy-alist alist)))
    (setf (alist-get key e) value)
    e))

(defun mu4e-bone--enrich-entry (existing info)
  "Refresh metadata from INFO in EXISTING."
  (let ((entry (copy-alist existing)))
    (dolist (pair '((:subject . :subject)
                    (:type    . :type)
                    (:date    . :created)))
      (let ((v (plist-get info (car pair))))
        (when v
          (setf (alist-get (cdr pair) entry) v))))
    (let ((author (mu4e-bone--author-string info)))
      (when author
        (setf (alist-get :author entry) author)))
    entry))

(defun mu4e-bone--state-put (state mid entry)
  "Set MID to ENTRY in STATE, keeping order."
  (if (assoc mid state)
      (mapcar (lambda (kv) (if (equal (car kv) mid) (cons mid entry) kv))
              state)
    (append state (list (cons mid entry)))))

(defun mu4e-bone--state-delete (state mid)
  "Remove MID from STATE."
  (cl-remove mid state :key #'car :test #'equal))

(defun mu4e-bone--apply-transition (state action mid info)
  "Apply ACTION transition for MID in STATE."
  (let* ((base (mu4e-bone--enrich-entry (cdr (assoc mid state)) info))
         (flag (alist-get :flag base))
         (new
          (pcase action
            (:read   (if (alist-get :read-at base)
                         (mu4e-bone--alist-dissoc base :read-at)
                       (mu4e-bone--alist-assoc  base :read-at
                                                 (mu4e-bone--iso-now))))
            (:todo   (if (eq flag :todo)
                         (mu4e-bone--alist-dissoc base :flag)
                       (mu4e-bone--alist-assoc  base :flag :todo)))
            (:sticky (if (eq flag :sticky)
                         (mu4e-bone--alist-dissoc base :flag)
                       (mu4e-bone--alist-assoc  base :flag :sticky))))))
    (if (and (null (alist-get :flag    new))
             (null (alist-get :read-at new)))
        (mu4e-bone--state-delete state mid)
      (mu4e-bone--state-put state mid new))))

;; --- Annotation formatting ------------------------------------------------

(defun mu4e-bone--mark-prefix (entry)
  "Get mark char for state ENTRY."
  (let ((flag (cdr (assq :flag entry)))
        (read (cdr (assq :read-at entry))))
    (cond
     ((eq flag :todo)   "!")
     ((eq flag :sticky) "*")
     (read              "r")
     (t                 " "))))

(defun mu4e-bone--normalize-mid (mid)
  "Strip surrounding angle brackets from MID."
  (let ((s (or mid "")))
    (if (and (string-prefix-p "<" s) (string-suffix-p ">" s))
        (substring s 1 -1)
      s)))

(defun mu4e-bone--bracketed-mid (mid)
  "Ensure MID is bracketed."
  (let ((bare (mu4e-bone--normalize-mid mid)))
    (concat "<" bare ">")))

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
               (format "msgid:%s" (mu4e-bone--normalize-mid (car r))))
             reports
             " OR "))

(defun mu4e-bone--build-mid-map (reports)
  "Build mapping from bare message-id to info."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (r reports)
      (puthash (mu4e-bone--normalize-mid (car r)) (cdr r) ht))
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
          (state  (mu4e-bone--read-state)))
      (mu4e-headers-for-each
       (lambda (msg)
         (when-let* ((raw  (mu4e-message-field msg :message-id))
                     (mid  (mu4e-bone--normalize-mid raw))
                     (info (gethash mid id-map)))
           (let* ((entry   (cdr (assoc (mu4e-bone--bracketed-mid mid) state)))
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
  "Search mu for REPORTS and install overlays once done."
  (setq mu4e-bone--pending-reports reports)
  (add-hook 'mu4e-headers-found-hook #'mu4e-bone--install-pending)
  (mu4e-headers-search (mu4e-bone--build-query reports))
  (message "Searching %d BARK reports%s." (length reports) label))

;; --- Interactive commands -------------------------------------------------

;;;###autoload
(defun mu4e-bone ()
  "Search mu4e for open BARK reports and highlight them."
  (interactive)
  (let ((reports (mu4e-bone--load-all-open-reports)))
    (if (null reports)
        (message "No open BARK reports found.")
      (mu4e-bone--search-and-watch reports ""))))

;;;###autoload
(defun mu4e-bone-highlight ()
  "Highlight BARK reports in current headers buffer."
  (interactive)
  (unless (derived-mode-p 'mu4e-headers-mode)
    (user-error "Not in a mu4e-headers buffer"))
  (let ((reports (mu4e-bone--load-all-open-reports)))
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
  "Reports matching TOPIC."
  (cl-remove-if-not (lambda (r) (equal (plist-get (cdr r) :topic) topic))
                    reports))

;;;###autoload
(defun mu4e-bone-topic ()
  "Search BARK reports filtered by topic."
  (interactive)
  (let* ((reports (mu4e-bone--load-all-open-reports))
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
    (mu4e-bone--normalize-mid mid)))

(defun mu4e-bone--info-for-mid (mid reports)
  "Return info plist for MID in REPORTS."
  (cdr (assoc (mu4e-bone--bracketed-mid mid) reports)))

(defun mu4e-bone--action-on-p (state bracketed-mid action)
  "Check if ACTION is set for BRACKETED-MID in STATE."
  (let ((entry (cdr (assoc bracketed-mid state))))
    (pcase action
      (:read   (cdr (assq :read-at entry)))
      (:todo   (eq (cdr (assq :flag entry)) :todo))
      (:sticky (eq (cdr (assq :flag entry)) :sticky)))))

(defun mu4e-bone--mark (action on-msg off-msg)
  "Toggle ACTION mark, showing ON-MSG or OFF-MSG."
  (let* ((reports (or mu4e-bone--active-reports
                      (mu4e-bone--load-all-open-reports)))
         (mid     (and reports (mu4e-bone--current-mid)))
         (info    (and mid (mu4e-bone--info-for-mid mid reports))))
    (cond
     ((null reports) (user-error "No BARK reports loaded"))
     ((null mid)     (user-error "No message-id on current line"))
     ((null info)    (user-error "Current message is not a BARK report: %s" mid))
     (t
      (let* ((state (mu4e-bone--read-state))
             (bmid  (mu4e-bone--bracketed-mid mid))
             (new   (mu4e-bone--apply-transition state action bmid info)))
        (mu4e-bone--write-state new)
        (mu4e-bone--rehighlight)
        (message "%s" (if (mu4e-bone--action-on-p new bmid action)
                          on-msg off-msg)))))))

;;;###autoload
(defun mu4e-bone-mark-read ()
  "Toggle :read-at timestamp for current report."
  (interactive)
  (mu4e-bone--mark :read "Marked read" "Unmarked read"))

;;;###autoload
(defun mu4e-bone-mark-todo ()
  "Toggle :todo flag for current report."
  (interactive)
  (mu4e-bone--mark :todo "Marked TODO" "Unmarked TODO"))

;;;###autoload
(defun mu4e-bone-mark-sticky ()
  "Toggle :sticky flag for current report."
  (interactive)
  (mu4e-bone--mark :sticky "Marked STICKY" "Unmarked STICKY"))

(provide 'mu4e-bone)
;;; mu4e-bone.el ends here
