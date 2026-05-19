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
(require 'mu4e)

(declare-function mu4e-message-at-point "mu4e-message")
(declare-function mu4e-message-field "mu4e-message")
(declare-function mu4e-headers-for-each "mu4e-headers")
(declare-function mu4e-headers-search "mu4e-headers")

(defvar mu4e-bone-config-file "~/.config/bone/config.edn"
  "Path to bone config.edn.
The file is an EDN map with at least these keys:
  :addresses  vector of email addresses belonging to the user
  :sources    vector of maps, each with a :url key pointing at a
              reports.json (local file:// URI or http(s) URL).")

(defvar mu4e-bone-state-file "~/.config/bone/state.edn"
  "Path to bone's local state file.
An EDN map keyed by RFC-2822 message-id (with angle brackets).
Each value is a map with keys :subject :type :author :created and
optionally :flag (:todo or :sticky) and :read-at (ISO-8601 string).
Shared with the bone CLI.")

(defface mu4e-bone-face
  '((((background light)) :background "#e8e8e8")
    (((background dark))  :background "#333333"))
  "Subtle highlight for BARK reports in mu4e headers."
  :group 'mu4e-bone)

(defface mu4e-bone-annotation-face
  '((t :inherit shadow))
  "Face for right-margin annotations (type, flags, priority, votes)."
  :group 'mu4e-bone)

(defconst mu4e-bone-minimum-bark-format "0.9.1"
  "Minimum supported BONE reports.json bark-format.")

(defvar mu4e-bone-votes-width 7
  "Fixed width for the votes column.")

(defvar mu4e-bone-deadline-width 5
  "Fixed width for the deadline column (e.g. \"D-2  \" or \"     \").")

(defvar mu4e-bone-expiry-width 5
  "Fixed width for the expiry column (e.g. \"E-2  \" or \"     \").")

;;; --- Config / sources loading ---------------------------------------------

(defun mu4e-bone--uri-to-path (uri)
  "Convert a file:// URI to a local path; pass other URIs through unchanged."
  (if (string-prefix-p "file://" uri)
      (url-unhex-string (substring uri 7))
    uri))

(defun mu4e-bone--read-edn-source-urls (text)
  "Return list of :url strings from the :sources vector in EDN TEXT."
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

(defun mu4e-bone--load-sources ()
  "Return list of reports.json paths/URLs from `mu4e-bone-config-file'."
  (let* ((file (expand-file-name mu4e-bone-config-file))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string))))
    (mapcar #'mu4e-bone--uri-to-path
            (mu4e-bone--read-edn-source-urls text))))

(defun mu4e-bone--http-url-p (source)
  "Return non-nil if SOURCE is an HTTP(S) URL."
  (string-match-p "\\`https?://" source))

(defun mu4e-bone--read-json (source)
  "Read JSON from SOURCE, a local path or HTTP(S) URL."
  (let ((json-object-type 'alist)
        (json-array-type 'list))
    (if (mu4e-bone--http-url-p source)
        (let ((buf (url-retrieve-synchronously source t)))
          (unless buf (error "mu4e-bone: failed to fetch %s" source))
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (unless (re-search-forward "\n\n" nil t)
                  (error "mu4e-bone: malformed HTTP response from %s" source))
                (json-read))
            (kill-buffer buf)))
      (json-read-file source))))

(defun mu4e-bone--extract-open-reports (source)
  "Extract report plists for open reports from SOURCE.
SOURCE may be a local file path or an HTTP(S) URL.
Each entry is (MESSAGE-ID . plist).  A report is open when its
status is >= 4."
  (let* ((data (mu4e-bone--read-json source))
         (fv (alist-get 'bark-format data))
         (reports (alist-get 'reports data))
         (result '()))
    (when (and fv (version< fv mu4e-bone-minimum-bark-format))
      (message "mu4e-bone: %s has bark-format %s, minimum supported is %s"
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
                                 (_ (if closed "R" "-"))))))
            (push (cons mid (list :type (or type "bug")
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
  "Collect open (message-id . plist) pairs from all sources."
  (mapcan #'mu4e-bone--extract-open-reports (mu4e-bone--load-sources)))

;;; --- Minimal EDN reader/writer for ~/.config/bone/state.edn ---------------

(defun mu4e-bone--edn-skip-ws ()
  (skip-chars-forward " \t\n\r,"))

(defun mu4e-bone--edn-read ()
  "Read one EDN value at point."
  (mu4e-bone--edn-skip-ws)
  (let ((c (char-after)))
    (cond
     ((null c)   (error "mu4e-bone EDN: unexpected EOF"))
     ((eq c ?\") (mu4e-bone--edn-read-string))
     ((eq c ?:)  (mu4e-bone--edn-read-keyword))
     ((eq c ?\{) (mu4e-bone--edn-read-map))
     ((eq c ?\[) (mu4e-bone--edn-read-vector))
     ((or (and (>= c ?0) (<= c ?9))
          (and (eq c ?-) (let ((d (char-after (1+ (point)))))
                           (and d (>= d ?0) (<= d ?9)))))
      (mu4e-bone--edn-read-number))
     (t (mu4e-bone--edn-read-symbol)))))

(defun mu4e-bone--edn-read-string ()
  (forward-char 1)
  (let ((chars nil))
    (while (not (eq (char-after) ?\"))
      (let ((c (char-after)))
        (cond
         ((null c) (error "mu4e-bone EDN: unterminated string"))
         ((eq c ?\\)
          (forward-char 1)
          (let ((esc (char-after)))
            (unless esc (error "mu4e-bone EDN: dangling backslash"))
            (push (pcase esc
                    (?n ?\n) (?t ?\t) (?r ?\r)
                    (?b ?\b) (?f ?\f)
                    (?\\ ?\\) (?\" ?\")
                    (?u (forward-char 1)
                        (let ((hex (buffer-substring-no-properties
                                    (point) (+ (point) 4))))
                          (forward-char 3)
                          (string-to-number hex 16)))
                    (_ esc))
                  chars))
          (forward-char 1))
         (t (push c chars) (forward-char 1)))))
    (forward-char 1)
    (apply #'string (nreverse chars))))

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
  "Serialize S as an EDN/Clojure string literal."
  (concat "\""
          (replace-regexp-in-string
           "[\\\\\"\n\t\r\b\f]"
           (lambda (m)
             (pcase (aref m 0)
               (?\n "\\n") (?\t "\\t") (?\r "\\r")
               (?\b "\\b") (?\f "\\f")
               (?\\ "\\\\") (?\" "\\\"")))
           s t t)
          "\""))

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
  "Format an inner state ENTRY (alist with keyword keys) as an EDN map."
  (if (null entry) "{}"
    (concat "{"
            (mapconcat (lambda (kv)
                         (concat (mu4e-bone--edn-write-value (car kv))
                                 " "
                                 (mu4e-bone--edn-write-value (cdr kv))))
                       entry ", ")
            "}")))

;;; --- State file I/O -------------------------------------------------------

(defun mu4e-bone--read-state ()
  "Return bone's state.edn as an alist of (mid . inner-alist), or nil."
  (let ((file (expand-file-name mu4e-bone-state-file)))
    (when (file-readable-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (mu4e-bone--edn-skip-ws)
            (when (eq (char-after) ?{)
              (mu4e-bone--edn-read-map)))
        (error
         (message "mu4e-bone: cannot parse %s: %s -- using empty state."
                  file (error-message-string err))
         nil)))))

(defun mu4e-bone--write-state (state)
  "Write STATE (alist of (mid . inner-alist)) to `mu4e-bone-state-file'."
  (let ((file (expand-file-name mu4e-bone-state-file)))
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

;;; --- State transitions (mirror bone's apply-transition) -------------------

(defun mu4e-bone--iso-now ()
  (format-time-string "%Y-%m-%dT%H:%M:%S.%6NZ" nil t))

(defun mu4e-bone--author-string (info)
  "Build \"Name <email>\" from INFO's :from-name and :from, or nil."
  (let ((n (plist-get info :from-name))
        (e (plist-get info :from)))
    (cond
     ((and n e (not (string-empty-p n))) (concat n " <" e ">"))
     (e e)
     (n n))))

(defun mu4e-bone--enrich-entry (existing info)
  "Refresh entry metadata from INFO plist.  Only sets fields present in INFO.
EXISTING is an alist (keyword keys).  Returns a new alist."
  (let ((entry (copy-alist existing)))
    (dolist (pair '((:subject . :subject)
                    (:type    . :type)
                    (:date    . :created)))
      (when-let* ((v (plist-get info (car pair))))
        (setf (alist-get (cdr pair) entry) v)))
    (when-let* ((author (mu4e-bone--author-string info)))
      (setf (alist-get :author entry) author))
    entry))

(defun mu4e-bone--state-put (state mid entry)
  "Set MID -> ENTRY in STATE, preserving order when MID is already present."
  (if (assoc mid state)
      (mapcar (lambda (kv) (if (equal (car kv) mid) (cons mid entry) kv))
              state)
    (append state (list (cons mid entry)))))

(defun mu4e-bone--state-delete (state mid)
  "Remove MID from STATE."
  (cl-remove mid state :key #'car :test #'equal))

(defun mu4e-bone--alist-dissoc (alist key)
  "Return a copy of ALIST without KEY."
  (assq-delete-all key (copy-alist alist)))

(defun mu4e-bone--alist-assoc (alist key value)
  "Return a copy of ALIST with KEY set to VALUE."
  (let ((e (copy-alist alist)))
    (setf (alist-get key e) value)
    e))

(defun mu4e-bone--apply-transition (state action mid info)
  "Toggle ACTION (:read, :todo, :sticky) for MID in STATE.
INFO is the report's plist (used to enrich the entry on first touch)."
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

;;; --- Annotation formatting ------------------------------------------------

(defun mu4e-bone--mark-prefix (entry)
  "Return a single-character mark for state ENTRY."
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
  "Ensure MID is bracketed, for state.edn key compatibility with bone."
  (let ((bare (mu4e-bone--normalize-mid mid)))
    (concat "<" bare ">")))

(defun mu4e-bone--type-letter (type)
  "Return a single-letter abbreviation for report TYPE."
  (pcase type
    ("bug"          "B")
    ("patch"        "P")
    ("request"      "?")
    ("announcement" "A")
    ("release"      "R")
    ("change"       "C")
    (_              "·")))

(defun mu4e-bone--deadline-days (deadline)
  "Return days until DEADLINE (a \"YYYY-MM-DD\" string), or nil."
  (when deadline
    (let* ((dl (date-to-time (concat deadline " 00:00:00")))
           (diff (float-time (time-subtract dl (current-time)))))
      (ceiling (/ diff 86400.0)))))

(defun mu4e-bone--annotation (info &optional entry)
  "Build a fixed-width annotation string from report INFO plist.
When non-nil, ENTRY is the state.edn alist for this report."
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

;;; --- Query building -------------------------------------------------------

(defun mu4e-bone--build-query (reports)
  "Build a mu4e query matching all message-ids in REPORTS.
REPORTS is a list of (message-id . plist).  mu4e/mu uses bare
message-ids, so brackets are stripped.  Message-ids containing
Xapian-special characters (spaces, parens, AND/OR/NOT as bare
tokens) are passed through verbatim and may break the query."
  (mapconcat (lambda (r)
               (format "msgid:%s" (mu4e-bone--normalize-mid (car r))))
             reports
             " OR "))

(defun mu4e-bone--build-mid-map (reports)
  "Build a hash-table from REPORTS keyed by bare message-id, value is the plist."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (r reports)
      (puthash (mu4e-bone--normalize-mid (car r)) (cdr r) ht))
    ht))

;;; --- Overlay highlighting -------------------------------------------------

(defvar-local mu4e-bone--active-reports nil
  "Buffer-local cache of BARK reports for auto-rehighlighting.")

(defvar mu4e-bone--pending-reports nil
  "Global pending reports awaiting the next `mu4e-headers-found-hook' fire.
Consumed by `mu4e-bone--install-pending'.")

(defun mu4e-bone--apply-overlays (reports)
  "Apply overlays for REPORTS in the current `mu4e-headers-mode' buffer.
Annotation is prepended via `before-string' so it doesn't clobber
mu4e's header columns."
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
  "Re-apply BARK overlays in the current buffer.
Intended for the buffer-local `mu4e-headers-found-hook'."
  (when mu4e-bone--active-reports
    (remove-overlays (point-min) (point-max) 'mu4e-bone t)
    (mu4e-bone--apply-overlays mu4e-bone--active-reports)))

(defun mu4e-bone--install-pending ()
  "Install `mu4e-bone--pending-reports' as the active cache in this buffer.
Runs once on the next `mu4e-headers-found-hook' fire, then detaches."
  (remove-hook 'mu4e-headers-found-hook #'mu4e-bone--install-pending)
  (when-let ((reports mu4e-bone--pending-reports))
    (setq mu4e-bone--pending-reports nil)
    (when (derived-mode-p 'mu4e-headers-mode)
      (setq mu4e-bone--active-reports reports)
      (add-hook 'mu4e-headers-found-hook #'mu4e-bone--rehighlight nil t)
      (mu4e-bone--apply-overlays reports))))

(defun mu4e-bone--search-and-watch (reports label)
  "Search mu for REPORTS' message-ids and install overlays once results land.
LABEL is appended to the user-facing message (e.g. \" for topic X\")."
  (setq mu4e-bone--pending-reports reports)
  (add-hook 'mu4e-headers-found-hook #'mu4e-bone--install-pending)
  (mu4e-headers-search (mu4e-bone--build-query reports))
  (message "Searching %d BARK reports%s." (length reports) label))

;;; --- Interactive commands -------------------------------------------------

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
  "Highlight lines in the current mu4e headers buffer that match BARK reports."
  (interactive)
  (unless (derived-mode-p 'mu4e-headers-mode)
    (user-error "Not in a mu4e-headers buffer"))
  (let ((reports (mu4e-bone--load-all-open-reports)))
    (if (null reports)
        (message "No open BARK reports found.")
      (remove-overlays (point-min) (point-max) 'mu4e-bone t)
      (setq mu4e-bone--active-reports reports)
      (add-hook 'mu4e-headers-found-hook #'mu4e-bone--rehighlight nil t)
      (mu4e-bone--apply-overlays reports)
      (message "Highlighted %d BARK reports." (length reports)))))

(defun mu4e-bone--collect-topics (reports)
  "Return sorted list of unique topics from REPORTS."
  (let ((topics nil))
    (dolist (r reports)
      (when-let* ((topic (plist-get (cdr r) :topic)))
        (cl-pushnew topic topics :test #'equal)))
    (sort (copy-sequence topics) #'string<)))

(defun mu4e-bone--filter-by-topic (reports topic)
  "Return REPORTS whose :topic equals TOPIC."
  (cl-remove-if-not (lambda (r) (equal (plist-get (cdr r) :topic) topic))
                     reports))

;;;###autoload
(defun mu4e-bone-topic ()
  "Like `mu4e-bone', but limited to a single topic."
  (interactive)
  (let* ((reports (mu4e-bone--load-all-open-reports))
         (topics  (mu4e-bone--collect-topics reports)))
    (cond
     ((null reports) (message "No open BARK reports found."))
     ((null topics)  (message "No topics in any report."))
     (t
      (let* ((topic    (completing-read "BARK topic: " topics nil t))
             (filtered (and (not (string-empty-p topic))
                            (mu4e-bone--filter-by-topic reports topic))))
        (cond
         ((or (string-empty-p topic) (null filtered))
          (message "No reports for topic \"%s\"." topic))
         (t
          (mu4e-bone--search-and-watch
           filtered (format " for topic \"%s\"" topic)))))))))

;;;###autoload
(defun mu4e-bone-clear ()
  "Remove all mu4e-bone overlays and disable auto-rehighlighting."
  (interactive)
  (remove-overlays (point-min) (point-max) 'mu4e-bone t)
  (setq mu4e-bone--active-reports nil)
  (remove-hook 'mu4e-headers-found-hook #'mu4e-bone--rehighlight t))

;;; --- Marking commands -----------------------------------------------------

(defun mu4e-bone--current-mid ()
  "Return current header line's bare message-id, or nil."
  (when-let* ((msg (ignore-errors (mu4e-message-at-point)))
              (mid (mu4e-message-field msg :message-id)))
    (mu4e-bone--normalize-mid mid)))

(defun mu4e-bone--info-for-mid (mid reports)
  "Return the info plist for bare MID in REPORTS, or nil."
  (catch 'found
    (dolist (r reports)
      (when (equal (mu4e-bone--normalize-mid (car r)) mid)
        (throw 'found (cdr r))))))

(defun mu4e-bone--action-on-p (state bracketed-mid action)
  "Return non-nil when ACTION is set for BRACKETED-MID in STATE."
  (let ((entry (cdr (assoc bracketed-mid state))))
    (pcase action
      (:read   (cdr (assq :read-at entry)))
      (:todo   (eq (cdr (assq :flag entry)) :todo))
      (:sticky (eq (cdr (assq :flag entry)) :sticky)))))

(defun mu4e-bone--mark (action on-msg off-msg)
  "Toggle ACTION on the current message.  Show ON-MSG or OFF-MSG when done."
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
  "Toggle the :read-at timestamp for the current BARK report."
  (interactive)
  (mu4e-bone--mark :read "Marked read" "Unmarked read"))

;;;###autoload
(defun mu4e-bone-mark-todo ()
  "Toggle the :todo flag for the current BARK report."
  (interactive)
  (mu4e-bone--mark :todo "Marked TODO" "Unmarked TODO"))

;;;###autoload
(defun mu4e-bone-mark-sticky ()
  "Toggle the :sticky flag for the current BARK report."
  (interactive)
  (mu4e-bone--mark :sticky "Marked STICKY" "Unmarked STICKY"))

(provide 'mu4e-bone)
;;; mu4e-bone.el ends here
