;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (c) 2011 Scott Vokes <vokes.s@gmail.com>
;; Copyright (c) 2015 Scott Locklin/Kerf Software
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; major-mode and a few utilities for working with kona (k).
;; Usage:
;;
;; (require 'kerf-mode)
;;
;; Bind run-kerf to something convenient, e.g.
;;   (global-set-key (kbd "C-c i k") 'run-kerf)
;; and use that to start a connected kerf session.
;;
;; Use kerf-send (C-c C-n) to send the region (if any) or current line
;; or kerf-send-buffer (C-c C-b) to send blocks of code to it.
;;
;; TODO
;; * smart indentation
;; * syntax-table
;; * custom stuff? (I don't use it...)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'comint)

(defvar kerf-program-name "kerf" "kerf executable name.")

(defvar kerf-prompt-string "KeRF> "
  "String printed by interpreter to represent a ready prompt.")

(defvar kerf-process nil "Current kerf comint process, if any.")

(defvar kerf-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-n") 'kerf-send)    ;eval region or line
    (define-key m (kbd "C-c C-r") 'kerf-send)    ;eval region or line
    (define-key m (kbd "C-c C-z") 'run-kerferf)
    (define-key m (kbd "C-c C-b") 'kerf-send-buffer)
    m)
  "Keymap for kerf mode.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface kerf-normal-face
    '((t (:inherit nil)))
  "Font lock for text with no special highlighting.")

(defface kerf-builtin-face
    '((t (:inherit font-lock-builtin-face)))
  "Font lock for builtins, such as add mmul inv.")

(defface kerf-number-face
    '((t (:inherit kerf-normal-face)))
  "Font lock for numbers.")

(defface kerf-variable-face
    '((t (:inherit kerf-normal-face)))
  "Font lock for variables.")

(defface kerf-variable-binding-face
    '((t (:inherit font-lock-variable-name-face)))
  "Font lock for variable bindings sites.")

(defface kerf-verb-face
    '((t (:inherit kerf-normal-face)))
  "Font lock for verbs.")

(defface kerf-string-face
    '((t (:inherit font-lock-string-face)))
  "Font lock for strings.")

(defface kerf-symbol-face
    '((t (:inherit font-lock-constant-face)))
  "Font lock for symbols.")

(defface kerf-date-face
    '((t (:inherit font-lock-constant-face)))
  "Font lock for dates.")

(defface kerf-time-face
    '((t (:inherit font-lock-constant-face)))
  "Font lock for times.")

(defface kerf-adverb-face
    '((t (:weight bold :inherit font-lock-keyword-face)))
  "Font lock for adverbs.")

(defface kerf-comment-delimeter-face
    '((t (:inherit font-lock-comment-face)))
  "Font lock for comment marker.")

(defface kerf-comment-face
    '((t (:inherit font-lock-comment-face)))
  "Font lock for comments.")

(defface kerf-brace-face
    '((t (:inherit font-lock-function-name-face)))
  "Font lock for {}s.")

(defface kerf-bracket-face
    '((t (:weight bold :inherit font-lock-normal-face)))
  "Font lock for []s.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME:
;; - doesn't get labeled a verb (matching numbers?)
;; 
(defun kerf-font-lock-keyword-maker ()
  '( ("^\\(//\\)\\([^\n]*\\)$" 
     (1 'kerf-comment-delimeter-face)
     (2 'kerf-comment-face))
    (" \\(//\\)\\([^\n]*\\)$" 
     (1 'kerf-comment-delimeter-face)
     (2 'kerf-comment-face))
    ("[\\][>|<|=|~|/\\]?" . 'kerf-adverb-face) 
    ("\\(fold\\|refold\\|mapdown\\|mapright\\|mapleft\\|mapback\\|reduce\\|rereduce\\|converge\\|reconverge\\|unfold\\|deconverge\\|mapcores\\)" . 'kerf-adverb-face)
    ("[0-9][0-9][0-9][0-9]\\.[01][0-9]\\.[0123][0-9]". 'kerf-date-face)
    ("[012][0-9]:[012345][0-9]\\(:[012345][0-9]\\(\\.[0-9]\\{0,3\\}\\)?\\)?". 
     'kerf-time-face)
    ("\"[^\"]*\"" . 'kerf-string-face)
    ("\'[^\']*\'" . 'kerf-string-face)
    ("[a-zA-Z][a-zA-Z0-9_]*:" . 'kerf-variable-binding-face)
    ("-?[0-9]+\\(\\.?[0-9]*\\)?\\([eE][+-]?[0-9]+\\)?" . 'kerf-number-face)
    ("[!#$%&*+,.;<=/>?@^|~-:]:?" . 'kerf-builtin-face)
("\\<\\(abs\\|acos\\|add\\|and\\|ascend\\|asin\\|asof_join\\|atan\\|atlas\\|atom\\|avg\\|between\\|btree\\|bucketed\\||car\\|cdr\\|ceil\\|char\\|close_socket\\|cross\\|combinations\\|cos\\|cosh\\|count\\|count_nonnull\\|count_null\\|create_table_from_csv\\|create_table_from_psv\\|create_table_from_tsv\\|deal\\|def\\|delete_keys\\|descend\\|dir_ls\\|display\\|distinct\\|divide\\|dlload\\|dotp\\|drop\\|enlist\\|enum\\|enumerate\\|equal\\|equals\\|erf\\|erfc\\|eval\\|except\\|exit\\|exp\\|explode\\|filter\\|first\\|flatten\\|float\\|floor\\|function\\|global\\|globals\\|greater\\|greatereq\\|has_column\\|hash\\|hashed\\|has_key\\|ident\\|ifnull\\|implode\\|in\\|index\\|indexed\\|int\\|intersect\\|isnull\\|join\\|json_from_kerf\\|kerf_from_json\\|kerf_type\\|kerf_type_name\\|KEY\\|last\\|left_join\\|len\\|less\\|lesseq\\|lg\\|lines\\|ln\\|load\\|log\\|lsq\\|map\\|match\\|mavg\\|max\\|maxes\\|mcount\\|median\\|meta_table\\|min\\|mins\\|minus\\|minv\\|mkdir\\|mmax\\|mmin\\|mmul\\|mod\\|msum\\|negate\\|negative\\|ngram\\|NONNULL\\|not\\|noteq\\|noteq\\|now\\|now_date\\|now_time\\|open_socket\\|open_table\\|or\\|order\\|out\\|part\\|permutations\\|plus\\|pow\\|PRIMARY\\|rand\\|range\\|read_from_path\\|read_striped_from_path\\|read_table_from_csv\\|read_table_from_delimited_file\\|read_table_from_fixed_file\\|read_table_from_tsv\\|rep\\|repeat\\|reserved\\|reverse\\|rsum\\|run\\|search\\|seed_prng\\|send_async\\|send_sync\\|setminus\\|shift\\|shuffle\\|sin\\|sinh\\|sleep\\|sort\\|sort_debug\\|split\\|sqrt\\|stamp_diff\\|std\\|string\\|subtract\\|sum\\|system\\|tables\\|take\\|tan\\|tanh\\|times\\|timing\\|tolower\\|toupper\\|transpose\\|trim\\|type_null\\|uneval\\|union\\|unique\\|UNIQUE\\|var\\|which\\|write_csv_from_table\\|write_delimited_file_from_table\\|write_striped_to_path\\|write_text\\|write_to_path\\|xbar\\|xkeys\\|xvals\\)\\>"  . 'kerf-builtin-face)
    ("[{}]" . 'kerf-brace-face)
    ("[][]" . 'kerf-bracket-face)
    ("[)(]" . 'kerf-bracket-face)))

 (setq kerf-font-lock-keywords (kerf-font-lock-keyword-maker))

(defvar kerf-font-lock-keywords
  (kerf-font-lock-keyword-maker)
   "Keyword highlighting specification for `kerf-mode'.")

(defvar kerf-mode-hook nil "Hooks called when starting kerf-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; major-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode kerf-mode nil "kerf"
  "Major mode for kerf code.

\\{kerf-mode-map}
"
  ; :syntax-table kerf-mode-syntax-table
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (use-local-map kerf-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(kerf-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.kerf$" . kerf-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kerf-proc ()
  "Get kerf process."
  (get-process kerf-program-name))

(defun kerf-proc-buffer ()
  "Get kerf process's buffer."
  (get-buffer (concat "*" kerf-program-name "*")))

(defun kerf-proc-kill ()
  "Kill the current kerf process, if any."
  (interactive)
  (let ((kp (kerf-proc)))
    (when kp (delete-process kp))))

(defun run-kerf (uarg)
  "Switch to a kerf process, or spawn a new one if not running.
Universal argument switches to it in another window."
;; TODO see if you can fix the formatting here, possibly with
;; (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
  (interactive "P")
  (let* ((kproc (or (kerf-proc)
                    (make-comint kerf-program-name kerf-program-name)))
         (kbuf (kerf-proc-buffer)))
    (when kbuf
      (with-current-buffer kbuf
        (add-hook 'comint-output-filter-functions
                  'kerf-comint-output-filter nil t))
      (unless (equal (current-buffer) kbuf)
        (if uarg
            (switch-to-buffer-other-window kbuf)
            (switch-to-buffer kbuf))))
  kproc))

(defun kerf-send-str (s)
  "Send string to the k process, if existing."
  (let ((kproc (or (kerf-proc) (run-kerf t)))
        (kbuf (kerf-proc-buffer)))
    (when (and kproc s)
      (comint-send-string kproc s))))
            
(defun kerf-buffer-is-visible ()
  "Check if the k process buffer is currently visible."
  (let ((b (kerf-proc-buffer)))
    (when b
      (member b
              (mapcar (lambda (w) (window-buffer w))
                      (window-list))))))

(defun kerf-comint-output-filter (s)
  "Print output from code sent to k in the minibuffer."
  (unless (kerf-buffer-is-visible)
    (let ((drop (min (length s)
                     (+ 1 (length kerf-prompt-string)))))
      (princ (substring s 0 (- drop))))))

(defun kerf-send-region (start end)
  "Send region to k process."
  (interactive "r")
  (let ((str (concat (buffer-substring start end) "\n")))
    (kerf-send-str str)))

(defun kerf-send-buffer ()
  "Send whole buffer to k process."
  (interactive)
  (kerf-send-region (point-min) (point-max)))

(defun kerf-send-line ()
  "Send current line to k process."
  (interactive)
  (save-excursion
    (let ((bol (progn (beginning-of-line) (point)))
          (eol (progn (end-of-line) (point))))
      (let ((str (concat (buffer-substring bol eol) "\n")))
        (kerf-send-str str)))))

(defun kerf-send ()
  "Send current line or region to k process."
  (interactive)
  (if mark-active
      (kerf-send-region (region-beginning) (region-end))
      (kerf-send-line)))


(provide 'kerf-mode)
