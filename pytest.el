;;; pytest --- stuff


;;; Commentary:


;;; Code:
(require 'python)
(require 'projectile)
(require 'transient)

(defcustom pytest-cmd "docker-compose run --rm app pytest"
  "The pytest executable that will be used by pytest mode."
  :group 'pytest-mode)

(defcustom pytest-test-dir ""
  "Explicitly set the test directory rather than relying on pytests discovery."
  :group 'pytest-mode)

(defcustom pytest-history-ring-size 20
  "Number of pytest invocations to save before overwritting last entries in history."
  :group  'pytest-mode)

;; for re-running previous pytest invocations we keep a history of per-projet
(defvar pytest-history (make-hash-table :test 'equal))

(defvar pytest-mode-map
  (let ((map (make-keymap "pytest-mode-map")))
    (define-key map (kbd "C-q") 'quit-window)
    (define-key map (kbd "C-t") 'pytest-runner)
    (define-key map (kbd "C-<return>") 'pytest-jump-to-failed)
    map))

(define-minor-mode pytest-mode
  "Pytest mode."
  :lighter pytest
  :keymap pytest-mode-map)

;;
;; -- Pytest transient
;;

(defclass pytest-extra-arg (transient-argument)
  ((reader :initarg :reader)
   (argument :initarg :argument :initform "")
   (value :initarg :value :initform "")))

(cl-defmethod transient-format-value ((obj pytest-extra-arg))
  (if-let ((value (oref obj value)))
      (propertize value 'face 'transient-value)
    (propertize "unset" 'face 'transient-inactive-value)))

(defun pytest-extras-reader (prompt _initial-input history)
  "Read extra flags to be passed to pytest from the user."
  (completing-read prompt '() nil nil _initial-input history))

(transient-define-argument pytest-extra-args ()
  :description "extra-flags"
  :shortarg "-e"
  :class 'pytest-extra-arg
  :reader #'pytest-extras-reader )

(transient-define-prefix pytest-runner ()
  "Pytest Runner Interface"
  ["Arguments"
   ("-v" "verbose" "-vv")
   ("-l" "no-sqla-logs" "--no-sqla-logs")
   (pytest-extra-args)]
  [["Test Current"
    :if pytest-is-test-file-p
    ("cb" "buffer" pytest-run-current-file)
    ("cf" "function" pytest-run-current-test)]
   ["Past Invocations"
    :if pytest-has-invocations-p
    ("p" "run previous" pytest-run-previous)]
   ["Failed Tests"
    :if pytest-has-failed-tests-p
    ("f" "run failed tests" pytest-run-failed)
    ("s" "run selection" pytest-run-failed-selection)]
   ["General"
    ("r" "run tests" pytest-run)
    ("b" "pytest buffer" pytest-open-buffer)]])

(defun pytest-is-test-file-p ()
  "Non-nil if current file has 'test_' prefix in name."
  (string-prefix-p "test_" (file-name-nondirectory (or (buffer-file-name) ""))))

(defun pytest-has-invocations-p ()
  "Non-nil if there are previous pytest invocations for proejct."
  (not (eq (gethash (projectile-project-name) pytest-history) nil)))

(defun pytest-has-failed-tests-p ()
  "Non-nil if buffer has failed test strings."
  (> (length (pytest-get-failed-in-string (buffer-string))) 0))

(defun pytest-open-buffer ()
  "Open the pytest buffer."
  (interactive)
  (let ((split-window-preferred-function 'pytest-window-split)
	(buffer (pytest-buffer-name)))
    (if (get-buffer buffer)
	(switch-to-buffer-other-window buffer)
      (message (concat "No pytest buffer for project " (projectile-project-name))))))

(defun transient-pytest-args ()
  "Transient args getter."
  (transient-args 'pytest-runner))

;;
;; -- Pytest Helpers
;;

(defun pytest-window-split (window)
  "Prefer horizontal split of WINDOW regardless of layout."
  (split-window (frame-root-window) (frame-root-window) 'below))

(defun pytest-buffer-name ()
  "Generate buffer name for test invocation."
  (format "*%s-test*" (projectile-project-name)))

(defun pytest-push-history (command)
  "Save the currently executed COMMAND for future history lookup."
  (let* ((project (projectile-project-name))
	 (history (gethash project pytest-history))
	 (command-string (mapconcat 'identity command " ")))
    (puthash project
	     (add-to-history 'history command-string pytest-history-ring-size)
	     pytest-history)))

(defun pytest-command-formatter (params)
  "Generate a list of command parts from PARAMS `term-ansi-make-term` for test process."
  (append (split-string-and-unquote pytest-cmd) params))

(defun pytest-interactive-runner (params)
  "Spawn the pytest process with the given PARAMS."
  (let* ((default-directory (projectile-project-root))
	 (split-window-preferred-function 'pytest-window-split)
	 (args (pytest-command-formatter params))
	 (cmd (car args))
         (switches (cdr args))
	 (process-name (pytest-buffer-name))
	 (*buffer* (get-buffer-create process-name)))
    (pytest-push-history params)
    ;; erase buffer if there is no process running and it has previous output
    (with-current-buffer *buffer*
      (when (and (not (term-check-proc *buffer*))
		 (> (buffer-size *buffer*) 0))
	(let ((inhibit-read-only t))
	  (erase-buffer)))
      ;; try spawn the process -- nop if process is already running
      (apply 'term-ansi-make-term process-name cmd nil switches)
      (term-char-mode)
      (pytest-mode))
    *buffer*))

(defun pytest-command-runner (params)
  "Spawn the pytest process with given PARAMS and open proces buffer."
  (let ((split-window-preferred-function 'pytest-window-split)
	(*buffer* (pytest-interactive-runner params)))
    (when (not (get-buffer-window *buffer*))
      (switch-to-buffer-other-window *buffer*))))

(defun pytest-parent-class ()
  "Find the first occurance of a python class above current point."
  (save-match-data
    (when (re-search-backward "^class \\(.*?\\):" nil t)
      (match-string-no-properties 1))))

(defun pytest-get-failed-in-string (string)
  "Parse the failed test(s) from the STRING and re-run just the test(s) extracted."
  (let ((pos 0)
	(matches)
	(params))
    (save-match-data
      (while (string-match "^\\(FAILED\\|ERROR\\) \\(.*?\\.py::.*?\\)\\( - \\|$\\)" string pos)
	(push (match-string 2 string) matches)
	(setq pos (match-end 2))))
    matches))

;;
;; -- Interactives
;;

(defun pytest-run-current-test (&optional flags)
  "Run the test above current point with optional FLAGS."
  (interactive (list (transient-pytest-args)))
  (save-excursion
    (mark-defun)
    (let ((region (buffer-substring (region-beginning) (region-end)))
	  (test-name (string-remove-prefix (projectile-project-root) buffer-file-name)))
      (deactivate-mark)
      (save-match-data
	(string-match "^\\(\s\\{4\\}\\)?def \\(test_.*?\\)(" region 0)
	(let ((test-func (match-string 2 region)))
	  ;; when function is indented, attempt to find parent test class
	  (when (match-string 1 region)
	    (setq test-name (concat test-name (format "::%s" (pytest-parent-class)))))
	  (if test-func
	      (setq test-name (concat test-name (format "::%s" test-func)))
	    (message "no test found"))
	  (pytest-command-runner
	   (append (split-string-and-unquote test-name) flags)))))))

(defun pytest-jump-to-failed ()
  "Jump the the failed test start section."
  (interactive)
  (let ((content (buffer-substring (line-beginning-position) (line-end-position))))
    (save-match-data
      (string-match "^\\(FAILED\\|ERROR\\)\s.*?.py::\\(.*?\\)\\(\s-\\|$\\)" content 0)
      (search-backward
       (format "__ %s __" (replace-regexp-in-string "::" "." (match-string 2 content)))))))

(defun pytest-run-failed (&optional flags)
  "Re-run failed test(s) in matched in the test runner buffer with provided FLAGS."
  (interactive (list (transient-pytest-args)))
  (pytest-command-runner (append (pytest-get-failed-in-string (buffer-string)) flags)))

(defun pytest-run-failed-selection (&optional flags)
  "Re-run fialed test(s) with provded FLAGS in marked-region or current line."
  (interactive (list (transient-pytest-args)))
  (let (start end)
    (if (use-region-p)
	(progn
	  (setq start (region-beginning) end (region-end))
	  (deactivate-mark))
      (setq start (line-beginning-position) end (line-end-position)))
    (pytest-command-runner
     (append (pytest-get-failed-in-string (buffer-substring start end)) flags))))

(defun pytest-run-previous ()
  "Run the previous command again."
  (interactive)
  (if-let* ((history (gethash (projectile-project-name) pytest-history)))
      (pytest-command-runner
       (split-string-and-unquote (completing-read "command:" history)))
    (error "No previous pytest invocations")))

(defun pytest-run (&optional flags)
  "Run pytest with optional FLAGS."
  (interactive (list (transient-pytest-args)))
  (pytest-command-runner (append (split-string-and-unquote pytest-test-dir) flags)))

(defun pytest-run-current-file (&optional flags)
  "Run pytest with the current file and optional FLAGS."
  (interactive (list (transient-pytest-args)))
  (pytest-command-runner
   (append (split-string-and-unquote
	    (string-remove-prefix (projectile-project-root) buffer-file-name)) flags)))

(provide 'pytest)
;;; pytest.el ends here
