;;; pytest --- stuff


;;; Commentary:


;;; Code:
(require 'python)
(require 'projectile)
(require 'transient)

(define-minor-mode pytest-mode
  "pytest"
  :lighter pytest
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") 'quit-window)
	    (define-key map (kbd "t") 'pytest-runner)
            map))

(defcustom pytest-cmd "docker-compose run --rm app pytest"
  "The pytest executable that will be used by pytest mode."
  :group 'pytest-mode)

(defcustom pytest-test-dir ""
  "Needed temporarily while we have multiple folders in papyrus."
  :group 'pytest-mode)

(defcustom pytest-history (make-hash-table :test 'equal)
  "Record the history of pytest commands."
  :group 'pytest-mode)

(transient-define-prefix pytest-runner ()
  "Pytest Runner"
  ["Arguments"
   ("-v", "Verbose" "-vv")
   ("-l", "No SQLA Logs" "--no-sqla-logs")]
  ["Test Current"
   :if is-test-file-p
   ("c" "buffer" pytest-run-current-file)
   ("f" "function" pytest-run-current-test)]
  ["Past Invocations"
   :if has-invocations-p
   ("p" "run previous" pytest-run-previous)]
  ["Failed Tetst"
   :if has-failed-tests-p
   ("f" "run failed tests" pytest-rerun-failed)
   ("s" "run selection" pytest-run-failed-selection)]
  ["General"
   ("r" "run tests" pytest-run)
   ("b" "pytest buffer" pytest-open-buffer)])

(defun is-test-file-p ()
  (string-prefix-p "test_" (file-name-nondirectory (or (buffer-file-name) ""))))

(defun has-invocations-p ()
  (not (eq (gethash (projectile-project-name) pytest-history) nil)))

(defun has-failed-tests-p ()
  (> (length (pytest-get-failed-in-string (buffer-string))) 0))

(defun transient-pytest-args ()
  (transient-args 'pytest-runner))

(defun pytest-window-split (window)
  "Prefer horizontal split of WINDOW regardless of layout."
  (split-window (frame-root-window) (frame-root-window) 'below))

(defun pytest-buffer-name ()
  (format "*%s-test*" (projectile-project-name)))

(defun pytest-save-command (command)
  "Save the currently executed COMMAND in the history lookup."
  (puthash (projectile-project-name) command pytest-history))

(defun pytest-command-formatter (params)
  (append (split-string-and-unquote pytest-cmd) params))

(defun pytest-interactive-runner (params)
  (let* ((default-directory (projectile-project-root))
	 (split-window-preferred-function 'pytest-window-split)
	 (args (pytest-command-formatter params))
	 (cmd (car args))
         (switches (cdr args))
	 (process-name (pytest-buffer-name))
	 (*buffer* (get-buffer-create process-name)))
    ;; save the invocation in the history
    (pytest-save-command params)
    ;; erase buffer if there is no process running and it has previous output
    (with-current-buffer *buffer*
      (when (and (not (term-check-proc *buffer*))
		 (> (buffer-size *buffer*) 0))
	(let ((inhibit-read-only t))
	  (erase-buffer)))
      ;; try spawn the process -- nop if process is already running
      (apply 'term-ansi-make-term process-name cmd nil switches)
      (term-mode)
      (term-char-mode)
      (pytest-mode))
    *buffer*))

(defun pytest-command-runner (params)
  (let ((split-window-preferred-function 'pytest-window-split)
	(*buffer* (pytest-interactive-runner params)))
    (when (not (get-buffer-window *buffer*))
      (switch-to-buffer-other-window *buffer*))))
(php-current-class)

(defun pytest-get-test-name (string)
  (string-match "def \\(test_.*?\\)(" string 0)
  (match-string 1 string))

(defun pytest-parent-class ()
  (save-match-data
    (when (re-search-backward "^class \\(.*?\\):" nil t)
      (match-string-no-properties 1))))

(defun pytest-run-current-test (&optional flags)
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

(defun pytest-rerun-failed (&optional flags)
  "Re-run failed test(s) in matched in the test runner buffer."
  (interactive (list (transient-pytest-args)))
  (pytest-command-runner (append (pytest-get-failed-in-string (buffer-string)) flags)))

(defun pytest-run-failed-selection (&optional flags)
  "Re-run fialed test(s) in marked-region."
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
  (if-let* ((last-command (gethash (projectile-project-name) pytest-history)))
      (pytest-command-runner last-command)
    (error "No previous pytest invocations")))

(defun pytest-run (&optional flags)
  (interactive (list (transient-pytest-args)))
  (pytest-command-runner (append (split-string-and-unquote pytest-test-dir) flags)))

(defun pytest-run-current-file (&optional flags)
  (interactive (list (transient-pytest-args)))
  (pytest-command-runner
   (split-string-and-unquote
    (string-remove-prefix (projectile-project-root) buffer-file-name))))

(defun pytest-open-buffer ()
  "Open the pytest buffer."
  (interactive)
  (let ((split-window-preferred-function 'pytest-window-split))
    (switch-to-buffer-other-window (pytest-buffer-name))))

(provide 'pytest)
;;; pytest.el ends here
