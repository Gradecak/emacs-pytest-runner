;;; pytest --- stuff


;;; Commentary:


;;; Code:

(require 'python)
(require 'projectile)

(define-minor-mode pytest-mode
  "pytest"
  :lighter pytest
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-q") 'quit-window)
	    (define-key map (kbd "C-S-f") 'pytest-rerun-failed)
	    (define-key map (kbd "C-S-r") 'pytest-run)
	    (define-key map (kbd "C-S-p") 'pytest-run-previous)
	    (define-key map (kbd "C-c C-v") 'pytest-run-failed-selection)
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

(defun pytest-rerun-failed ()
  "Re-run failed test(s) in matched in the test runner buffer."
  (interactive)
  (pytest-command-runner (pytest-get-failed-in-string (buffer-string))))

(defun pytest-run-failed-selection ()
  "Re-run fialed test(s) in marked-region."
  (interactive)
  (let (start end)
    (if (use-region-p)
	(progn
	  (setq start (region-beginning) end (region-end))
	  (deactivate-mark))
      (setq start (line-beginning-position) end (line-end-position)))
    (pytest-command-runner
     (pytest-get-failed-in-string (buffer-substring start end)))))

(defun pytest-run-previous ()
  "Run the previous command again."
  (interactive)
  (if-let* ((last-command (gethash (projectile-project-name) pytest-history)))
      (pytest-command-runner last-command)
    (error "No previous pytest invocations")))

(defun pytest-run ()
  (interactive)
  (pytest-command-runner (split-string-and-unquote pytest-test-dir)))

(defun pytest-run-current-file ()
  (interactive)
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
