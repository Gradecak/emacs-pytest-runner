;;; pytest --- stuff


;;; Commentary:


;;; Code:

(require 'python)
(require 'projectile)

(define-minor-mode pytest-mode
  "pytest"
  :lighter pytest
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") 'quit-window)
	    (define-key map (kbd "F") 'pytest-rerun-failed)
	    (define-key map (kbd "R") 'pytest-run)
	    (define-key map (kbd "P") 'pytest-run-previous)
	    (define-key map (kbd "C-c C-c") 'pytest-run-failed-selection)
            map))

(defvar pytest-cmd "docker-compose run --rm app pytest"
  "The pytest executable that will be used by pytest mode.")

(defvar pytest-test-dir ""
  "Needed temporarily while we have multiple folders in papyrus.")

(defvar pytest-history (make-hash-table :test 'equal)
  "Record the history of pytest commands.")

(defvar pytest-buffer-name "*tests*"
  "Buffer name for pytest suites.")

(defun pytest-save-command (command)
  "Save the currently executed COMMAND in the history lookup."
  (puthash (projectile-project-root) command pytest-history))

(defun pytest-command-runner (params bufferName)
  "Execute pytest process with the given PARAMS and put output into into a special bufer named BUFFERNAME."
  (let* ((*server-buffer* (get-buffer-create bufferName))
	 ;; prefer horizontal split
	 (split-window-preferred-function 'mg/split-window-horizontal)
	 (command (format "%s %s" pytest-cmd params)))
    ;; If the process is not already running, start it
    (when (not (get-process bufferName))
      (with-current-buffer *server-buffer*
	;; if there is already output from previous run clear it.
	(when (> (buffer-size *server-buffer*) 0)
	  (erase-buffer))
	(ansi-color-for-comint-mode-on)
	(comint-mode)
	(pytest-mode))
      (let ((*server-process*
	     (start-process-shell-command bufferName *server-buffer* command)))
	(set-process-filter *server-process* 'comint-output-filter)
	(pytest-save-command params)))
    ;; switch focus to server buffer if its not already visible
    (when (not (get-buffer-window *server-buffer*))
      (switch-to-buffer-other-window bufferName))))

(defun pytest-run-failed-in-string (string)
  "Parse the failed test(s) from the STRING and re-run just the test(s) extracted."
  (let ((pos 0)
	(matches)
	(params))
    (save-match-data
      (while (string-match "^\\(FAILED\\|ERROR\\) \\(.*?\\.py::.*?\\)\\( - \\|$\\)" string pos)
	(push (match-string 2 string) matches)
	(setq pos (match-end 2))))
    (setq params (mapconcat (lambda (m) (format "\"%s\"" m)) matches " "))
    (pytest-command-runner params pytest-buffer-name)))

(defun pytest-rerun-failed ()
  "Re-run failed test(s) in matched in the test runner buffer."
  (interactive)
  (pytest-run-failed-in-string (buffer-string)))

(defun pytest-run-failed-selection ()
  "Re-run fialed test(s) in marked-region."
  (interactive)
  (let (start end)
    (if (use-region-p)
	(setq start (region-beginning) end (region-end))
      (setq start (line-beginning-position) end (line-end-position)))
    (pytest-run-failed-in-string (buffer-substring start end))))

(defun pytest-run-previous ()
  "Run the previous command again."
  (interactive)
  (if-let* ((default-directory (projectile-project-root))
	    (last-command (gethash default-directory pytest-history)))
      (pytest-command-runner last-command pytest-buffer-name)
    (error "No previous pytest invocations")))

(defun pytest-run-current-file ()
  "Pass the file name associated with buffer as argument to pytest runner."
  (interactive)
  (pytest-command-runner
   (string-remove-prefix (projectile-project-root) buffer-file-name)
   pytest-buffer-name))


(defun pytest-run ()
  "Run the test suite for the current projectile project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (pytest-command-runner pytest-test-dir pytest-buffer-name)))

(defun pytest-open-buffer ()
  "Open the pytest buffer."
  (interactive)
  (let ((split-window-preferred-function 'mg/split-window-horizontal))
    (switch-to-buffer-other-window pytest-buffer-name)))

(provide 'pytest)
;;; pytest.el ends here
