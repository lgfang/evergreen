;;; evergreen.el --- Emacs interface for MongoDB Evergreen CLI -*- lexical-binding: t; -*-

;; Author: Lungang Fang
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))

;;; Commentary:
;; An Emacs package that provides a transient-based interface for the
;; MongoDB Evergreen CLI, simplifying the patch submission workflow.

;;; Code:

(require 'comint)
(require 'transient)

;;;; Customizable variables

(defgroup evergreen nil
  "Interface for MongoDB Evergreen CLI."
  :group 'tools
  :prefix "evergreen-")

(defcustom evergreen-default-project "sys-perf"
  "Default project name for evergreen patch commands.
Can be a string or a function returning a string."
  :type '(choice string function)
  :group 'evergreen)

(defcustom evergreen-default-description #'evergreen--last-commit-message
  "Default description for evergreen patch commands.
Can be a string or a function returning a string."
  :type '(choice string function)
  :group 'evergreen)

(defcustom evergreen-default-alias nil
  "Default alias for evergreen patch commands.
Can be nil, a string, or a function returning a string."
  :type '(choice (const nil) string function)
  :group 'evergreen)

(defcustom evergreen-config-file "~/.evergreen.yml"
  "Path to the Evergreen configuration file."
  :type 'string
  :group 'evergreen)

(defcustom evergreen-patch-default-flags '()
  "Default flags for `evergreen patch'.
A list of flag strings to enable by default in the transient menu.
Possible values: \"--include-modules\", \"--finalize\", \"--browse\",
\"--uncommitted\"."
  :type '(repeat string)
  :group 'evergreen)

(defcustom evergreen-patch-set-module-default-flags '("--uncommitted")
  "Default flags for `evergreen patch-set-module'.
A list of flag strings to enable by default in the transient menu.
Possible values: \"--uncommitted\"."
  :type '(repeat string)
  :group 'evergreen)

(defcustom evergreen-project-source-dirs '(("sys-perf" . "~/source/mongo"))
  "Alist mapping project names to their source repository paths.
Each entry is (PROJECT-NAME . SOURCE-DIR).  The evergreen patch
command will be run from SOURCE-DIR for the given project."
  :type '(alist :key-type string :value-type directory)
  :group 'evergreen)

;;;; Helper utilities

(defun evergreen--resolve-value (val)
  "If VAL is a function, call it; otherwise return it as-is."
  (if (functionp val) (funcall val) val))


(defun evergreen--repo-root ()
  "Return the root directory of the current git repository."
  (string-trim
   (shell-command-to-string "git rev-parse --show-toplevel")))

(defun evergreen--current-branch ()
  "Return the current git branch name."
  (string-trim
   (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))

(defun evergreen--last-commit-message ()
  "Return the last commit message."
  (string-trim
   (shell-command-to-string "git log -1 --pretty=%s")))

(defun evergreen--detect-module ()
  "Detect if the current directory is a known Evergreen module.
Returns the module name (e.g., \"dsi\") or nil."
  (let ((dir (abbreviate-file-name default-directory)))
    (when (string-match "/\\(dsi\\)\\(?:-[0-9]+\\)?/" dir)
      (match-string 1 dir))))

(defun evergreen--project-source-dir (project)
  "Look up the source directory for PROJECT from `evergreen-project-source-dirs'."
  (let ((entry (assoc project evergreen-project-source-dirs)))
    (when entry
      (expand-file-name (cdr entry)))))

(defun evergreen--update-module-path (project module module-path)
  "Update MODULE path to MODULE-PATH for PROJECT in `evergreen-config-file'."
  (let* ((config-file (expand-file-name evergreen-config-file))
         (content (with-temp-buffer
                    (insert-file-contents config-file)
                    (buffer-string)))
         (in-project nil)
         (in-module-paths nil)
         (result nil))
    (dolist (line (split-string content "\n"))
      (cond
       ;; Detect project name line
       ((string-match "^  - name: \\(.+\\)" line)
        (setq in-project (string= (match-string 1 line) project))
        (setq in-module-paths nil)
        (push line result))
       ;; Detect module_paths section
       ((and in-project (string-match "^    module_paths:" line))
        (setq in-module-paths t)
        (push line result))
       ;; Replace module path line
       ((and in-project in-module-paths
             (string-match (format "^      %s: " (regexp-quote module)) line))
        (push (format "      %s: %s" module module-path) result))
       ;; Any other line at lower indent exits module_paths
       (t
        (when (and in-module-paths
                   (not (string-match "^      " line))
                   (not (string= "" line)))
          (setq in-module-paths nil))
        (push line result))))
    (with-temp-file config-file
      (insert (string-join (nreverse result) "\n")))))

(defun evergreen--process-sentinel (process _event)
  "Sentinel for evergreen PROCESS.  Insert exit status into the buffer."
  (when (memq (process-status process) '(exit signal))
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize
                   (format "\nProcess exited with code %d\n"
                           (process-exit-status process))
                   'font-lock-face (if (= 0 (process-exit-status process))
                                       'success
                                     'error))))
        (when evergreen--module-dir
          (setq default-directory evergreen--module-dir))))))

(defun evergreen--run-command (cmd-parts dir)
  "Run command built from CMD-PARTS in DIR in the *evergreen* comint buffer.
CMD-PARTS is a list of command-line arguments.
The user can interact with the process directly in the buffer."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (command (string-join cmd-parts " "))
         (buf (get-buffer-create "*evergreen*")))
    (with-current-buffer buf
      (unless (eq major-mode 'evergreen-mode)
        (evergreen-mode))
      ;; Kill any existing process before starting a new one
      (when-let ((old-proc (get-buffer-process buf)))
        (when (process-live-p old-proc)
          (delete-process old-proc)))
      ;; Set the buffer's working directory to DIR so comint-exec uses it
      (setq default-directory dir)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (propertize
                 (format "\n$ %s\n"
                         (if (> (length cmd-parts) 2)
                             (concat (car cmd-parts) " " (cadr cmd-parts) " \\\n"
                                     (mapconcat (lambda (part) (concat "    " part))
                                                (cddr cmd-parts) " \\\n"))
                           command))
                 'font-lock-face 'font-lock-comment-face))
        (insert (propertize (format "  (in %s)\n" default-directory)
                            'font-lock-face 'font-lock-comment-delimiter-face))))
    ;; Use comint-exec so comint properly manages the process I/O
    (comint-exec buf "evergreen" shell-file-name nil (list shell-command-switch command))
    (let ((proc (get-buffer-process buf)))
      (set-process-sentinel proc #'evergreen--process-sentinel)
      (set-process-query-on-exit-flag proc t))
    (display-buffer buf)))

;;;; Evergreen mode

(defvar-keymap evergreen-mode-map
  :parent comint-mode-map
  :doc "Keymap for `evergreen-mode'.")

(define-derived-mode evergreen-mode comint-mode "Evergreen"
  "Major mode for the Evergreen buffer.
Derives from `comint-mode' so the user can interact with CLI
processes directly.  Use `evergreen-transient-menu' to invoke
patch commands.
\\{evergreen-mode-map}"
  (setq-local comint-prompt-read-only nil)
  (setq-local evergreen--module-dir nil))

;;;; Entry point

(defun evergreen--process-live-p ()
  "Return non-nil if the *evergreen* buffer has a live process."
  (when-let ((buf (get-buffer "*evergreen*"))
             (proc (get-buffer-process buf)))
    (process-live-p proc)))

(defun evergreen--setup-buffer (&optional dir)
  "Ensure the *evergreen* buffer exists with `evergreen-mode' active.
Uses DIR or the git repository root as the working directory.
Signals an error if a process is already running."
  (when (evergreen--process-live-p)
    (user-error "A process is still running in *evergreen* — switch to the buffer to interact with it"))
  (let* ((default-directory (if dir (file-name-as-directory (expand-file-name dir))
                              default-directory))
         (dir (file-name-as-directory (expand-file-name (evergreen--repo-root))))
         (default-directory dir)
         (buf (get-buffer-create "*evergreen*"))
         (module (evergreen--detect-module))
         (branch (evergreen--current-branch)))
    (with-current-buffer buf
      (unless (eq major-mode 'evergreen-mode)
        (evergreen-mode))
      (setq default-directory dir)
      (setq evergreen--module-dir dir)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize "\nEvergreen\n" 'font-lock-face 'font-lock-keyword-face))
        (insert (propertize "=========\n" 'font-lock-face 'font-lock-keyword-face))
        (insert (propertize "Directory:  " 'font-lock-face 'transient-heading)
                (propertize (format "%s\n" dir) 'font-lock-face 'font-lock-doc-face))
        (insert (propertize "Branch:     " 'font-lock-face 'transient-heading)
                (propertize (format "%s\n" branch) 'font-lock-face 'font-lock-constant-face))
        (insert (propertize "Module:     " 'font-lock-face 'transient-heading)
                (propertize (format "%s\n" (or module "none"))
                            'font-lock-face (if module 'transient-value 'shadow)))
        (insert (propertize (make-string 40 ?─) 'font-lock-face 'shadow))
        (insert "\n")))
    (switch-to-buffer buf)
    buf))

(transient-define-prefix evergreen--menu ()
  "Transient menu for Evergreen commands."
  ["Evergreen"
   ("p" "Patch" evergreen-patch)
   ("m" "Patch set-module" evergreen-patch-set-module)
   ("u" "Get update" evergreen-get-update)])

;;;###autoload
(defun evergreen (dir)
  "Open the *evergreen* buffer and show the command menu.
With prefix argument, prompt for DIR."
  (interactive (list (when current-prefix-arg
                       (read-directory-name "Evergreen directory: "))))
  (evergreen--setup-buffer dir)
  (evergreen--menu))

;;;; Transient: evergreen patch

(defun evergreen--fetch-aliases (project)
  "Fetch patch aliases for PROJECT from the Evergreen CLI.
Returns a deduplicated list of alias name strings."
  (condition-case nil
      (let* ((cmd (format "evergreen list --patch-aliases --include-config-aliases -p %s 2>/dev/null"
                          (shell-quote-argument project)))
             (output (shell-command-to-string cmd))
             (seen (make-hash-table :test 'equal))
             aliases)
        (dolist (line (split-string output "\n" t))
          (when (string-match "^\\([^\t ]+\\)\t" line)
            (let ((name (match-string 1 line)))
              (unless (gethash name seen)
                (puthash name t seen)
                (push name aliases)))))
        (nreverse aliases))
    (error nil)))

(defun evergreen--read-alias (prompt _initial-input _history)
  "Read an alias with completion from Evergreen CLI.
PROMPT is the prompt string."
  (let* ((project (or (transient-arg-value "--project=" (transient-args 'evergreen-patch))
                      (evergreen--resolve-value evergreen-default-project)))
         (aliases (evergreen--fetch-aliases project)))
    (completing-read prompt aliases nil nil)))

(transient-define-prefix evergreen-patch ()
  "Submit a new patch to Evergreen."
  :value (lambda ()
            (let ((default-directory
                   (or (when-let ((buf (get-buffer "*evergreen*")))
                         (buffer-local-value 'evergreen--module-dir buf))
                       default-directory)))
              (let ((alias (evergreen--resolve-value evergreen-default-alias)))
                (append
                 (list
                  (format "--project=%s" (evergreen--resolve-value evergreen-default-project))
                  (format "--description=%s" (evergreen--resolve-value evergreen-default-description)))
                 (when alias
                   (list (format "--alias=%s" alias)))
                 evergreen-patch-default-flags))))
  ["Options"
   ("-p" "project" "--project=")
   ("-d" "description" "--description=")
   ("-a" "alias" "--alias=" :reader evergreen--read-alias)
   ("-r" "reuse-compile-from" "--reuse-compile-from=")
   ("-i" "include-modules" "--include-modules")
   ("-f" "finalize" "--finalize")
   ("-b" "browse" "--browse")
   ("-u" "uncommitted" "--uncommitted")]
  ["Actions"
   ("p" "Submit patch" evergreen--do-patch)])

(defun evergreen--do-patch (&optional args)
  "Execute the evergreen patch command with ARGS from transient."
  (interactive (list (transient-args 'evergreen-patch)))
  (let* ((project (or (transient-arg-value "--project=" args)
                      (evergreen--resolve-value evergreen-default-project)))
         (description (transient-arg-value "--description=" args))
         (alias (transient-arg-value "--alias=" args))
         (reuse-compile (transient-arg-value "--reuse-compile-from=" args))
         (include-modules (transient-arg-value "--include-modules" args))
         (finalize (transient-arg-value "--finalize" args))
         (browse (transient-arg-value "--browse" args))
         (uncommitted (transient-arg-value "--uncommitted" args))
         (source-dir (evergreen--project-source-dir project))
         (module-dir (when-let ((buf (get-buffer "*evergreen*")))
                       (buffer-local-value 'evergreen--module-dir buf)))
         (module (when module-dir
                   (let ((default-directory module-dir))
                     (evergreen--detect-module))))
         (cmd-parts (list "evergreen" "patch"
                          (format "--project %s" (shell-quote-argument project)))))
    ;; Update module path in config if needed
    (when (and module include-modules module-dir)
      (evergreen--update-module-path project module
                                     (directory-file-name (expand-file-name module-dir)))
      (let ((buf (get-buffer "*evergreen*")))
        (when buf
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert (propertize
                       (format "\nUpdated %s module path to %s in %s\n"
                               module (directory-file-name (expand-file-name module-dir))
                               evergreen-config-file)
                       'font-lock-face 'font-lock-warning-face)))))))
    ;; Build command
    (when description
      (setq cmd-parts (append cmd-parts
                              (list (format "--description %s" (shell-quote-argument description))))))
    (when alias
      (setq cmd-parts (append cmd-parts
                              (list (format "--alias %s" (shell-quote-argument alias))))))
    (when reuse-compile
      (setq cmd-parts (append cmd-parts
                              (list (format "--param %s"
                                            (shell-quote-argument
                                             (format "reuse_compile_from=%s" reuse-compile)))))))
    (when include-modules
      (setq cmd-parts (append cmd-parts (list "--include-modules"))))
    (when finalize
      (setq cmd-parts (append cmd-parts (list "--finalize"))))
    (when browse
      (setq cmd-parts (append cmd-parts (list "--browse"))))
    (when uncommitted
      (setq cmd-parts (append cmd-parts (list "--uncommitted"))))
    ;; Execute
    (if source-dir
        (evergreen--run-command cmd-parts source-dir)
      (user-error "No source directory configured for project %s" project))))

(defun evergreen--collect-buffer-patch-ids ()
  "Collect patch IDs from the *evergreen* buffer.
Parses lines matching `         ID : <hex-id>'.
Returns a list of (ID . DESCRIPTION) cons cells."
  (let ((buf (get-buffer "*evergreen*"))
        patches)
    (when buf
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\s-*ID : \\([0-9a-f]+\\)" nil t)
            (let ((id (match-string 1))
                  (desc nil)
                  (limit (save-excursion
                           (or (re-search-forward "^\\s-*ID : " nil t)
                               (point-max)))))
              (save-excursion
                (when (re-search-forward "^Description : \\(.+\\)" limit t)
                  (setq desc (match-string 1))))
              (push (cons id desc) patches))))))
    (nreverse patches)))

(defun evergreen--fetch-recent-patches ()
  "Fetch recent non-hidden patches from Evergreen CLI.
Returns a list of (ID . DESCRIPTION) cons cells."
  (condition-case nil
      (let* ((json-str (shell-command-to-string
                        "evergreen list-patches -n 30 -j 2>/dev/null"))
             (patches (json-parse-string json-str :object-type 'alist))
             result)
        (seq-doseq (p patches)
          (let ((hidden (alist-get 'hidden p))
                (id (alist-get 'patch_id p))
                (desc (alist-get 'description p))
                (pr-number (let ((gh (alist-get 'github_patch_data p)))
                             (and gh (alist-get 'pr_number gh)))))
            (unless (or (eq hidden t)
                        (and pr-number (> pr-number 0)))
              (push (cons id desc) result))))
        (nreverse result))
    (error nil)))

(defun evergreen--collect-patch-ids ()
  "Collect patch IDs from the buffer and recent Evergreen patches.
Returns a deduplicated list of completion candidates."
  (let ((buffer-patches (evergreen--collect-buffer-patch-ids))
        (remote-patches (evergreen--fetch-recent-patches))
        (seen (make-hash-table :test 'equal))
        candidates)
    (dolist (p buffer-patches)
      (let ((id (car p))
            (desc (cdr p)))
        (unless (gethash id seen)
          (puthash id t seen)
          (push (if desc (format "%s  %s" id desc) id) candidates))))
    (dolist (p remote-patches)
      (let ((id (car p))
            (desc (cdr p)))
        (unless (gethash id seen)
          (puthash id t seen)
          (push (if desc (format "%s  %s" id desc) id) candidates))))
    (nreverse candidates)))

(defun evergreen--read-patch-id (prompt _initial-input _history)
  "Read a patch ID with completion from buffer and recent patches.
PROMPT is the prompt string.  Allows free-form input."
  (let* ((choice (completing-read prompt (evergreen--collect-patch-ids) nil nil))
         (id (car (split-string choice))))
    id))

;;;; Transient: evergreen patch-set-module

(transient-define-prefix evergreen-patch-set-module ()
  "Set a module on an existing Evergreen patch."
  :value (lambda ()
           (let* ((module-dir (when-let ((buf (get-buffer "*evergreen*")))
                                (buffer-local-value 'evergreen--module-dir buf)))
                  (module (when module-dir
                            (let ((default-directory module-dir))
                              (evergreen--detect-module)))))
             (append
              (when module
                (list (format "--module=%s" module)))
              evergreen-patch-set-module-default-flags)))
  ["Options"
   ("-i" "patch-id" "--patch-id=" :reader evergreen--read-patch-id)
   ("-m" "module" "--module=")
   ("-u" "uncommitted" "--uncommitted")]
  ["Actions"
   ("m" "Submit patch-set-module" evergreen--do-patch-set-module)])

(defun evergreen--do-patch-set-module (&optional args)
  "Execute the evergreen patch-set-module command with ARGS from transient."
  (interactive (list (transient-args 'evergreen-patch-set-module)))
  (let* ((module-dir (when-let ((buf (get-buffer "*evergreen*")))
                       (buffer-local-value 'evergreen--module-dir buf)))
         (patch-id (transient-arg-value "--patch-id=" args))
         (module (or (transient-arg-value "--module=" args)
                     (when module-dir
                       (let ((default-directory module-dir))
                         (evergreen--detect-module)))))
         (uncommitted (transient-arg-value "--uncommitted" args))
         (cmd-parts (list "evergreen" "patch-set-module"
                          (format "--module %s" (shell-quote-argument (or module ""))))))
    (unless patch-id
      (user-error "Patch ID is required"))
    (unless module
      (user-error "Module name is required"))
    (setq cmd-parts (append cmd-parts
                            (list (format "--id %s" (shell-quote-argument patch-id)))))
    (when uncommitted
      (setq cmd-parts (append cmd-parts (list "--uncommitted"))))
    (evergreen--run-command cmd-parts (or module-dir default-directory))))

;;;; Update Evergreen CLI

(defun evergreen-get-update ()
  "Download and install the latest Evergreen CLI."
  (interactive)
  (let ((dir (or (when-let ((buf (get-buffer "*evergreen*")))
                   (buffer-local-value 'evergreen--module-dir buf))
                 default-directory)))
    (evergreen--run-command '("sudo" "evergreen" "get-update" "--install") dir)))

(provide 'evergreen)

;;; evergreen.el ends here
