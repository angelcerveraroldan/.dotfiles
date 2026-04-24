;;; coreos.el --- CoreOS VM manager for Emacs -*- lexical-binding: t -*-

;; Author: angelcerveraroldan
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (vterm "0.0"))
;; Keywords: tools, processes, coreos

;;; Commentary:
;; A small package for managing CoreOS VMs from Emacs.
;; Intended to be used to modify CoreOS images, and run tests on them using kola.
;; Coauthored: Claude
;; Manage and use CoreOS VMs with a GUI
;; Provides a dispatch buffer (SPC v) for all interactions.

;;; Code:

(require 'cl-lib)
(require 'vterm)

;; ---------------------------------------------------------------------------
;; Data
;; ---------------------------------------------------------------------------

(cl-defstruct (coreos-vm (:constructor coreos-vm--create))
  "A tracked CoreOS VM."
  name        ;; Unique name for the VM
  cosa-dir    ;; Where to run the cosa init (path to where the machine "lives")
  state       ;; State of the VM (building, initializing, ...)
  platform    ;; osbuild platform, e.g. "qemu"
  buffer      ;; vterm buffer when running (cosa run --devshell)
  process     ;; async process for background steps (build, osbuild)
  butane      ;; butane file to be used when running the image
  extra-args) ;; additional args passed to cosa run

(defvar coreos--vms nil
  "List of `coreos-vm' structs currently tracked.")

(defvar coreos--active-vm nil
  "Name of the currently active VM, or nil.")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun coreos--get-vm (name)
  "Return the VM struct with NAME, or nil."
  (cl-find name coreos--vms :key #'coreos-vm-name :test #'string=))

(defun coreos--active ()
  "Return the active VM struct, or nil."
  (and coreos--active-vm (coreos--get-vm coreos--active-vm)))

(defun coreos--require-active ()
  "Return the active VM or signal an error."
  (or (coreos--active)
      (user-error "No active VM -- press [a] to add one")))

(defun coreos--vm-alive-p (vm)
  "Return non-nil if VM's vterm buffer is still alive."
  (and (coreos-vm-buffer vm)
       (buffer-live-p (coreos-vm-buffer vm))
       (get-buffer-process (coreos-vm-buffer vm))))

(defun coreos--buffer-name (name suffix)
  "Generate a buffer name like *coreos:dev:shell*."
  (format "*coreos:%s:%s*" name suffix))


;; ---------------------------------------------------------------------------
;; Podman / cosa
;; ---------------------------------------------------------------------------

(defcustom coreos-container-image "quay.io/coreos-assembler/coreos-assembler:latest"
  "Container image for coreos-assembler."
  :group 'coreos
  :type 'string)

(defun coreos--podman-cmd (vm-name cosa-dir cosa-args &optional interactive)
  "Build a podman command to run cosa COSA-ARGS in COSA-DIR.
VM-NAME is used as the container name (--name cosa-VM-NAME).
If INTERACTIVE is non-nil, add -ti flags.

This command is basically the one found in the docs[1], but written inside here
so that we have a little more control over the inputs to it (i.e. the image name)

[1] https://coreos.github.io/coreos-assembler/building-fcos/"
  (let ((dir (expand-file-name cosa-dir))
        (container-name (format "cosa-%s" vm-name))) ;; The name of the image will be cosa-<input name>
    (format "podman run --rm %s --security-opt=label=disable --privileged --userns=keep-id:uid=1000,gid=1000 -v=%s:/srv/ --device=/dev/kvm --device=/dev/fuse --tmpfs=/tmp -v=/var/tmp:/var/tmp --name=%s %s %s"
            (if interactive "-ti" "")
            (shell-quote-argument dir)
            container-name
            coreos-container-image
            cosa-args)))

(defun coreos--state-display (vm)
  "Return a display string for VM's current state."
  (let ((state (coreos-vm-state vm)))
    (if (and (consp state) (eq (cdr state) 'failed))
        (format "%s/failed" (car state))
      (pcase state
        ('initialized  "initialized")
        ('building     "building...")
        ('built        "built")
        ('os-building  "osbuild...")
        ('os-built     "ready")
        ('running      (if (coreos--vm-alive-p vm) "running" "dead"))
        (_             (format "%s" state))))))

(defun coreos--state-face (vm)
  "Return a face for VM's current state."
  (let ((state (coreos-vm-state vm)))
    (if (and (consp state) (eq (cdr state) 'failed))
        'error
      (pcase state
        ('running      (if (coreos--vm-alive-p vm) 'success 'error))
        ('building     'warning)
        ('os-building  'warning)
        ('os-built     'success)
        ('built        'success)
        (_             'default)))))

(defun coreos--state-key (state)
  "Return the base state symbol, stripping failed status.
If STATE is a cons like (building . failed), return building.
Otherwise return STATE as-is."
  (if (and (consp state) (eq (cdr state) 'failed))
      (car state)
    state))

(defun coreos--run-async (vm buf-name command next-state)
  "Run COMMAND asynchronously for VM in buffer BUF-NAME.
When the process finishes successfully, set VM state to NEXT-STATE."
  (let* ((name (coreos-vm-name vm))
         (proc-buf (get-buffer-create buf-name))
         (proc (start-process-shell-command
                (format "coreos-%s" name) proc-buf command)))
    (setf (coreos-vm-process vm) proc)
    (set-process-sentinel
     proc
     (lambda (p _event)
       (when (memq (process-status p) '(exit signal))
         (setf (coreos-vm-process vm) nil)
         (if (= (process-exit-status p) 0)
              (progn
                (setf (coreos-vm-state vm) next-state)
                (message "CoreOS [%s]: %s done" name next-state))
            (setf (coreos-vm-state vm) (cons (coreos--state-key (coreos-vm-state vm)) 'failed))
            (message "CoreOS [%s]: command failed (exit %d) -- see %s"
                     name (process-exit-status p) buf-name))
         (coreos--refresh-dispatch))))
    proc))

;; ---------------------------------------------------------------------------
;; VM lifecycle
;; ---------------------------------------------------------------------------

(defun coreos-add-vm (name cosa-dir config-url &optional branch)
  "Add a new VM named NAME.  Run `cosa init' with CONFIG-URL in COSA-DIR.
Optional BRANCH specifies the git branch to use."
  (interactive
   (list (read-string "VM name: ")
         (read-directory-name "cosa work dir: " "~/" nil nil)
         (read-string "Config git URL: "
                      "https://github.com/coreos/fedora-coreos-config")
         (let ((b (read-string "Branch (blank for default): ")))
           (if (string-empty-p b) nil b))))
  (when (coreos--get-vm name)
    (user-error "VM '%s' already exists" name))
  (let* ((dir (expand-file-name cosa-dir))
         (buf-name (coreos--buffer-name name "init"))
         (init-args (concat "init"
                            (when branch
                              (format " --branch %s" (shell-quote-argument branch)))
                            " " (shell-quote-argument config-url)))
         (cmd (coreos--podman-cmd name dir init-args))
         (vm (coreos-vm--create :name name
                                :cosa-dir dir
                                :state 'initializing)))
    ;; Ensure dir exists
    (make-directory dir t)
    (push vm coreos--vms)
    (unless coreos--active-vm
      (setq coreos--active-vm name))
    (coreos--run-async vm buf-name cmd 'initialized)
    (message "VM '%s' initializing..." name)
    (coreos--refresh-dispatch)))

(defun coreos-build ()
  "Run `cosa build' for the active VM."
  (interactive)
  (let* ((vm (coreos--require-active))
         (name (coreos-vm-name vm))
         (state (coreos-vm-state vm)))
    (unless (memq (coreos--state-key state) '(initialized built os-built building))
      (user-error "VM '%s' is in state '%s' -- need initialized/built to build"
                  name (coreos--state-display vm)))
    (setf (coreos-vm-state vm) 'building)
    (coreos--run-async vm
                       (coreos--buffer-name name "build")
                       (coreos--podman-cmd name (coreos-vm-cosa-dir vm) "build")
                       'built)
    (message "VM '%s' building..." name)
    (coreos--refresh-dispatch)))

(defun coreos-osbuild (platform)
  "Run `cosa buildextend-PLATFORM' for the active VM."
  (interactive
   (list (read-string "Platform (e.g. qemu): " "qemu")))
  (let* ((vm (coreos--require-active))
         (name (coreos-vm-name vm))
         (state (coreos-vm-state vm)))
    (unless (memq (coreos--state-key state) '(built os-built os-building))
      (user-error "VM '%s' is in state '%s' -- need built first"
                  name (coreos--state-display vm)))
    (setf (coreos-vm-state vm) 'os-building)
    (setf (coreos-vm-platform vm) platform)
    (coreos--run-async vm
                       (coreos--buffer-name name "osbuild")
                       (coreos--podman-cmd name (coreos-vm-cosa-dir vm)
                                           (format "buildextend-%s" platform))
                       'os-built)
    (message "VM '%s' osbuild (%s)..." name platform)
    (coreos--refresh-dispatch)))

(defun coreos-run (&optional extra-args)
  "Run `cosa run --devshell' for the active VM in a vterm buffer.
EXTRA-ARGS are appended to the command."
  (interactive
   (list (let ((args (read-string "Extra cosa run args (optional): ")))
           (if (string-empty-p args) nil args))))
  (let* ((vm (coreos--require-active))
         (name (coreos-vm-name vm))
         (state (coreos-vm-state vm)))
    (unless (memq (coreos--state-key state) '(os-built))
      (user-error "VM '%s' is in state '%s' -- need os-built first"
                  name (coreos--state-display vm)))
    (let* ((ign-name (when (coreos-vm-butane vm)
                      (coreos--generate-ignition vm)))
           (run-args (concat "run --devshell"
                             (when ign-name
                               (format " -i /srv/%s" ign-name))
                             (when extra-args (concat " " extra-args))))
           (cmd (coreos--podman-cmd name (coreos-vm-cosa-dir vm) run-args t))
           (buf-name (coreos--buffer-name name "devshell"))
           (buf (get-buffer-create buf-name)))
      (with-current-buffer buf
        (unless (derived-mode-p 'vterm-mode)
          (vterm-mode))
        (vterm-send-string cmd)
        (vterm-send-return))
      (setf (coreos-vm-buffer vm) buf)
      (setf (coreos-vm-state vm) 'running)
      (setf (coreos-vm-extra-args vm) extra-args)
      (pop-to-buffer buf)
      (message "VM '%s' starting..." name)
      (coreos--refresh-dispatch))))

(defun coreos-remove-vm (name)
  "Completely remove VM NAME -- kill all processes, containers, and buffers."
  (interactive
   (list (completing-read "Remove VM: "
                          (mapcar #'coreos-vm-name coreos--vms) nil t)))
  (when-let ((vm (coreos--get-vm name)))
    (unless (yes-or-no-p (format "Completely remove VM '%s'? " name))
      (user-error "Aborted"))
    ;; Kill devshell buffer
    (when (and (coreos-vm-buffer vm) (buffer-live-p (coreos-vm-buffer vm)))
      (kill-buffer (coreos-vm-buffer vm)))
    ;; Kill background process
    (when-let ((proc (coreos-vm-process vm)))
      (when (process-live-p proc)
        (kill-process proc)))
    ;; Kill all associated output buffers
    (dolist (suffix '("init" "build" "osbuild" "kola" "devshell"))
      (when-let ((buf (get-buffer (coreos--buffer-name name suffix))))
        (kill-buffer buf)))
    ;; Kill podman containers (main + kola)
    (coreos--kill-container name)
    (coreos--kill-container (format "%s-kola" name))
    ;; Remove from list
    (setq coreos--vms (cl-remove name coreos--vms
                                 :key #'coreos-vm-name :test #'string=))
    (when (string= coreos--active-vm name)
      (setq coreos--active-vm (and coreos--vms
                                   (coreos-vm-name (car coreos--vms)))))
    (message "VM '%s' removed" name)
    (coreos--refresh-dispatch)))

(defun coreos--kill-container (vm-name)
  "Stop and remove the podman container for VM-NAME."
  (let ((container (format "cosa-%s" vm-name)))
    (start-process "coreos-cleanup" nil "podman" "stop" container)
    (start-process "coreos-cleanup-rm" nil "podman" "rm" "-f" container)))

(defun coreos-kill-vm ()
  "Kill the active VM's devshell process."
  (interactive)
  (let* ((vm (coreos--require-active))
         (name (coreos-vm-name vm)))
    (cond
     ((coreos--vm-alive-p vm)
      (kill-buffer (coreos-vm-buffer vm))
      (coreos--kill-container name)
      (setf (coreos-vm-state vm) 'os-built)
      (message "VM '%s' killed" name)
      (coreos--refresh-dispatch))
     ((and (coreos-vm-process vm) (process-live-p (coreos-vm-process vm)))
      (kill-process (coreos-vm-process vm))
      (coreos--kill-container name)
      (message "VM '%s' background process killed" name)
      (coreos--refresh-dispatch))
     (t (message "VM '%s' has nothing running" name)))))

(defun coreos-switch-vm ()
  "Switch the active VM."
  (interactive)
  (if (null coreos--vms)
      (user-error "No VMs tracked")
    (let ((name (completing-read "Switch to VM: "
                                 (mapcar #'coreos-vm-name coreos--vms) nil t)))
      (setq coreos--active-vm name)
      (message "Active VM: %s" name)
      (coreos--refresh-dispatch))))

(defun coreos-register-vm (name cosa-dir state)
  "Register an existing build directory as a VM without running cosa init.
Useful for resuming work after restarting Emacs."
  (interactive
   (list (read-string "VM name: ")
         (read-directory-name "cosa build dir: " "~/" nil t)
         (intern (completing-read "Current state: "
                                  '("initialized" "built" "os-built")
                                  nil t))))
  (when (coreos--get-vm name)
    (user-error "VM '%s' already exists" name))
  (let ((vm (coreos-vm--create :name name
                                :cosa-dir (expand-file-name cosa-dir)
                                :state state)))
    (push vm coreos--vms)
    (unless coreos--active-vm
      (setq coreos--active-vm name))
    (message "VM '%s' registered as %s" name state)
    (coreos--refresh-dispatch)))

(defun coreos-set-butane (butane-path)
  "Set a butane file for the active VM."
  (interactive
   (list (read-file-name "Butane file: " nil nil t)))
  (let ((vm (coreos--require-active)))
    (setf (coreos-vm-butane vm) (expand-file-name butane-path))
    (message "VM '%s' butane set to %s" (coreos-vm-name vm) butane-path)
    (coreos--refresh-dispatch)))

(defun coreos--generate-ignition (vm)
  "Convert VM's butane file to ignition and validate it.
Generates the .ign file in the VM's cosa-dir.  Returns the ignition
filename (relative, for use inside the container as /srv/<name>),
or signals an error if conversion or validation fails."
  (let* ((butane-path (coreos-vm-butane vm))
         (cosa-dir (coreos-vm-cosa-dir vm))
         (ign-name "coreos-emacs.ign")
         (ign-path (expand-file-name ign-name cosa-dir))
         (exit-code nil)
         (output nil))
    (unless butane-path
      (user-error "No butane file set for VM '%s'" (coreos-vm-name vm)))
    (unless (file-exists-p butane-path)
      (user-error "Butane file does not exist: %s" butane-path))
    ;; Convert butane -> ignition
    (with-temp-buffer
      (setq exit-code (call-process "butane" nil t nil
                                    "--pretty" "--strict"
                                    butane-path
                                    "-o" ign-path))
      (setq output (buffer-string)))
    (unless (= exit-code 0)
      (user-error "butane conversion failed (exit %d):\n%s" exit-code output))
    ;; Validate ignition
    (with-temp-buffer
      (setq exit-code (call-process "ignition-validate" nil t nil ign-path))
      (setq output (buffer-string)))
    (unless (= exit-code 0)
      (user-error "ignition-validate failed (exit %d):\n%s" exit-code output))
    (message "Ignition generated and validated: %s" ign-path)
    ign-name))

(defun coreos-set-state (state)
  "Manually set the active VM's state."
  (interactive
   (list (intern (completing-read "Set state to: "
                                  '("initialized" "built" "os-built" "running")
                                  nil t))))
  (let* ((vm (coreos--require-active))
         (name (coreos-vm-name vm)))
    (setf (coreos-vm-state vm) state)
    (message "VM '%s' state set to %s" name state)
    (coreos--refresh-dispatch)))

;; ---------------------------------------------------------------------------
;; VM interaction
;; ---------------------------------------------------------------------------

(defun coreos-ssh-shell ()
  "Open the devshell vterm buffer of the active VM."
  (interactive)
  (let* ((vm (coreos--require-active))
         (buf (coreos-vm-buffer vm)))
    (if (and buf (buffer-live-p buf))
        (pop-to-buffer buf)
      (user-error "VM '%s' has no devshell buffer" (coreos-vm-name vm)))))

(defun coreos-run-command (command)
  "Send COMMAND to the active VM's devshell vterm."
  (interactive "sCommand to run on VM: ")
  (let* ((vm (coreos--require-active))
         (buf (coreos-vm-buffer vm)))
    (unless (coreos--vm-alive-p vm)
      (user-error "VM '%s' is not running" (coreos-vm-name vm)))
    (with-current-buffer buf
      (vterm-send-string command)
      (vterm-send-return))
    (pop-to-buffer buf)))

(defun coreos-kola-run (test-name)
  "Run `cosa kola run TEST-NAME' for the active VM.
This spawns a new VM inside podman to run the test."
  (interactive "sKola test name: ")
  (let* ((vm (coreos--require-active))
         (name (coreos-vm-name vm))
         (state (coreos-vm-state vm)))
    (unless (memq (coreos--state-key state) '(built os-built running))
      (user-error "VM '%s' is in state '%s' -- need at least built"
                  name (coreos--state-display vm)))
    (let* ((buf-name (coreos--buffer-name name "kola"))
           (cmd (coreos--podman-cmd
                 (format "%s-kola" name)
                 (coreos-vm-cosa-dir vm)
                 (format "kola run %s" test-name))))
      (coreos--run-async vm buf-name cmd (coreos--state-key state))
      (message "CoreOS [%s]: kola run %s..." name test-name)
      (pop-to-buffer (get-buffer-create buf-name)))))

(defun coreos-logs ()
  "Stream journalctl logs from the active VM's devshell."
  (interactive)
  (coreos-run-command "journalctl -f"))

(defun coreos-view-output ()
  "View the output buffer for the active VM's last background command."
  (interactive)
  (let* ((vm (coreos--require-active))
         (name (coreos-vm-name vm))
         ;; Try the most recent buffer: kola > osbuild > build > init
         (buf (or (get-buffer (coreos--buffer-name name "kola"))
                  (get-buffer (coreos--buffer-name name "osbuild"))
                  (get-buffer (coreos--buffer-name name "build"))
                  (get-buffer (coreos--buffer-name name "init")))))
    (if buf
        (pop-to-buffer buf)
      (user-error "No output buffer found for VM '%s'" name))))

;; ---------------------------------------------------------------------------
;; Dispatch buffer
;; ---------------------------------------------------------------------------

(defvar coreos-dispatch-mode-map
  (make-sparse-keymap)
  "Keymap for `coreos-dispatch-mode'.")

(define-derived-mode coreos-dispatch-mode special-mode "CoreOS"
  "Major mode for the CoreOS VM dispatch buffer."
  :interactive nil
  (setq buffer-read-only t
        truncate-lines t))

;; Bind keys in Evil's normal state so they aren't shadowed by Evil.
(with-eval-after-load 'evil
  (evil-define-key* 'normal coreos-dispatch-mode-map
    "a" #'coreos-dispatch--add
    "v" #'coreos-dispatch--switch
    "i" #'coreos-dispatch--build
    "o" #'coreos-dispatch--osbuild
    "R" #'coreos-dispatch--run
    "t" #'coreos-dispatch--kola
    "s" #'coreos-dispatch--shell
    "c" #'coreos-dispatch--command
    "l" #'coreos-dispatch--logs
    "k" #'coreos-dispatch--kill
    "r" #'coreos-dispatch--remove
    "e" #'coreos-dispatch--register
    "S" #'coreos-dispatch--set-state
    "O" #'coreos-dispatch--output
    "g" #'coreos-dispatch--refresh
	"b" #'coreos-set-butane 
    "q" #'quit-window))

(defun coreos--render-dispatch ()
  "Render the dispatch buffer contents."
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)
    (insert (propertize "CoreOS VM Manager\n" 'face 'bold))
    (insert (make-string 40 ?=) "\n\n")

    ;; Active VM
    (if-let ((vm (coreos--active)))
        (progn
          (insert (propertize "Active VM: " 'face 'bold))
          (insert (propertize (coreos-vm-name vm) 'face 'success))
          (insert "  ")
          (insert (propertize (format "[%s]" (coreos--state-display vm))
                              'face (coreos--state-face vm)))
          (insert "\n")
          (insert (format "  Dir:      %s\n" (abbreviate-file-name (coreos-vm-cosa-dir vm))))
          (when (coreos-vm-platform vm)
            (insert (format "  Platform: %s\n" (coreos-vm-platform vm))))
          (when (coreos-vm-butane vm)
            (insert (format "  Butane:   %s\n" (abbreviate-file-name (coreos-vm-butane vm))))))
      (insert (propertize "No active VM\n" 'face 'warning)))

    (insert (make-string 40 ?-) "\n\n")

    ;; Keybindings - TODO: Make this message dynamic, so we can customise keybs
    (insert (propertize "Lifecycle:\n" 'face 'bold))
    (insert "  [a] Add VM (cosa init)    [i] cosa build\n")
    (insert "  [o] cosa osbuild          [R] cosa run (devshell)\n\n")
    (insert (propertize "Interact:\n" 'face 'bold))
    (insert "  [t] Run kola test         [s] SSH shell (devshell)\n")
    (insert "  [c] Run command           [l] Stream logs\n\n")
    (insert (propertize "Manage:\n" 'face 'bold))
    (insert "  [v] Switch active VM      [k] Kill VM/process\n")
    (insert "  [r] Remove VM             [e] Register existing dir\n")
    (insert "  [S] Set state             [O] View output buffer\n")
    (insert "  [g] Refresh               [q] Quit\n")
    (insert "  [b] Set butane file\n")

    ;; VM list
    (insert "\n" (propertize "VMs:\n" 'face 'bold))
    (if (null coreos--vms)
        (insert "  (none)\n")
      (dolist (vm coreos--vms)
        (let* ((active (string= (coreos-vm-name vm) coreos--active-vm))
               (marker (if active " * " "   "))
               (state-str (coreos--state-display vm))
               (state-face (coreos--state-face vm)))
          (insert marker)
          (insert (propertize (format "%-12s" (coreos-vm-name vm))
                              'face (if active 'bold 'default)))
          (insert (propertize (format "%-14s" state-str) 'face state-face))
          (insert (format "  %s" (abbreviate-file-name (coreos-vm-cosa-dir vm))))
          (when (coreos-vm-platform vm)
            (insert (format "  [%s]" (coreos-vm-platform vm))))
          (insert "\n"))))

    (goto-char (min pos (point-max)))))

(defun coreos--refresh-dispatch ()
  "Refresh the dispatch buffer if it exists."
  (when-let ((buf (get-buffer "*CoreOS*")))
    (with-current-buffer buf
      (coreos--render-dispatch))))

;; Dispatch wrappers

(defun coreos-dispatch--add ()
  "Add a VM from dispatch." (interactive) (call-interactively #'coreos-add-vm))
(defun coreos-dispatch--switch ()
  "Switch VM from dispatch." (interactive) (call-interactively #'coreos-switch-vm))
(defun coreos-dispatch--build ()
  "Build from dispatch." (interactive) (call-interactively #'coreos-build))
(defun coreos-dispatch--osbuild ()
  "Osbuild from dispatch." (interactive) (call-interactively #'coreos-osbuild))
(defun coreos-dispatch--run ()
  "Run from dispatch." (interactive) (call-interactively #'coreos-run))
(defun coreos-dispatch--kola ()
  "Kola from dispatch." (interactive) (call-interactively #'coreos-kola-run))
(defun coreos-dispatch--shell ()
  "Shell from dispatch." (interactive) (call-interactively #'coreos-ssh-shell))
(defun coreos-dispatch--command ()
  "Command from dispatch." (interactive) (call-interactively #'coreos-run-command))
(defun coreos-dispatch--logs ()
  "Logs from dispatch." (interactive) (call-interactively #'coreos-logs))
(defun coreos-dispatch--kill ()
  "Kill from dispatch."
  (interactive)
  (when (yes-or-no-p "Kill active VM? ")
    (call-interactively #'coreos-kill-vm)))
(defun coreos-dispatch--remove ()
  "Remove from dispatch." (interactive) (call-interactively #'coreos-remove-vm))
(defun coreos-dispatch--register ()
  "Register from dispatch." (interactive) (call-interactively #'coreos-register-vm))
(defun coreos-dispatch--set-state ()
  "Set state from dispatch." (interactive) (call-interactively #'coreos-set-state))
(defun coreos-dispatch--output ()
  "Output from dispatch." (interactive) (call-interactively #'coreos-view-output))
(defun coreos-dispatch--refresh ()
  "Refresh dispatch." (interactive) (coreos--refresh-dispatch))

;;;###autoload
(defun coreos-dispatch ()
  "Open the CoreOS VM dispatch buffer."
  (interactive)
  (let ((buf (get-buffer-create "*CoreOS*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'coreos-dispatch-mode)
        (coreos-dispatch-mode))
      (coreos--render-dispatch))
    (pop-to-buffer buf)))

(provide 'coreos)
;;; coreos.el ends here
