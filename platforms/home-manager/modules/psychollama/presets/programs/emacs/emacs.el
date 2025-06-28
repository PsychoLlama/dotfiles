; --- CORE BEHAVIOR ---

(setq ring-bell-function 'ignore) ; disable annoying bell
(setq visible-cursor nil) ; don't blink the cursor
(menu-bar-mode -1) ; disable menu bar

; Show line numbers in source code.
(line-number-mode 1)
(add-hook 'prog-mode-hook (lambda ()
                            ; Show line numbers
                            (display-line-numbers-mode)

                            ; Make the line number column's background transparent
                            (custom-set-faces
                              '(line-number ((t (:background nil))))
                              '(line-number-current-line ((t (:background nil)))))))

; Don't store backup files in the same directory.
(defconst df/emacs-auto-save-directory
  (expand-file-name "autosave" user-emacs-directory))

(setq auto-save-file-name-transforms
      `((".*" ,(concat df/emacs-auto-save-directory "\\1") t)))

; --- THEMEING ---

(load-theme 'atom-one-dark t)

(if (display-graphic-p)
  ; Remove chrome from the UI
  (progn
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (tooltip-mode -1))

  ; Use a transparent background in terminal mode
  (set-face-background 'default "unspecified-bg"))

; --- ESSENTIAL PLUGINS ---

(projectile-mode 1)
(counsel-mode 1)

; --- S-EXPR EDITING ---

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)

(defun df/paredit-RET ()
    "Wraps `paredit-RET' to restore evaluation when you press <return>."
    (interactive)
    (cond
     ((minibufferp)
      (read--expression-try-read))

     ((and (eq major-mode 'inferior-emacs-lisp-mode)
           (string-prefix-p "*ielm*" (buffer-name)))
      (ielm-return))

     (t
      (paredit-RET))))

(with-eval-after-load 'paredit
  (keymap-set paredit-mode-map "RET" #'df/paredit-RET))

; --- EVIL MODE ---

(defconst df/emacs-undo-tree-directory
  (expand-file-name "undo-tree" user-emacs-directory))

(global-undo-tree-mode)
(setq evil-undo-system 'undo-tree)
(setq undo-tree-history-directory-alist
      `(("." . ,df/emacs-undo-tree-directory)))

(setq evil-want-minibuffer t) ; use vim keybinds in the minibuffer
(setq evil-want-Y-yank-to-eol t) ; mirror nvim 0.10 `Y` behavior

; yay, evil!
(evil-mode 1)
(keymap-set evil-normal-state-map "C-u" 'evil-scroll-up)
(evil-terminal-cursor-changer-activate) ; fix terminal cursor modes

; evil plugins
(global-evil-surround-mode 1)
(evil-commentary-mode 1)

; --- TREESITTER INTEGRATION ---

(global-tree-sitter-mode 1)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

; --- COMPLETION ENGINE ---

(setq company-idle-delay 0)
(add-hook 'after-init-hook #'global-company-mode)

; --- COPILOT ---

(setq copilot-indent-offset-warning-disable t)
(add-hook 'prog-mode-hook 'copilot-mode)
(keymap-set evil-insert-state-map "C-j" #'copilot-accept-completion)

; --- CUSTOM KEYBINDINGS ---

(keymap-set evil-normal-state-map "SPC b" 'counsel-buffer-or-recentf)

(defun df/search-project-files ()
  "Uses `counsel-file-jump' to fuzzy-find a file within the current project."
  (interactive)
  (counsel-file-jump "" (projectile-project-root)))

(keymap-set evil-normal-state-map "SPC f" 'df/search-project-files)

(defun df/view-parent-directory ()
  "Opens the buffer's parent directory in dired"
  (interactive)
  (let ((parent-dir (expand-file-name ".." (buffer-name))))
    (if (file-directory-p parent-dir)
	(dired parent-dir)
      (message (format "Not a directory: %s" parent-dir)))))

(keymap-set evil-normal-state-map "SPC [" 'df/view-parent-directory)
