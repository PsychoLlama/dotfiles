(setq ring-bell-function 'ignore) ; disable annoying bell
(setq visible-cursor nil) ; don't blink the cursor
(menu-bar-mode -1) ; disable menu bar

(setq indent-tabs-mode nil) ; use spaces as the default
(setq tab-width 2) ; set the default tab width to 2 spaces

(line-number-mode 1) ; Show line numbers in source code.
(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode)
                            (custom-set-faces ; Make the line number column's background transparent
                             '(line-number ((t (:background nil))))
                             '(line-number-current-line ((t (:background nil)))))))

(setq select-enable-clipboard nil) ; Prefer xclip to sync the kill ring with the system clipboard.
(xclip-mode 1)

(defconst df/emacs-auto-save-directory ; Don't store backup files in the same directory.
  (expand-file-name "autosave" user-emacs-directory))

(setq auto-save-file-name-transforms
      `((".*" ,(concat df/emacs-auto-save-directory "\\1") t)))


;;; --- THEMEING ---
(load-theme 'doom-one t)
(if (display-graphic-p)
    (progn ; Remove chrome from the UI
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (tooltip-mode -1))
  (set-face-background 'default "unspecified-bg")) ; Use a transparent background in terminal mode


;;; --- PROJECT MANAGEMENT ---
(projectile-mode 1)
(setq projectile-project-search-path '(("~/projects/" . 2)))
(setq projectile-switch-project-action #'projectile-dired)


;;; --- PICKERS/NAVIGATION ---
(counsel-mode 1)


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


;;; --- EVIL MODE ---
(defconst df/emacs-undo-tree-directory
  (expand-file-name "undo-tree" user-emacs-directory))

(global-undo-tree-mode)
(setq evil-undo-system 'undo-tree)
(setq undo-tree-history-directory-alist
      `(("." . ,df/emacs-undo-tree-directory)))

(setq evil-want-keybinding nil) ; Extra modes are handled by `evil-collection`.
(setq evil-want-minibuffer t) ; Use vim keybinds in the minibuffer.
(setq evil-want-Y-yank-to-eol t) ; Mirror nvim 0.10 `Y` behavior.
(setq evil-search-wrap nil) ; Basically `&nowrapscan`.

(evil-mode 1) ; yay, evil!
(keymap-set evil-normal-state-map "C-u" 'evil-scroll-up)
(evil-terminal-cursor-changer-activate) ; fix terminal cursor modes

(global-evil-surround-mode 1)
(evil-commentary-mode 1)
(evil-collection-init)


;;; --- TREESITTER INTEGRATION ---
(global-tree-sitter-mode 1)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)


;;; --- COMPLETION ENGINE ---
(setq company-idle-delay 0)
(add-hook 'after-init-hook #'global-company-mode)


;;; --- AUTO-FORMATTING ---
(apheleia-global-mode 1)

(setq apheleia-formatters ; Map names to shell commands
      `((prettier . (,df/formatter-prettier filepath))
        (eslint . (,df/formatter-eslint
                   "--stdin-filename" filepath
                   "--stdin"
                   "--fix-to-stdout"))

        (nixfmt . (,df/formatter-nixfmt "--quiet"))
        (stylua . (,df/formatter-stylua
                   "--stdin-filepath" filepath
                   "--search-parent-directories"
                   "--allow-hidden"
                   "-"))

        (rustfmt . ("rustfmt" "--emit=stdout"))
        (gofmt . ("gofmt"))))

(setq apheleia-mode-alist ; Map major modes to formatters
      '((markdown-mode . (prettier))
        (css-mode . (prettier))
        (less-css-mode . (prettier))
        (html-mode . (prettier))
        (typescript-ts-mode . (prettier eslint))
        (rust-mode . (rustfmt))
        (yaml-ts-mode . (prettier))
        (nix-mode . (nixfmt))
        (lua-mode . (stylua))
        (go-mode . (gofmt))))


;;; --- LSP INTEGRATION ---
(setq eglot-server-programs
      `((nix-mode . (,df/lsp-nil :initializationOptions
                                 (:nil (:nix (:flake (:autoArchive t))))))
        (lua-mode . (,df/lsp-luals :initializationOptions
                                   (:Lua (:format (:enable :json-false)
                                                  :workspace (:checkThirdParty :json-false)
                                                  :addonManager (:enable :json-false)))))
        (typescript-ts-mode . (,df/lsp-tsserver "--stdio"))
        (go-ts-mode . (,df/lsp-gopls "-remote=auto"))
        (rust-mode . (,df/lsp-rust-analyzer))
        (nushell-ts-mode . ("nu" "--lsp"))))


(add-hook 'typescript-ts-mode-hook #'eglot-ensure) ; Apparently people usually run `M-x eglot` manually. Absurd.
(add-hook 'lua-mode-hook #'eglot-ensure)
(add-hook 'nix-mode-hook #'eglot-ensure)
(add-hook 'go-ts-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'eglot-ensure)


;;; --- LINTING ---
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-flycheck-eglot-mode 1) ; Bridge eglot diagnostics into flycheck.

(setq-default flycheck-javascript-eslint-executable df/linter-eslint)
(setq-default flycheck-sh-shellcheck-executable df/linter-shellcheck)
(setq-default flycheck-lua-luacheck-executable df/linter-luacheck)

(keymap-set evil-normal-state-map "[ d" 'flycheck-previous-error)
(keymap-set evil-normal-state-map "] d" 'flycheck-next-error)


;;; --- COPILOT ---
(setq copilot-indent-offset-warning-disable t)
(add-hook 'prog-mode-hook 'copilot-mode)
(keymap-set evil-insert-state-map "C-j" #'copilot-accept-completion)


;;; --- CUSTOM FILETYPES ---
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-ts-mode))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook (lambda () (setq tab-width 4))) ; Set tab width to 4 spaces
(setq rust-mode-treesitter-derive t) ; Use tree-sitter grammar for syntax highlighting


;;; --- CUSTOM KEYBINDINGS ---
(keymap-set evil-normal-state-map "SPC b" 'counsel-buffer-or-recentf)


(defun df/search-project-files ()
  "Use `counsel-file-jump' to fuzzy-find a file within the current project."
  (interactive)
  (counsel-file-jump "" (projectile-project-root)))

(keymap-set evil-normal-state-map "SPC f" 'df/search-project-files)

(defun df/view-parent-directory ()
  "Opens the buffer's parent directory in Dired."
  (interactive)
  (let ((parent-dir (expand-file-name ".." (buffer-name))))
    (if (file-directory-p parent-dir)
  (dired parent-dir)
      (message (format "Not a directory: %s" parent-dir)))))

(keymap-set evil-normal-state-map "SPC [" 'df/view-parent-directory)
