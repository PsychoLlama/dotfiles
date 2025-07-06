(setq ring-bell-function 'ignore) ; Disable annoying bell.
(setq visible-cursor nil) ; Don't blink the cursor.
(blink-cursor-mode 0) ; Seriously, don't blink the cursor.
(menu-bar-mode -1) ; Disable menu bar.

(setq scroll-conservatively 10000) ; Scroll without jumping.
(setq indent-tabs-mode nil) ; Use spaces as the default.
(setq tab-width 2) ; Set the default tab width to 2 spaces.
(setq show-paren-delay 0) ; Show matching parentheses immediately.

(defun display-startup-echo-area-message ()) ; Override echo message on startup.
(setq inhibit-startup-screen t) ; Don't show the startup screen.
(setq initial-scratch-message nil) ; Don't show scratch buffer hints.
(setq server-client-instructions nil) ; Suppress emacsclient startup help message.

(line-number-mode 1) ; Show line numbers in source code.
(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode)
                            (custom-set-faces ; Make the line number column's background transparent
                             '(line-number ((t (:background nil))))
                             '(line-number-current-line ((t (:background nil)))))))

(setq select-enable-clipboard nil) ; Prefer xclip to sync the kill ring with the system clipboard.
(xclip-mode 1)

(save-place-mode 1) ; Save the cursor position in files.
(savehist-mode 1) ; Save minibuffer history.

(electric-pair-mode 1) ; Automatically close brackets and quotes.

(defconst my/emacs-auto-save-directory ; Don't store backup files in the same directory.
  (expand-file-name "autosave" user-emacs-directory))

(setq auto-save-file-name-transforms
      `((".*" ,(concat my/emacs-auto-save-directory "\\1") t)))


;;; --- THEMEING ---
(load-theme 'doom-one t)
(if (display-graphic-p)
    (progn ; Remove chrome from the UI
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (tooltip-mode -1))
  (set-face-background 'default "unspecified-bg")) ; Use a transparent background in terminal mode

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode) ; Fancy delimiters.


;;; --- PROJECT MANAGEMENT ---
(projectile-mode 1)
(setq projectile-project-search-path '(("~/projects/" . 2)))
(setq projectile-switch-project-action #'projectile-dired)


;;; --- PICKERS/NAVIGATION ---
(counsel-mode 1)
(setq counsel-projectile-switch-project-action
      '(1 ("D" counsel-projectile-switch-project-action-dired "open Dired")))


;;; --- STRUCTURAL EDITING ---
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)

(defun my/paredit-RET ()
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
  (keymap-set paredit-mode-map "RET" #'my/paredit-RET))


;;; --- EVIL MODE ---
(defconst my/emacs-undo-tree-directory
  (expand-file-name "undo-tree" user-emacs-directory))

(global-undo-tree-mode)
(setq evil-undo-system 'undo-tree)
(setq undo-tree-history-directory-alist
      `(("." . ,my/emacs-undo-tree-directory)))

(setq evil-want-keybinding nil) ; Extra modes are handled by `evil-collection`.
(setq evil-want-minibuffer t) ; Use vim keybinds in the minibuffer.
(setq evil-want-Y-yank-to-eol t) ; Mirror nvim 0.10 `Y` behavior.
(setq evil-search-wrap nil) ; Basically `&nowrapscan`.
(setq evil-search-module 'evil-search) ; Replace `isearch' with `evil-search'.

(evil-mode 1) ; yay, evil!

(require 'evil-core) ; Provides `evil-define-key`.
(evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up)
(evil-terminal-cursor-changer-activate) ; Fixes terminal cursor modes

(global-evil-surround-mode 1)
(evil-commentary-mode 1)
(evil-collection-init)


;;; --- FILE BROWSING ---
(add-hook 'dired-mode-hook
 (lambda ()
   (evil-collection-define-key 'normal 'dired-mode-map
     (kbd "SPC") nil ; Turn off Dired's `SPC` keymap. It breaks leader bindings.
     "h" 'dired-up-directory
     "l" 'dired-find-file
     "i" 'dired-create-empty-file
     "a" 'dired-create-directory
     "f" 'counsel-fd-file-jump
     "t" 'counsel-fd-dired-jump)))

(defalias 'dired-x-find-file 'dired) ; Expected by `counsel-fd-dired-jump'.

(evil-define-key 'normal 'global (kbd "SPC [") 'dired-jump) ; Open parent dir of buffer.
(evil-define-key 'normal 'global (kbd "SPC z") 'counsel-projectile-switch-project)

(diredfl-global-mode 1) ; Exa-style highlighting in dired.

(add-hook 'dired-mode-hook 'dired-hide-details-mode) ; Only show file names.
(setq dired-listing-switches "-lAh --group-directories-first")
(setq dired-free-space nil) ; Don't show disk space.


;;; --- COMPLETION ENGINE ---
(setq company-idle-delay 0)
(add-hook 'after-init-hook #'global-company-mode)


;;; --- AUTO-FORMATTING ---
(apheleia-global-mode 1)

(setq apheleia-formatters		; Map names to shell commands
      `((prettier . (,my/formatter-prettier filepath))
        (eslint . (,my/formatter-eslint
                   "--stdin-filename" filepath
                   "--stdin"
                   "--fix-to-stdout"))

        (nixfmt . (,my/formatter-nixfmt "--quiet"))
        (stylua . (,my/formatter-stylua
                   "--stdin-filepath" filepath
                   "--search-parent-directories"
                   "--allow-hidden"
                   "-"))

        (rustfmt . ("rustfmt" "--emit=stdout"))
        (gofmt . ("gofmt"))))

(setq apheleia-mode-alist	       ; Map major modes to formatters
      '((markdown-mode . (prettier))
        (css-mode . (prettier))
        (less-css-mode . (prettier))
        (html-mode . (prettier))
        (typescript-ts-mode . (prettier eslint))
        (rust-mode . (rustfmt))
        (yaml-ts-mode . (prettier))
        (json-ts-mode . (prettier))
        (nix-ts-mode . (nixfmt))
        (lua-ts-mode . (stylua))
        (go-ts-mode . (gofmt))))


;;; --- LSP INTEGRATION ---
(setq eldoc-idle-delay 0.1) ; Show symbol information quickly.
(setq eglot-server-programs
      `((nix-ts-mode . (,my/lsp-nil :initializationOptions
                                 (:nil (:nix (:flake (:autoArchive t))))))
        (lua-ts-mode . (,my/lsp-luals :initializationOptions
                                   (:Lua (:format (:enable :json-false)
                                                  :workspace (:checkThirdParty :json-false)
                                                  :addonManager (:enable :json-false)))))
        (typescript-ts-mode . (,my/lsp-tsserver "--stdio"))
        (json-ts-mode . (,my/lsp-jsonls "--stdio"))
        (go-ts-mode . (,my/lsp-gopls "-remote=auto"))
        (rust-mode . (,my/lsp-rust-analyzer))
        (nushell-ts-mode . ("nu" "--lsp"))))

(add-hook 'go-ts-mode-hook #'eglot-ensure)
(add-hook 'json-ts-mode-hook #'eglot-ensure)
(add-hook 'lua-ts-mode-hook #'eglot-ensure)
(add-hook 'nix-ts-mode-hook #'eglot-ensure)
(add-hook 'nushell-ts-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)

(evil-define-key 'normal prog-mode-map (kbd "SPC r n") 'eglot-rename) ; Rename symbol under cursor.


;;; --- LINTING ---
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-flycheck-eglot-mode 1) ; Bridge eglot diagnostics into flycheck.

(setq-default flycheck-javascript-eslint-executable my/linter-eslint)
(setq-default flycheck-sh-shellcheck-executable my/linter-shellcheck)
(setq-default flycheck-lua-luacheck-executable my/linter-luacheck)

(setq-default flycheck-disabled-checkers
	      '(emacs-lisp-checkdoc emacs-lisp)) ; Too many false errors.

(evil-define-key 'normal prog-mode-map (kbd "[ d") 'flycheck-previous-error)
(evil-define-key 'normal prog-mode-map (kbd "] d") 'flycheck-next-error)


;;; --- COPILOT ---
(setq copilot-indent-offset-warning-disable t)
(setq copilot-max-char-warning-disable t)
(add-hook 'prog-mode-hook 'copilot-mode)
(evil-define-key 'insert prog-mode-map (kbd "C-j") #'copilot-accept-completion)


;;; --- LLMS ---
(setq gptel-api-key (getenv "OPENAI_API_KEY"))
(gptel-make-anthropic "Claude" :stream t :key (getenv "ANTHROPIC_API_KEY"))
(gptel-make-gemini "Gemini" :stream t :key (getenv "GEMINI_API_KEY"))
(gptel-make-gh-copilot "Copilot")

(evil-define-key 'normal 'global (kbd "SPC c") 'gptel)
(evil-define-key 'normal 'gptel-mode-map (kbd "g ?") 'gptel-menu)


;;; --- ENVIRONMENT ---
(direnv-mode 1)
(setq direnv-always-show-summary nil) 


;;; --- CUSTOM FILETYPES ---
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
(add-to-list 'auto-mode-alist '("\\.nu\\'" . nushell-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\`[Jj]ustfile\\'" . just-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(dolist (ext '("json" "lock" "jsonc" "json5" "ndjson"))
  (add-to-list 'auto-mode-alist (cons (concat "\\." ext "\\'") 'json-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook (lambda () (setq tab-width 4))) ; Set tab width to 4 spaces
(setq rust-mode-treesitter-derive t) ; Use tree-sitter grammar for syntax highlighting


;;; --- AI INTEGRATIONS ---
(evil-define-key 'normal 'global (kbd "SPC a") 'aidermacs-transient-menu)
(setq aidermacs-default-chat-mode 'ask)


;;; --- CUSTOM KEYBINDINGS ---
(defun my/dired-project-root ()
  "Open the project's root directory in Dired."
  (interactive)
  (dired (projectile-project-root)))

(evil-define-key 'normal 'global (kbd "SPC b") 'counsel-buffer-or-recentf)
(evil-define-key 'normal 'global (kbd "SPC f") 'counsel-projectile-find-file)
(evil-define-key 'normal 'global (kbd "SPC g") 'magit-status)
(evil-define-key 'normal 'global (kbd "SPC p") 'my/dired-project-root)

; Clear next/prev bindings in evil mode for the minibuffer.
(eval-after-load "evil-maps"
  (dolist (map '(evil-motion-state-map
		 evil-insert-state-map
		 evil-emacs-state-map))
    (define-key (eval map) "\C-n" nil)
    (define-key (eval map) "\C-p" nil)))
