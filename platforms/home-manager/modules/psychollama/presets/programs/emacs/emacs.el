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
(company-mode 1)

; --- S-EXPR EDITING ---

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)

; --- EVIL MODE ---

(global-undo-tree-mode)
(setq evil-undo-system 'undo-tree)
(setq undo-tree-history-directory-alist
      `(("." . ,(expand-file-name "undo-tree" user-emacs-directory))))

(setq evil-want-minibuffer t) ; use vim keybinds in the minibuffer
(setq evil-want-Y-yank-to-eol t) ; mirror nvim 0.10 `Y` behavior

; yay, evil!
(evil-mode 1)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(evil-terminal-cursor-changer-activate) ; fix terminal cursor modes

; evil plugins
(global-evil-surround-mode 1)
(evil-commentary-mode 1)

; --- TREESITTER INTEGRATION ---

(global-tree-sitter-mode 1)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

; --- CUSTOM KEYBINDINGS ---
(defun dotfiles-search-project-files ()
  "Uses `counsel-file-jump' to fuzzy-find a file within the current project."
  (interactive)
  (counsel-file-jump "" (projectile-project-root)))

(define-key evil-normal-state-map (kbd "SPC f") 'dotfiles-search-project-files)
