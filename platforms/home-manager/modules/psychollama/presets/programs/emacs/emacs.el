(setq ring-bell-function 'ignore) ; disable annoying bell
(setq visible-cursor nil) ; don't blink the cursor
(menu-bar-mode -1) ; disable menu bar

(load-theme 'atom-one-dark t)
(set-face-background 'default "unspecified-bg") ; transparent background

; yay, evil!
(evil-mode 1)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(evil-terminal-cursor-changer-activate) ; fix terminal cursor modes

(global-evil-surround-mode 1)
(evil-commentary-mode 1)

(projectile-mode 1)
(counsel-mode 1)
(company-mode 1)

(global-tree-sitter-mode 1)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
