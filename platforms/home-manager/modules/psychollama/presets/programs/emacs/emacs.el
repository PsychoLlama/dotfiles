(setq ring-bell-function 'ignore) ; disable annoying bell
(setq visible-cursor nil) ; don't blink the cursor
(menu-bar-mode -1) ; disable menu bar

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

; --- EVIL MODE ---

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
