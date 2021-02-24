;; evil mode

(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config ;; tweak evil after loading it
  (evil-mode)
  ;; convert key map for colemake
  (define-key evil-motion-state-map (kbd "h") 'evil-backward-char)
  (define-key evil-motion-state-map (kbd "n") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "e") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "i") 'evil-forward-char)
  (define-key evil-normal-state-map (kbd "i") 'evil-forward-char)
  (define-key evil-visual-state-map (kbd "i") 'evil-forward-char)
  (define-key evil-normal-state-map (kbd "s") 'evil-insert)
  (define-key evil-normal-state-map (kbd "t") 'evil-append)
  (define-key evil-visual-state-map (kbd "c") 'evil-yank)
  (define-key evil-normal-state-map (kbd "v") 'evil-paste-after)
  (define-key evil-normal-state-map (kbd "a") 'evil-visual-char)
  (define-key evil-normal-state-map (kbd "x") 'evil-delete)
  (define-key evil-normal-state-map (kbd "X") 'evil-delete-line)
  (define-key evil-normal-state-map (kbd "z") 'evil-undo)
  (define-key evil-normal-state-map (kbd "u") 'evil-forward-word-begin)
  (define-key evil-normal-state-map (kbd "l") 'evil-backward-word-begin)
  (define-key evil-motion-state-map (kbd "C-w h") 'evil-window-left)
  (define-key evil-motion-state-map (kbd "C-w n") 'evil-window-down)
  (define-key evil-motion-state-map (kbd "C-w e") 'evil-window-up)
  (define-key evil-motion-state-map (kbd "C-w i") 'evil-window-right)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
  )

(use-package evil-collection
  :config
  (evil-collection-init))

(message "evil inited!")
(provide 'evil-setup)
