(defvar normal-global-leader-key "SPC")
(defvar non-normal-leader-key "<f18>")
(defvar normal-major-mode-leader-key ",")
(defvar non-normal-major-mode-leader-key "<f19>")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t) 
  (setq use-package-always-defer nil)
  (setq use-package-always-demand t)
  (setq use-package-expand-minimally t)
  (setq use-package-verbose t))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(package-install 'auctex)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  )

(defun herl/evil-colemak-setup ()
  (interactive)
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
  (herl/evil-colemak-setup)
  )

(use-package evil-collection
  :config
  (evil-collection-init))

(message "evil inited!")

(use-package helm
  :config
  (require 'helm-config)
  (helm-autoresize-mode 1)
  (setq helm-autoresize-max-height 50)
  (setq helm-autoresize-min-height 15)
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x C-b" . helm-mini)
  ("M-y" . helm-show-kill-ring)
  )

(message "helm inited!")

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-secondary-delay 0.5)
  )
(message "which key inited!")

(use-package general :ensure t
  :config
  (defun herl-open-emacs-org()
    (interactive)
    (find-file "~/.emacs.d/Emacs.org"))
  (general-override-mode)
  (general-evil-setup)
  (general-create-definer global-leader
    :states '(normal insert emacs visual)
    :prefix normal-global-leader-key
    :non-normal-prefix non-normal-leader-key)
  (general-create-definer major-leader
    :states '(normal insert emacs visual)
    :prefix normal-major-mode-leader-key
    :non-normal-prefix non-normal-major-mode-leader-key)
  (general-nmap "SPC m" (general-simulate-key "," :which-key "major mode"))
  (global-leader 
    "f" '(:ignore t :which-key "file")
    "ff" '(helm-find-files :which-key "open file")
    "fs" '(save-buffer :which-key "save")
    "fr" '(helm-recentf :which-key "recent file")
    "fd" '(:ignore t :which-key "dot files")
    "fde" '(herl-open-emacs-org :which-key "open Emacs.org")
    "w" '(:ignore t :which-key "window")
    "ww" '(other-window :which-key "other window")
    "wh" '(evil-window-left :which-key "window below")
    "wn" '(evil-window-down :which-key "window down")
    "we" '(evil-window-up :which-key "window up")
    "wi" '(evil-window-right :which-key "window right")
    "w/" '(split-window-right :which-key "split right")
    "w-" '(split-window-below :which-key "split below")
    "wd" '(delete-window :which-key "delete window")
    "wm" '(delete-other-windows :which-key "maxmize")
    "b" '(:ignore t :which-key "buffer")
    "bb" '(helm-mini :which-key "buffer list")
    "bn" '(next-buffer :which-key "next buffer")
    "be" '(previous-buffer :which-key "previous buffer")
    "q" '(save-buffers-kill-terminal :which-key "quit")
    "SPC" '(helm-M-x :which-key "M-x")
    )
  )

(message "general inided!")

(use-package winum
  :config
  (winum-mode)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-m"
   "1" '(winum-select-window-1 :which-key "window 1")
   "2" '(winum-select-window-2 :which-key "window 2")
   "3" '(winum-select-window-3 :which-key "window 3")
   "4" '(winum-select-window-4 :which-key "window 4")
   "5" '(winum-select-window-5 :which-key "window 5")
   "6" '(winum-select-window-6 :which-key "window 6")
   )
  )

(message "winum inided!")

(use-package treemacs-evil
  :config
  (defun herl-close-tree ()
    "close tree view"
    (interactive)
    (treemacs-select-window)
    (treemacs-quit)
    )
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-m"
   "ft" '(:ignore t :which-key "file tree")
   "ftt" '(treemacs :which-key "open file tree")
   "fts" '(treemacs-select-window :which-key "switch to tree")
   "ftq" '(herl-close-tree :which-key "quit tree")
   )
  )
(message "treemacs inited!")

(quelpa '(yasnippet :fetcher git :url "https://github.com/HerculeWu/yasnippet.git"))
(yas-global-mode)

(message "yasnippet inited!")

(use-package magit)
(global-leader
  "g" '(:ignore t :which-key "git")
  "gg" '(magit-status :which-key "status")
  "gs" '(magit-stage-file :which-key "stage")
  "gu" '(magit-unstage-file :which-key "unstage")
  )
(dolist (state '(normal motion))
  (evil-define-key state with-editor-mode-map
    (concat normal-major-mode-leader-key normal-major-mode-leader-key) 'with-editor-finish
    (concat normal-major-mode-leader-key "c") 'with-editor-finish
    (concat normal-major-mode-leader-key "k") 'with-editor-cancel
    )
  )

(message "magit inited!")

(use-package lsp-mode)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(setq lsp-completion-provider :capf)
(setq lsp-idle-delay 0.500)
(setq lsp-auto-guess-root nil)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-position 'at-point)
(global-leader
  "l" '(:ignore t :which-key "lsp")
  "l=" '(:ignore t :which-key "format")
  "lf" '(:ignore t :which-key "workspace")
  "lg" '(:ignore t :which-key "goto")
  "lt" '(:ignore t :which-key "tree")
  "la" '(:ignore t :which-key "action")
  "lh" '(:ignore t :which-key "help")
  "lr" '(:ignore t :which-key "rename")
  "ls" '(:ignore t :which-key "server")
  "lm" '(lsp-ui-imenu :which-key "imenu")
  "laa" '(lsp-execute-code-action :which-key "execute code action")
  "lah" '(lsp-document-highlight :which-key "highlight")
  "lro" '(lsp-organize-imports :which-key "organize imports")
  "lrr" '(lsp-rename :which-key "rename")
  "lhg" '(lsp-ui-doc-glance :which-key "doc glance")
  "lhh" '(lsp-describe-thing-at-point :which-key "describe thing at point")
  "lhs" '(lsp-signature-activate :which-key "signature activate")
  "lge" '(lsp-treemacs-errors-list :which-key "treemacs errors list")
  "lgg" '(lsp-find-definition :which-key "defination")
  "lgh" '(lsp-treemacs-call-hierarchy :which-key "treemacs call hierarchy")
  "lgr" '(lsp-find-references :which-key "references")
  "ltd" '(lsp-modeline-diagnostics-mode :which-key "modeline diagnostics mode")
  "ltl" '(lsp-toggle-trace-io :which-key "toggle trace io")
  "lts" '(lsp-ui-sideline-mode :which-key "ui sideline mode")
  "ltt" '(lsp-treemacs-sync-mode :which-key "treemacs sync mode")
  "lta" '(lsp-modeline-code-actions-mode :which-key "modeline code actions mode")
  "ltb" '(lsp-headerline-breadcrumb-mode :which-key "headerline breadcrumb mode")
  "ltd" '(lsp-ui-doc-mode :which-key "ui doc mode")
  "lth" '(lsp-toggle-symbol-highlight :which-key "toggle symbol highlight")
  "ltl" '(lsp-lens-mode :which-key "lens mode")
  "lts" '(lsp-toggle-signature-auto-activate :which-key "toggle signature auto activate")
  "lfa" '(lsp-workspace-folders-add :which-key "folders add")
  "lfb" '(lsp-workspace-blacklist-remove :which-key "blacklist remove")
  "lfr" '(lsp-workspace-folders-remove :which-key "folders remove")
  "l==" '(lsp-format-buffer :which-key "buffer")
  "l=r" '(lsp-format-region :which-key "region")
  "lsd" '(lsp-disconnect :which-key "disconnect")
  "lsd" '(lsp-describe-session :which-key "describe session")
  "lsq" '(lsp-workspace-shutdown :which-key "workspace shutdown")
  "lsr" '(lsp-workspace-restart :which-key "workspace restart")
  "lss" '(lsp :which-key "start/restart")
)

(message "lsp inited!")

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'bitmap))

(message "highlight-indent-guides inited!")

(use-package info-colors 
  :ensure t 
  :hook ('Info-selection-hook . 'info-colors-fontify-node))

(message "info-colors inited!")

(use-package nyan-mode
  :ensure t
  :hook (after-init . nyan-mode)
  :config
  (setq nyan-wavy-trail t
		nyan-animate-nyancat t))

(message "cat is with you now!")

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dired
  :ensure nil)
(use-package all-the-icons-dired)

(use-package git-gutter
  :config
  (global-git-gutter-mode +1)
  (custom-set-variables
   '(git-gutter:update-interval 1)
   '(git-gutter:added-sign "+")
   '(git-gutter:deleted-sign "-")
   '(git-gutter:modified-sign "~")
   ))

(use-package org
  :config
  (setq org-ellipsis " ▼")
  (setq org-hide-emphasis-markers t)
  )
(use-package evil-org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
(add-hook 'org-mode-hook
	  (lambda ()
	    (progn
	      (evil-org-mode)
	      (org-bullets-mode)
	      (herl/evil-colemak-setup)
	      (major-leader 'org-mode-map
		"," 'org-ctrl-c-ctrl-c
		"*" 'org-ctrl-c-star
		"*" 'org-ctrl-c-star
		"RET" 'org-ctrl-c-ret
		"-" 'org-ctrl-c-minus
		"'" 'org-edit-special
		"^" 'org-sort
		"/" 'org-sparse-tree
		"." 'org-time-stamp
		"!" 'org-time-stamp-inactive
		"a" 'org-agenda
		"b" 'org-tree-to-indirect-buffer
		"A" 'org-archive-subtree
		"c" 'org-capture
		"C" 'evil-org-recompute-clocks
		"d" 'org-deadline
		"D" 'org-insert-drawer
		"e" 'org-export-dispatch
		"f" 'org-set-effort
		"K" 'org-clock-in
		"l" 'org-open-at-point
		"O" 'org-clock-out
		"P" 'org-set-property
		"q" 'org-clock-cancel
		"R" 'org-refile
		"s" 'org-schedule
		"T" 'org-show-todo-tree
		"I" 'org-shiftright
		"H" 'org-shiftleft
		"E" 'org-shiftup
		"N" 'org-shiftdown
		"B" '(:ignore t :wk "babel")
		"B I" 'org-babel-view-src-block-info
		"B a" 'org-babel-sha1-hash
		"B b" 'org-babel-execute-buffer
		"B c" 'org-babel-check-src-block
		"B d" 'org-babel-demarcate-block
		"B e" 'org-babel-execute-maybe
		"B f" 'org-babel-tangle-file
		"B g" 'org-babel-goto-named-src-block
		"B h" 'org-babel-describe-bindings
		"B i" 'org-babel-lob-ingest
		"B j" 'org-babel-insert-header-arg
		"B k" 'org-babel-remove-result-one-or-many
		"B l" 'org-babel-load-in-session
		"B n" 'org-babel-next-src-block
		"B o" 'org-babel-open-src-block-result
		"B p" 'org-babel-previous-src-block
		"B r" 'org-babel-goto-named-result
		"B s" 'org-babel-execute-subtree
		"B t" 'org-babel-tangle
		"B u" 'org-babel-goto-src-block-head
		"B v" 'org-babel-expand-src-block
		"B x" 'org-babel-do-key-sequence-in-edit-buffer
		"B z" 'org-babel-switch-to-session-with-code
		)
	      )))

(defun herl/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . herl/org-mode-visual-fill))

(defun org-babel-auto-tangle ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-auto-tangle)))

(message "org inited!")

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(defun latex/change-env ()
  (interactive)
  (LaTeX-environment t))
(defun latex/font-bold () (interactive) (TeX-font nil ?\C-b))
(defun latex/font-code () (interactive) (TeX-font nil ?\C-t))
(defun latex/font-emphasis () (interactive) (TeX-font nil ?\C-e))
(defun latex/font-italic () (interactive) (TeX-font nil ?\C-i))
(defun latex/font-clear () (interactive) (TeX-font nil ?\C-d))
(defun latex/font-oblique () (interactive) (TeX-font nil ?\C-s))

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (progn
	      (major-leader 'LaTeX-mode-map
		"p" '(:ignore t :wk "preview")
		"p e" '(preview-environment :wk "environment")
		"p l" '(preview-region :wk "region")
		"p s" '(preview-section :wk "section")
		"p p" '(preview-at-point :wk "at point")
		"p c" '(:ignore t :wk "clear preview")
		"p c b" '(preview-clearout-buffer :wk "buffer")
		"p c s" '(preview-clearout-section :wk "section")
		"p c p" '(preview-clearout-at-point :wk "at point")
		"e" '(:ignore t :wk "environment")
		"e i" '(LaTeX-environment :wk "insert")
		"e c" '(latex/change-env :wk "change")
		"f" '(:ignore t :wk "insert font")
		"fb"  '(latex/font-bold :wk "bold")
		"fc"  '(latex/font-code :wk "code")
		"fe"  '(latex/font-emphasis :wk "emphasis")
		"fi"  '(latex/font-italic :wk "italic")
		"fr"  '(latex/font-clear :wk "clear")
		"fo"  '(latex/font-oblique :wk "oblique")
		)
	    )
	  )
 )

(message "latex inited!")

(use-package python-mode
  :hook (python-mode . lsp)
  :custom
  (python-shell-interpreter "python3"))
(use-package py-isort)

(use-package julia-mode)

(quelpa '(lsp-julia :fetcher github
		    :repo "non-Jedi/lsp-julia"
		    :files (:defaults "languageserver")))

(add-hook 'julia-mode-hook #'lsp)

(message "Julia inited!")

(global-leader
  "a" '(:ignore t :wk "app")
  "as" '(:ignore t :wk "shell")
  "ase" '(eshell :wk "eshell")
  "asv" '(vterm :wk "vterm")
  )

(message "vterm inited")

(defun herl/open-google ()
  (interactive)
  (xwidget-webkit-browse-url "https://www.google.com" t)
  )
(global-leader
  "aw" '(:ignore t :wk "webkit")
  "awg" '(herl/open-google :wk "google")
  "awu" '(xwidget-webkit-browse-url :wk "open url")
  )

(defun herl/webkit-open-local-file (fpath)
  (interactive "fEnter file path: ")
  (when (member (substring fpath -4 nil) '("html" ".pdf"))
    (xwidget-webkit-browse-url
     (concat "file://" (expand-file-name fpath)))
    )
  )

(global-leader
  "awf" '(herl/webkit-open-local-file :wk "open loacl html"))
