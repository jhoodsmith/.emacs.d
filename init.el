;; Package Management
(require 'package)
(add-to-list 'package-archives 
               '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
;; Don't need to check every time.
;; (when (not package-archive-contents)
;;   (package-refresh-contents))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(eval-when-compile
  (require 'use-package))

;; User Details
(setq user-full-name "James Hood-Smith")
(setq user-mail-address "james@hood-smith.co.uk")
(setq ispell-dictionary "british")


;; Start-Up Options
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq echo-keystrokes 0.1)
(setq use-dialog-box nil)
(setq ring-bell-function 'ignore)
(setq visible-bell nil)
(tooltip-mode -1)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; UTF-8
(setq locale-coding-system 'utf-8) 
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Auto-close brackets and double quotes
(electric-pair-mode 1)

;; Marking Text
(delete-selection-mode t)
(transient-mark-mode t)
(setq select-enable-clipboard t)
(setq-default fill-column 80)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Smooth Scroll:
(setq mouse-wheel-scroll-amount '(1 ((shift) .1))) ;; one line at a time

;; Scrol one line when hitting bottom of window
(setq scroll-conservatively 10000)

;; Sentences do not need double spaces to end.
(set-default 'sentence-end-double-space nil)

;; No backup or lock files
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; No .saves files
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix
      "~/.emacs.d/.cache/auto-save-list/.saves-")

;; eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Allow clipboard from outside emacs
(setq x-select-enable-clipboard t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

;; History
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Key Bindings
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(global-set-key (kbd "C-x e") 'server-edit)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-frame)

;; Shows a popup with all the possible key bindings that would complete the
;; started binding.
(use-package guide-key
  :ensure t
  :defer 4
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence t)
    (setq guide-key/popup-window-position :bottom)
    (setq guide-key/idle-delay 0.4)
    (guide-key-mode 1)))

;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :init
  (setq magit-auto-revert-mode nil)
  :config
  (setq magit-repository-directories
        '(;; Directory and depth to search
          ("~/git/"      . 1)
          ;; Specific project root directory
          ("~/.emacs.d/" . 0))))

;; Mode for .gitignore files.
(use-package gitignore-mode
  :ensure t)

;; Themes
(use-package material-theme)
(load-theme 'material t)

; Web Mode
(use-package web-mode
  :ensure t
  :bind (("C-c C-v" . browse-url-of-buffer)
         ("C-c w t" . web-mode-element-wrap))
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.html?" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode)))
  :config
  (progn
    ;; Set tab to 4 to play nice with old editors
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4)))

;; LaTeX
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil
          TeX-PDF-mode t
          LaTeX-electric-left-right-brace t
          TeX-electric-sub-and-superscript t
          TeX-insert-braces nil)
    (setq-default TeX-master nil))
  :config
  (add-hook 'plain-TeX-mode-hook
            (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                            (cons "$" "$"))))
  (add-hook 'LaTeX-mode-hook
            (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                            (cons "$" "$")))))

;; Javascript
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" "\\.json\\'")
  :interpreter "node"
  :config
  (setq js2-basic-offset 2)
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))

(use-package tern
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'js2-mode-hook 'tern-mode)))

(use-package company-tern
  :ensure t
  :defer t
  :init
  (progn
    (require 'company)
    (add-to-list 'company-backends 'company-tern)))

;; Coffeescript
(use-package coffee-mode :ensure t :defer t
	     :mode "\\.coffee\\'")

;; SASS
(use-package scss-mode
  :ensure t :defer t
  :mode ("\\.scss\\'" . scss-mode)
  :init
  (setq scss-compile-at-save nil))

;; Rainbow
(use-package rainbow-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))


;; Python

;; Standard Jedi.el setting
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Use Company for auto-completion interface.
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(use-package company-jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'my/python-mode-hook))

(use-package elpy
  :ensure t
  :defer 2
  :config
  (progn
    ;; Use Flycheck instead of Flymake
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
      (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    (elpy-enable)
    (define-key elpy-mode-map (kbd "C-c C-f") 'helm-projectile-find-file)
    (setq python-shell-interpreter "ipython"
	  python-shell-interpreter-args "-i --simple-prompt"
	  elpy-test-pytest-runner-command '("py.test" "-x" "-s")
	  elpy-rpc-backend "jedi")))

(use-package py-test
    :ensure t
    :demand t)

;; flycheck
(use-package flycheck
  :ensure t
  :defer 2
  :diminish flycheck-mode " ✓"
  :commands global-flycheck-mode
  :config
  (progn
    (global-flycheck-mode 1)
    (setq-default flycheck-disabled-checkers
                  '(html-tidy
                    emacs-lisp-checkdoc))))

;; flyspell
(use-package flyspell
  :defer 2
  :init
  :config
  (progn
    (setq ispell-program-name "aspell")
    (setq flyspell-issue-welcome-flag nil)
    (if (eq system-type 'darwin)
	(setq-default ispell-program-name "/usr/local/bin/aspell")
      (setq-default ispell-program-name "/usr/bin/aspell"))
    (add-hook 'text-mode-hook 'flyspell-mode)))

(use-package define-word)

;; Tramp
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

;; Ruby
(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Capfile$"
  :mode "Gemfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  :interpreter "ruby"

  :init
  (setq ruby-indent-level 2
	ruby-insert-encoding-magic-comment nil
        ruby-indent-tabs-mode nil)
  (add-hook 'ruby-mode 'superword-mode)

  :bind
  (([(meta down)] . ruby-forward-sexp)
   ([(meta up)]   . ruby-backward-sexp)
   (("C-c C-e"    . ruby-send-region))))

(use-package rvm)

(use-package yari
  :ensure t
  :init
  (add-hook 'ruby-mode-hook
            (lambda ()
              (local-set-key [f1] 'yari))))

(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(use-package rubocop
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  :diminish rubocop-mode)

(use-package robe
  :ensure t
  :bind ("C-M-." . robe-jump)

  :init
  (add-hook 'ruby-mode-hook 'robe-mode)

  :config
  (defadvice inf-ruby-console-auto
    (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby)))


;; Projectile for project file navigation
(use-package projectile
  :defer 1
  :diminish projectile-mode
  :after magit
  :config
  (projectile-mode)
  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  (setq projectile-sort-order (quote recently-active))
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  (mapc #'projectile-add-known-project
        (mapcar #'file-name-as-directory (magit-list-repos)))
  (projectile-save-known-projects)))
  )

;; Adds projectile helpers functions for Rails projects
(use-package projectile-rails
  :defer 1)

;; Revelas the current file in Finder.app.
(use-package reveal-in-osx-finder
  :ensure t
  :if (eq system-type 'darwin))

;; Moves selected region around.
(use-package drag-stuff
  :ensure t
  :diminish drag-stuff-mode
  :bind (("M-<down>" . drag-stuff-down)
         ("M-<up>" . drag-stuff-up))
  :config
  (drag-stuff-global-mode))

;; Helm

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
	 ("M-y" . 'helm-show-kill-ring)
         ("C-x b" . helm-buffers-list))
  :init
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
	helm-split-window-inside-p t)
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . helm-completing-read-symbols)))

(use-package helm-ag
  :ensure t)

(use-package helm-tramp
  :ensure t
  :defer t)

(use-package helm-projectile
  :ensure t
  :bind (("C-c p D" . projectile-dired)
         ("C-c p v" . projectile-vc)
         ("C-c p k" . projectile-kill-buffers)

         ("C-c p p" . helm-projectile-switch-project)
         ("C-c p f" . helm-projectile-find-file)
         ("C-c p F" . helm-projectile-find-file-in-known-projects)
         ("C-c p g" . helm-projectile-find-file-dwin)
         ("C-c p d" . helm-projectile-find-dir)
         ("C-c p C-r" . helm-projectile-recentf)
         ("C-c p b" . helm-projectile-switch-to-buffer)
         ("C-c p s s" . helm-projectile-ag)
         ("C-c p s g" . helm-projectile-grep)
         )
  :diminish projectile-mode
  :init
  (setq projectile-keymap-prefix (kbd "C-c p")
        projectile-enable-caching t
        projectile-completion-system 'helm))

;; Company
(use-package company
  :ensure t
  :diminish ""
  :config
  (global-company-mode 1)
  (push 'company-robe company-backends)
  (setq
   company-show-numbers t
   company-echo-delay 0
   company-idle-delay 0.2
   company-minimum-prefix-length 1
   company-tooltip-align-annotations t
   company-tooltip-limit 20))


(use-package w3m
  :disabled t
  :commands (w3m-search w3m-find-file)
  :bind (("C-. u"   . w3m-browse-url)
         ("C-. U"   . w3m-browse-url-new-session))
  :init
  (setq w3m-command "w3m")

  (setq w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8))


;; Org Mode
(use-package org
	     :ensure t
	     :init
	     (setq org-directory "~/org"))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "WORKON_HOME"))

(when (display-graphic-p)
  (toggle-frame-maximized))


;; Keep emacs Custom-settings in separate file.
(setq custom-file "~/.emacs.d/custom.el")
(when (not (file-exists-p custom-file))
  (with-temp-buffer (write-file custom-file)))
(load custom-file)
