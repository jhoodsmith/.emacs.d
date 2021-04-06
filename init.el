;; Package Management
(require 'package)
(add-to-list 'package-archives 
               '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

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
(setq default-directory "~/")

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Show line number globally
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; UTF-8
(setq locale-coding-system 'utf-8) 
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Auto-close brackets and double quotes
;; (electric-pair-mode 1)

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

;; Silver searcher
(use-package ag)

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
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-frame)
(global-set-key (kbd "C-c s") 'vterm)
(global-set-key (kbd "C-c C-j") 'helm-imenu)

;; which-key

(use-package which-key
  :diminish
  :config (which-key-mode))

;; lsp-mode

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ((python-mode ruby-mode go-mode json-mode js-mode web-mode clojure-mode) . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; Go

(use-package go-mode)

;; Magit

(use-package magit
  :bind (("C-x g" . magit-status))
  :init
  ;; (gitmoji-commit-mode)
  (setq magit-auto-revert-mode nil)
  (setq magit-repository-directories
        '(;; Directory and depth to search
          ("~/git/"      . 1)
          ;; Specific project root directory
          ("~/.emacs.d/" . 0))))

(use-package gist)
(use-package forge
  :after magit)

;; Mode for .gitignore files.
(use-package gitignore-mode)

;; Swift
(use-package swift-mode)


;; Clojure
;; requires installation of clojure and leiningen
;; brew install clojure/tools/clojure
;; brew install leiningen
(use-package cider)

;; Themes
(use-package material-theme)
(load-theme 'material t)

; Web Mode
(use-package web-mode
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

(use-package cdlatex)

(use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'LaTeX-mode-hook #'cdlatex-mode)
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
;; (use-package js2-mode
;;   :mode ("\\.js\\'" "\\.json\\'")
;;   :interpreter "node"
;;   :config
;;   (setq js2-basic-offset 2)
;;   (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))

;; (use-package nodejs-repl)

;; (use-package tern
;;   :defer t
;;   :config
;;   (progn
;;     (add-hook 'js2-mode-hook 'tern-mode)))

;; (use-package company-tern
;;   :defer t
;;   :init
;;   (progn
;;     (require 'company)
;;     (add-to-list 'company-backends 'company-tern)))

;; ;; Elm
;; (use-package elm-mode
;;   :init
;;   (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
;;   (add-to-list 'company-backends 'company-elm)
;;   ; (elm-indent-mode false)
;;   (setq elm-indent-offset 2))


;; ;; Emojis
;; (use-package company-emoji
;;   :defer t
;;   :init
;;   (progn
;;     (require 'company)
;;     (add-to-list 'company-backends 'company-emoji)))


;; (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

(use-package unicode-fonts
   :ensure t
   :config
    (unicode-fonts-setup))

;; ;; Coffeescript
;; (use-package coffee-mode
;;   :defer t
;;   :mode "\\.coffee\\'")

;; SASS
(use-package scss-mode
  :defer t
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
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

;; Python
(use-package pyenv-mode)

;; (use-package elpy
;;   :defer 2
;;   :config
;;   (progn
;;     ;; Use Flycheck instead of Flymake
;;     (when (require 'flycheck nil t)
;;       (remove-hook 'elpy-modules 'elpy-module-flymake)
;;       (remove-hook 'elpy-modules 'elpy-module-yasnippet)
;;       (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
;;       (add-hook 'elpy-mode-hook 'flycheck-mode))
;;     (pyenv-mode)
;;     (elpy-enable)
;;     (define-key elpy-mode-map (kbd "C-c C-p") 'pytest-pdb-one)
;;     (define-key elpy-mode-map (kbd "C-c C-f") 'helm-projectile-find-file)
;;     (setq python-shell-interpreter "ipython"
;; 	  python-shell-interpreter-args "--simple-prompt --pprint"
;; 	  elpy-test-runner 'elpy-test-pytest-runner
;; 	  elpy-test-pytest-runner-command '("py.test" "--disable-warnings" "-x")
;; 	  elpy-rpc-backend "jedi")))

;; (use-package python-pytest
;;   :config
;;     (magit-define-popup-switch 'python-pytest-popup
;;       ?w "disable warning" "--disable-warnings")
;;     (magit-define-popup-switch 'python-pytest-popup
;;       ?s "no capture" "--capture=no"))


(use-package pip-requirements
  :mode ("/requirements.txt$" . pip-requirements-mode)
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package pytest)


;; Yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :defer nil
  :custom
  (yas-indent-line nil)
  :config
  (yas-global-mode))

(use-package yasnippet-snippets)

;; flycheck
(use-package flycheck
  :diminish
  :hook ((flycheck-mode . flymake-mode-off))
  :config (global-flycheck-mode))

;; flyspell
(use-package flyspell
  :diminish
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  :custom
  (ispell-program-name "aspell")
  (flyspell-issue-welcome-flag nil)
  :config
  (if (file-exists-p "/opt/homebrew/bin/aspell")
      (setq-default ispell-program-name "/opt/homebrew/bin/aspell")
    (setq-default ispell-program-name "/usr/local/bin/aspell")))


(use-package helm-flyspell)
(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)

;; Tramp
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
(setq tramp-default-method "ssh")
(use-package helm-tramp)

;; Kubernetes
(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

;; Ruby
(use-package ruby-mode
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Capfile$"
  :mode "Gemfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  :init
  (setq lsp-solargraph-use-bundler t))

(use-package rvm)
(use-package rspec-mode
  :init
  (setq rspec-use-spring-when-possible nil
	rspec-use-rvm t))

(use-package yaml-mode)

;; (use-package yari
;;   :init
;;   (add-hook 'ruby-mode-hook
;;             (lambda ()
;;               (local-set-key [f1] 'yari))))

(use-package inf-ruby
  :init
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(use-package rubocop
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  :diminish rubocop-mode)

;; (use-package robe
;;   :bind ("C-M-." . robe-jump)

;;   :init
;;   (add-hook 'ruby-mode-hook 'robe-mode)

;;   :config
;;   (defadvice inf-ruby-console-auto
;;     (before activate-rvm-for-robe activate)
;;     (rvm-activate-corresponding-ruby)))


;; Projectile for project file navigation
(use-package projectile
  :defer 1
  :diminish projectile-mode
  :config
  (projectile-mode)
  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  (setq projectile-sort-order (quote recently-active))
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos))))
  (projectile-save-known-projects))

;; Adds projectile helpers functions for Rails projects
(use-package projectile-rails
  :defer 1)

;; Moves selected region around.
(use-package drag-stuff
  :diminish drag-stuff-mode
  :bind (("M-<down>" . drag-stuff-down)
         ("M-<up>" . drag-stuff-up))
  :config
  (drag-stuff-global-mode))

;; Helm

(use-package helm
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
  (helm-mode 1))

(use-package helm-ag)

(use-package helm-dictionary
  :init
  (setq helm-dictionary-database "~/.emacs.d/en-es-enwiktionary.txt"))

;; (use-package helm-tramp
;;   :ensure t
;;   :defer t
;;   :bind (("C-c s" . helm-tramp))
;;   :init
;;   (setq tramp-default-method "ssh")
;;   )

(defun projectile-project-org-notes-file ()
  "If working within a projectile project, set notes file to be within project root. Otherwise use user default"
  (if (projectile-project-root)
      (expand-file-name "NOTES.org" (expand-file-name (projectile-project-name) "~/work/org"))
    "~/work/org/NOTES.org"))

(defun projectile-project-org-todo-file ()
  "If working within a projectile project, set TODO file to be within project root. Otherwise use user default"
  (if (projectile-project-root)
      (expand-file-name "TODO.org" (expand-file-name (projectile-project-name) "~/work/org"))
    "~/work/org/TODO.org"))

(defun make-directory-maybe (filename &rest args)
  "Wrapper around find-file to make file's directory if not exist"
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

(advice-add 'find-file :before #'make-directory-maybe)

(use-package helm-projectile
  :bind (("C-c p D" . projectile-dired)
         ("C-c p v" . projectile-vc)
         ("C-c p k" . projectile-kill-buffers)

         ("C-c p p" . helm-projectile-switch-project)
         ("C-c p f" . helm-projectile-find-file)
         ("C-c p F" . helm-projectile-find-file-in-known-projects)
         ("C-c p g" . helm-projectile-find-file-dwin)
         ("C-c p C-r" . helm-projectile-recentf)
         ("C-c p b" . helm-projectile-switch-to-buffer)
         ("C-c p s s" . helm-projectile-ag)
         ("C-c p s g" . helm-projectile-grep)
         ("C-c n" . (lambda () (interactive) (find-file (projectile-project-org-notes-file))))
         ("C-c t" . (lambda () (interactive) (find-file (projectile-project-org-todo-file))))
         )
  :diminish projectile-mode
  :init
  (setq projectile-keymap-prefix (kbd "C-c p")
        projectile-enable-caching t
        projectile-completion-system 'helm))


;; Company
(use-package company
  :diminish company-mode
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (global-company-mode t)
  (setq
   company-show-numbers t
   company-echo-delay 0
   company-idle-delay 0.2
   company-minimum-prefix-length 1
   company-tooltip-align-annotations t
   company-tooltip-limit 20))

(use-package company-box
  :after company
  :diminish company-box-mode
  :hook
  (company-mode . company-box-mode))

;; Org Mode

;; (use-package ob-ipython
;;   :config
;;   (setq indent-tabs-mode nil
;;         org-src-preserve-indentation nil
;;   ))

(use-package plantuml-mode
  :init
  (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2019.6/libexec/plantuml.jar"))


;; Start EIN using ONE of the following:
;; Open an .ipynb file, press C-c C-o, or,
;; M-x ein:run launches a jupyter process from emacs, or,
;; M-x ein:login to a running jupyter server, or,
;; M-x ein:login to any of
;; https://hub.data8x.berkeley.edu
;; https://hub.data8x.berkeley.edu/user/1dcdab3
;; https://hub.data8x.berkeley.edu/user/1dcdab3/?token=c421c68
(use-package ein
  :commands ein:run)

(use-package org-trello)

;; hugo org-mode integration
(use-package ox-hugo)

(use-package ob-restclient)

(use-package org
  :init
  (setq org-directory "~/work/org")
  :bind ("C-c c" . org-capture)
  :config
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (setq org-agenda-files '("~/work/org"))
  (setq org-src-fontify-natively t)
  ; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil)
  (setq org-capture-templates
	'(("n" "Note" entry (file+headline projectile-project-org-notes-file "Notes")
           "\n* %^{Title}\nEntered on %U\n%i\n%?")
	  ("t" "TODO" entry (file+headline projectile-project-org-todo-file "Tasks")
           "\n* TODO %^{Task}\nEntered on %U\n%?\n%i")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (plantuml . t)
     ;; (ipython . t)
     (ruby . t)
     (shell . t)
     (restclient . t)
     (sql . t)
     (js . t)
     (latex . t)
     (emacs-lisp . t)
     (ein . t))))

(defun my-insert-shell-prompt (_backend)
  (org-babel-map-src-blocks nil         ; nil implies current buffer
    (let (;; capture macro-defined variables
         (lang lang)
         (beg-body beg-body)
         (end-body end-body)
         ;; other variables
         (shell-langs '("sh" "shell"))
         (prefix "$ "))
      (when (member lang shell-langs)
        (goto-char beg-body)
        (skip-chars-forward "\n\s-" end-body)
        (while (< (point) end-body)
          (insert prefix)
          (end-of-line)
          (skip-chars-forward "\n\s-" end-body))))))

(add-hook 'org-export-before-parsing-hook #'my-insert-shell-prompt)

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "FACTSET_PASSWORD" "AWS_SECRET_ACCESS_KEY"))
  (exec-path-from-shell-initialize))

;; (when (display-graphic-p)
;;   (toggle-frame-maximized))

;; Ledger mode
(use-package ledger-mode)

(use-package json-mode
  :ensure t)

;; Restclient
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :bind (:map restclient-mode-map
	      ("C-c C-f" . json-mode-beautify)))

;; Vterm
(use-package vterm
  :ensure t)


;; Keep emacs Custom-settings in separate file.
(setq custom-file "~/.emacs.d/custom.el")
(when (not (file-exists-p custom-file))
  (with-temp-buffer (write-file custom-file)))

(load custom-file)

;; Server
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
	    (when server-buffer-clients
	      (local-set-key (kbd "C-x k") 'server-edit))))

(setq sql-postgres-program "/usr/local/bin/psql")

(setenv "PATH"
        (concat
         "/usr/local/texlive/2019/bin/x86_64-darwin" ":"
         (getenv "PATH")))
