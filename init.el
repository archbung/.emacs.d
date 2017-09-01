;; set font
(set-face-attribute 'default nil :font "Iosevka 10")

;; global variables
(setq inhibit-startup-screen t
      create-lockfiles nil
      make-backup-files nil
      column-number-mode t
      scroll-error-top-bottom t
      show-paren-delay 0.5
      use-package-always-ensure t
      sentence-end-double-space nil
      custom-file "~/.emacs.d/custom.el")

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)


;; buffer local variables
(setq-default indent-tabs-mode nil
              tab-width 4
              show-trailing-whitespace t
              c-basic-offset 4)

;; modes


;; global keybindings
(global-unset-key (kbd "C-z"))


;; package manager
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;; Ivy
(use-package counsel
  :ensure t
  :pin melpa
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-counsel-format "(%d/%d) "
        ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus))
        counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)


;; Colors
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

;; Scala
(use-package ensime
  :ensure t
  :pin melpa)

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)


;; Haskell
(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(require 'haskell-mode)
(define-key haskell-mode-map [f5] (lambda () (interactive) (compile "stack build --fast")))
(define-key haskell-mode-map [f12] 'intero-devel-reload)

(use-package hindent
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode)
  (setq hindent-style "johann-tibell"))


;; Markups
(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; Ledger
(use-package ledger-mode
  :ensure t
  :mode (("\\.ledger$" . ledger-mode)
         ("\\.dat$" . ledger-mode))
  :config
  (setq ledger-post-auto-adjust-amounts t))

(use-package flycheck-ledger
  :ensure t)
