(set-frame-font "Inconsolata 12" nil t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq custom-file "~/.emacs.d/custom.el"
      inhibit-startup-screen t
      make-backup-files nil)



;; Use-package
(require 'package)
(setq package-archives
      '(("elpa". "https://elpa.gnu.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 100)
	("elpa" . 50)
	("org" . 20)
	("melpa" . 10)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)



;; Eye candies
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))



;; Evil mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package evil-magit
  :ensure t)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))



;; Ivy
(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffer t
	ivy-counsel-format "(%d/%d) "
	ivy-re-builders-alist
	'((read-file-name-internal . ivy--regex-fuzzy)
	  (t . ivy--regex-plus))
	counsel-grep-base-command
	"rg -i -M 120 --no-heading --line-number --color never '%s' %s"))



;; Magit
(use-package magit
  :ensure t)



;; Org
(use-package org
  :ensure t)



;; Ledger mode
(use-package ledger-mode
  :ensure t
  :mode "\\.ledger$")
