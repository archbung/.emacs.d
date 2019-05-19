;;; init.el --- A humble Emacs config
;;; Commentary:
;;; Code:


(set-frame-font "Hack 10" nil t)

(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq custom-file "~/.emacs.d/custom.el"
      inhibit-startup-screen t
      make-backup-files nil
      require-final-newline t
      ring-bell-function 'ignore
      load-prefer-newer t
      save-place-file (concat user-emacs-directory "places")
      auto-save-default nil)

;;###autoload
(defun keychain-refresh-environment ()
  "Set ssh-agent and gpg-agent environment variables.
Set `SSH_AUTH_SOCK`, `SSH_AGENT_PID`, and `GPG_AGENT` in Emacs'
`process-environment` according to keychain"
  (interactive)
  (let* ((ssh (shell-command-to-string "keychain -q --noask --agent ssh --eval"))
	 (gpg (shell-command-to-string "keychain -q --noask --agent gpg --eval")))
    (list (and ssh
	       (string-match "SSH_AUTH_SOCK[=\s]\\([^\s;\n]*\\)" ssh)
	       (setenv "SSH_AUTH_SOCK" (match-string 1 ssh)))
	  (and ssh
	       (string-match "SSH_AGENT_PID[=\s]\\([0-9]*\\)?" ssh)
	       (setenv "SSH_AGENT_PID" (match-string 1 ssh)))
	  (and gpg
	       (string-match "GPG_AGENT_INFO[=\s]\\([^\s;\n]*\\)" gpg)
	       (setenv "GPG_AGENT_INFO" (match-string 1 gpg))))))

(keychain-refresh-environment)


;; Use-package
(require 'package)
(setq package-archives
      '(("elpa". "https://elpa.gnu.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 20)
	("elpa" . 50)
	("org" . 20)
	("melpa" . 100)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)


;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (evil-mode 1))


;; Essentials
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

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t
  :config
  (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward))

(use-package find-file-in-project
  :ensure t
  :bind ("C-c C-f" . find-file-in-project)
  :config
  (setq ffip-use-rust-fd t))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (add-hook 'undo-tree-visualizer-mode-hook
	    (lambda ()
	      (undo-tree-visualizer-selection-mode)
	      (setq display-line-numbers nil)))
  :config
  (global-undo-tree-mode 1))


;; Proof General
(use-package proof-general
  :ensure t)

;; Ledger
(use-package ledger-mode
  :ensure t)


;; Haskell
(use-package intero
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package hindent
  :ensure t
  :config
  (setq hindent-style 'johan-tibell))


;; Rust
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))


;; Z3
(use-package z3-mode
  :init
  :ensure t)


(provide 'init)
;;; init.el ends here
