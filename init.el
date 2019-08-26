;;; init.el --- A humble Emacs config
;;; Commentary:
;;; Code:


(set-frame-font "Hack 10" nil t)

;; Cleaner UI
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      inhibit-compacting-font-caches t)


;; Set default browser
(setq browse-url-generic-program (executable-find (getenv "BROWSER"))
      browse-url-browser-function 'browse-url-generic)


(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))


;; Handle backup, temporary, and changed files
(setq make-backup-files nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq load-prefer-newer t
      auto-save-default t)


;; Show matching parentheses
(show-paren-mode 1)


;; Use spaces
(setq-default indent-tabs-mode nil)


;; Line numbers
(setq-default display-line-numbers 'visual)


;; Handle trailing whitespaces
(setq-default show-trailing-whitespace t)
(add-hook 'after-save-hook #'delete-trailing-whitespace)


;; Custom file
(setq custom-file "~/.emacs.d/custom.el")


;; Handle credentials
;;###autoload
(defun keychain-refresh-environment ()
  "Set ssh-agent and gpg-agent environment variables.

Set `SSH_AUTH_SOCK`, `SSH_AGENT_PID`, and `GPG_AGENT` in Emacs'
`process-environment` according to keychain"
  (interactive)
  (let* ((ssh (shell-command-to-string "keychain -q --noask --agents ssh --eval"))
	 (gpg (shell-command-to-string "keychain -q --noask --agents gpg --eval")))
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


;; Bootstrap straight.el
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)


;; PACKAGES
;; Essentials
(use-package use-package-ensure-system-package
  :config
  (setq system-packages-use-sudo t
        system-packages-package-manager 'pacman))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package which-key
  :config
  (which-key-mode))

(use-package undo-tree
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :hook
  (undo-tree-visualizer-mode .
	    (lambda ()
	      (undo-tree-visualizer-selection-mode)
	      (setq display-line-numbers nil)))
  :config
  (global-undo-tree-mode 1))


;; Navigation
(defun linum:relative ()
  (setq-local display-line-numbers 'visual))

(defun linum:absolute ()
  (setq-local display-line-numbers t))

(use-package evil
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil
        evil-want-keybinding nil
        evil-want-integration t)
  :config
  (add-hook 'evil-insert-state-entry-hook #'linum:absolute)
  (add-hook 'evil-insert-state-exit-hook #'linum:relative)

  (evil-mode 1))

(use-package evil-surround
  :after evil
  :hook (org-mode . (lambda () (push '(?m . ("\\( " . " \\)")) evil-surround-pairs-alist)))
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-magit
  :config
  (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward))

(use-package evil-org
  :after org)

(use-package avy)

(use-package ace-window)


;; Org
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are DONE, to TODO otherwise."
  (let (org-log-done org-log-states)
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(use-package org
  :hook (org-after-todo-statistics . org-summary-todo)
  :config
  (add-to-list 'org-export-backends 'ox-md)
  (mapc (lambda (arg) (add-to-list 'org-modules arg)) '(org-habit 'org-timer))
  (add-to-list 'org-agenda-files "~/org")
  (setq org-enforce-todo-dependencies t
        org-return-follows-link t
        org-todo-keywords
        '((sequence "TODO(t)" "VERIFY(v@/!)" "|" "DONE(d!)" "CANCELED(c@)"))
        org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Inbox")
           "* TODO %?\n %i %a")
          ("T" "Tickler" entry (file+headline "~/org/tickler.org" "Tickler")
           "* TODO %?\n %i %U")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\n %i Entered on %U\n"))
        org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
                             ("~/org/someday.org" :maxlevel . 1)
                             ("~/org/tickler.org" :maxlevel . 2))
        org-log-into-drawer t
        org-archive-location "~/org/archive.org::* From %s"
        org-clock-persist 'history
        )
  (org-clock-persistence-insinuate))


;; Ivy
(use-package counsel
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffer t
	ivy-counsel-format "(%d/%d) "
	ivy-re-builders-alist
	'((read-file-name-internal . ivy--regex-fuzzy)
	  (t . ivy--regex-plus))
        ; TODO: make swiper faster
	counsel-grep-base-command
	"rg -i -M 120 --no-heading --line-number --color never '%s' %s"))


;; Project management
(use-package magit)

(use-package find-file-in-project
  :config
  (setq ffip-use-rust-fd t))


;; Snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)


;; Eye candies
(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package rainbow-delimiters-mode
  :straight rainbow-delimiters
  :hook prog-mode)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons)


;; Proof General
(use-package company-coq-mode
  :straight company-coq
  :hook coq-mode
  :init
  (setq company-coq-disabled-features
        '(smart-subscripts prettify-symbols title-comments)))

(use-package proof-general)


;; Accounting
(use-package ledger-mode
  :config
  (add-hook 'ledger-mode-hook
            (lambda ()
              (setq-local tab-always-indent 'complete)
              (setq-local completion-cycle-threshold t)
              (setq-local ledger-complete-in-steps t))))


;; Haskell
(use-package intero-mode
  :straight intero
  :hook haskell-mode)

(use-package hindent-mode
  :straight hindent
  :hook haskell-mode)


;; Rust
(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (setq rust-format-on-save t))

(use-package cargo)


;; Z3
(use-package z3-mode
  :mode "\\.z3\\'")


;; Documents processing
(use-package tex
  :straight auctex
  :config
  (add-hook 'LaTeX-mode-hook #'outline-minor-mode)
  (setq TeX-command-default "LatexMk"
        font-latex-fontify-script nil
        font-latex-fontify-sectioning 'color)
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura")))

(use-package auctex-latexmk
  :init
  (with-eval-after-load 'tex
    (auctex-latexmk-setup)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "markdown"))

(use-package spthy-mode
  :straight (spthy-mode :host github :repo "tamarin-prover/editors")
  :mode "\\.spthy\\'")

(use-package pkgbuild-mode)


(provide 'init)
;;; init.el ends here
