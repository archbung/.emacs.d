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


;; Handle trailing whitespaces
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
(use-package evil
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil
        evil-want-keybinding nil
        evil-want-integration t)
  :config
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (evil-mode 1))

(use-package evil-surround
  :after evil
  :hook (org-mode . (lambda () (push '(?m . ("\\( " . " \\)")) evil-surround-pairs-alist)))
  :config
  (global-evil-surround-mode 1))

(use-package evil-magit
  :config
  (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward))

(use-package general
  :config
  (general-def
    :states 'normal
    :keymaps 'ibuffer-mode-map
    (kbd "j") 'evil-next-line
    (kbd "k") 'evil-previous-line
    (kbd "J") 'ibuffer-jump-to-buffer
    (kbd "l") 'ibuffer-visit-buffer)

  (general-def
    :states 'motion
    :keymaps 'org-agenda-mode-map
    (kbd "<tab>") 'org-agenda-goto
    (kbd "<return>") 'org-agenda-switch-to
    (kbd "M-<return>") 'org-agenda-recenter
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    (kbd "t") 'org-agenda-todo
    (kbd "u") 'org-agenda-undo
    "gj" 'org-agenda-next-item
    "gk" 'org-agenda-previous-item
    (kbd "[") 'org-agenda-earlier
    (kbd "]") 'org-agenda-later
    "J" 'org-agenda-priority-down
    "K" 'org-agenda-priority-up
    "H" 'org-agenda-do-date-earlier
    "L" 'org-agenda-do-date-later)

  (general-def
    :states 'normal
    :keymaps 'org-mode-map
    (kbd "<return>") 'org-open-at-point)

  (general-def
    :keymaps 'org-capture-mode-map
    [remap evil-save-and-close]          'org-capture-finalize
    [remap evil-save-modified-and-close] 'org-capture-finalize
    [remap evil-quit]                    'org-capture-kill)

  (general-def
    :states 'normal
    :keymaps 'rust-mode-map
    (kbd "<tab>") 'company-indent-or-complete-common)

  (general-def
    :states 'normal
    :keymaps 'outline-minor-mode-map
    (kbd "<tab>") 'outline-toggle-children)

  (general-create-definer leader-def :prefix "SPC")
  (leader-def
    :states 'visual
    "c"   'comment-dwim)

  (leader-def
    :states 'normal
    :keymaps 'flyspell-mode-map
    "s"   'flyspell-correct-wrapper)

  (leader-def
    :states 'normal
    "/ g" 'counsel-grep-or-swiper
    "/ r" 'counsel-rg

    "b b" 'counsel-ibuffer

    "f f" 'counsel-find-file
    "f r" 'counsel-recentf

    "g s" 'magit-status

    "j c" 'avy-goto-char
    "j l" 'avy-goto-line

    "h f" 'counsel-describe-function
    "h v" 'counsel-describe-variable
    "h i" 'info
    "h m" 'describe-mode
    "h k" 'isearch-describe-key
    "h K" 'general-describe-keybindings

    "o a" 'org-agenda
    "o c" 'org-capture
    "o l" 'org-store-link

    "p f" 'find-file-in-project
    )

  (general-create-definer localleader-def :prefix "SPC m")
  (localleader-def
    :states 'normal
    :keymaps 'org-mode-map
    "/"   'org-sparse-tree
    "c"   'org-columns
    "a a" 'org-archive-subtree-default

    "p t" 'org-set-tags-command
    "p a" 'org-toggle-archive-tag
    "p s" 'org-set-property
    "p d" 'org-delete-property

    "t ." 'org-time-stamp
    "t !" 'org-time-stamp-inactive
    "t d" 'org-deadline
    "t s" 'org-schedule
    "t e" 'org-clock-modify-effort-estimate
    "t i" 'org-clock-in
    "t o" 'org-clock-out
    "t t" 'org-timer-set-timer
    "t p" 'org-timer-pause-or-continue
    "t T" 'org-timer-stop

    "r r" 'org-refile
    "r y" 'org-copy
    "r y" 'avy-org-refile-as-child)

  (localleader-def
    :states 'normal
    :keymaps 'ledger-mode-map
    "a"   'ledger-add-transaction

    "r e" 'ledger-report-edit-report
    "r r" 'ledger-report
    "r n" 'ledger-report-next-month
    "r p" 'ledger-report-previous-month

    "R"   'ledger-reconcile
    "b"   'ledger-display-balance-at-point)

  (localleader-def
    :states 'normal
    :keymaps 'TeX-mode-map
    "c"   'TeX-command-master)

  (localleader-def
    :states 'normal
    :keymaps 'intero-mode-map
    "a"   'intero-add-package
    "l"   'intero-repl-load
    "i"   'intero-info
    "t"   'intero-type-at
    "d"   'intero-goto-definition)

  (localleader-def
    :states 'normal
    :keymaps 'cargo-minor-mode-map
    "n"   'cargo-process-new
    "r"   'cargo-process-run
    "t"   'cargo-process-test
    "c"   'cargo-process-build
    "C"   'cargo-process-clean
    "k"   'cargo-process-check
    "K"   'cargo-process-clippy
    "u"   'cargo-process-update
    "U"   'cargo-process-upgrade)
  )

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


;; Syntax checking
(use-package flycheck
  :hook (rust-mode . flycheck-mode))

(use-package flycheck-ledger
  :after flycheck)

(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))


;; Completion
(use-package company
  :hook ((rust-mode racer-mode emacs-lisp-mode LaTeX-mode) . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.3
        company-tooltip-align-annotations t
        company-minimum-prefix-length 1
        company-selection-wrap-around t))


;; Snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)


;; Spell checking
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (setq flyspell-issue-message-flag nil
        ispell-dictionary "en_US"
        ispell-program-name "hunspell"))

(use-package flyspell-correct-ivy
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))


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
  (setq rust-format-on-save t))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package racer
  :hook (rust-mode . racer-mode))


;; Z3
(use-package z3-mode
  :mode "\\.z3\\'")


;; Documents processing
(use-package tex
  :straight auctex
  :config
  (setq TeX-command-default "LatexMk"
        font-latex-fontify-script nil
        font-latex-fontify-sectioning 'color)
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura")))

(use-package outline
  :hook (LaTeX-mode . outline-minor-mode))

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

(use-package eldoc
  :hook (racer-mode . eldoc-mode))

(use-package pkgbuild-mode)


(provide 'init)
;;; init.el ends here
