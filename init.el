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

(use-package evil
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (evil-define-key 'normal ibuffer-mode-map
    (kbd "j") 'evil-next-line
    (kbd "k") 'evil-previous-line
    (kbd "J") 'ibuffer-jump-to-buffer
    (kbd "l") 'ibuffer-visit-buffer)
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (evil-define-key 'motion org-agenda-mode-map
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
    "L" 'org-agenda-do-date-later
    )
  (evil-define-key 'normal org-mode-map
    (kbd "<return>") 'org-open-at-point)

  (defun linum:relative ()
    (setq-local display-line-numbers 'visual))

  (defun linum:absolute ()
    (setq-local display-line-numbers t))

  (add-hook 'evil-insert-state-entry-hook #'linum:absolute)
  (add-hook 'evil-insert-state-exit-hook #'linum:relative)

  (evil-mode 1))

(use-package evil-surround
  :hook (org-mode . (lambda () (push '(?m . ("\\( " . " \\)")) evil-surround-pairs-alist)))
  :config
  (global-evil-surround-mode 1))

; Keybindings 2.0: electric boogaloo
(use-package general
  :config
  (general-create-definer leader-def :prefix "SPC")
  (leader-def 'normal
    "/ g" 'counsel-grep-or-swiper
    "/ /" 'swiper-isearch
    "/ r" 'counsel-rg
    "b b" 'counsel-ibuffer
    "f r" 'counsel-recentf
    "f f" 'counsel-find-file
    "g s" 'magit-status
    "h f" 'counsel-describe-function
    "h v" 'counsel-describe-variable
    "h m" 'describe-mode
    "h i" 'info
    "j c" 'avy-goto-char
    "j l" 'avy-goto-line
    "o l" 'org-store-link
    "o a" 'org-agenda
    "o c" 'org-capture
    "p f" 'find-file-in-project
    "p v" 'ffip-split-window-horizontally
    "p s" 'ffip-split-window-vertically
    "v u" 'undo-tree-visualize)
  (general-create-definer localleader-def :prefix "SPC m")
  (localleader-def 'normal
    "j d" 'intero-goto-definition

    ;; Org-related
    "a a" 'org-archive-subtree-default
    "a t" 'org-toggle-archive-tag

    ; Time-related
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

    ; View-related
    "/"   'org-sparse-tree
    "v c" 'org-columns
    "v h" 'outline-hide-entry
    "v s" 'outline-show-entry

    ; Refile and copy
    "r r" 'org-refile
    "r y" 'org-copy
    "r y" 'avy-org-refile-as-child

    ; Properties-related
    "p t" 'org-set-tags-command
    "p s" 'org-set-property
    "p d" 'org-delete-property

    "c c" 'TeX-command-master

    "b p" 'ledger-display-balance-at-point))

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

(use-package magit)

(use-package evil-magit
  :config
  (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward))

(use-package avy)

(use-package find-file-in-project
  :config
  (setq ffip-use-rust-fd t))

(use-package which-key
  :config
  (which-key-mode))

(use-package ace-window
  ; TODO: learn how this package works
  ; TODO: perhaps use a better keymap
  :bind ("M-o" . ace-window))

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
  (let ((current-hour (string-to-number (format-time-string "%H"))))
    (if (or (< current-hour 6) (> current-hour 20)) (load-theme 'doom-one t) (load-theme 'doom-one-light t)))
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
