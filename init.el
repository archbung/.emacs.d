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
      ; This speeds up the modeline
      inhibit-compacting-font-caches t
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


;; Essentials
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-version t
        auto-package-update-interval 3)
  (auto-package-update-maybe))

(use-package evil
  :ensure t
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (evil-define-key 'normal ibuffer-mode-map
    (kbd "j") 'evil-next-line
    (kbd "k") 'evil-previous-line
    (kbd "J") 'ibuffer-jump-to-buffer
    (kbd "l") 'ibuffer-visit-buffer)
  (evil-mode 1))

; Keybindings 2.0: electric boogaloo
(use-package general
  :ensure t
  :config
  (general-create-definer leader-def :prefix "SPC")
  (leader-def 'normal
    "/ g" 'counsel-grep-or-swiper
    "/ /" 'swiper-isearch
    "b b" 'counsel-ibuffer
    "f r" 'counsel-recentf
    "f f" 'counsel-find-file
    "g s" 'magit-status
    "h f" 'counsel-describe-function
    "h v" 'counsel-describe-variable
    "j c" 'avy-goto-char
    "j l" 'avy-goto-line
    "o l" 'org-store-link
    "o a" 'org-agenda
    "o c" 'org-capture
    "p f" 'find-file-in-project
    "p v" 'ffip-split-window-horizontally
    "p s" 'ffip-split-window-vertically
    "r"   'counsel-rg
    "v u" 'undo-tree-visualize)
  (general-create-definer localleader-def :prefix "SPC m")
  (localleader-def 'normal
    "j d" 'intero-goto-definition

    ;; Org-related
    ; Time-related
    "t ." 'org-time-stamp
    "t !" 'org-time-stamp-inactive
    "t d" 'org-deadline
    "t s" 'org-schedule
    "t e" 'org-clock-modify-effort-estimate
    "t i" 'org-clock-in
    "t o" 'org-clock-out

    ; View-related
    "/"   'org-sparse-tree

    ; Refile and copy
    "r r" 'org-refile
    "r c" 'org-copy

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
  :ensure t
  :hook (org-after-todo-statistics . org-summary-todo)
  :config
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-agenda-files "~/org")
  (setq org-enforce-todo-dependencies t
        org-todo-keywords
        '((sequence "TODO(t)" "VERIFY(v@/!)" "|" "DONE(d!)" "CANCELED(c@)"))
        org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Inbox")
           "* TODO %?\n %i\n %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n %i\n %a"))
        org-log-into-drawer t
        ))

(use-package counsel
  :ensure t
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

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t
  :config
  (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward))

(use-package avy
  :ensure t)

(use-package find-file-in-project
  :ensure t
  :config
  (setq ffip-use-rust-fd t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package ace-window
  :ensure t
  ; TODO: learn how this package works
  ; TODO: perhaps use a better keymap
  :bind ("M-o" . ace-window))

(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :hook
  (undo-tree-visualizer-mode .
	    (lambda ()
	      (undo-tree-visualizer-selection-mode)
	      (setq display-line-numbers nil)))
  :config
  (global-undo-tree-mode 1))


;; Eye candies
(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-spacegrey t)
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package rainbow-delimiters-mode
  :ensure rainbow-delimiters
  :hook prog-mode)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons)


;; Proof General
(use-package company-coq-mode
  :ensure company-coq
  :hook coq-mode
  :init
  (setq company-coq-disabled-features
        '(smart-subscripts prettify-symbols title-comments)))

(use-package proof-general
  :ensure t)


;; Accounting
(use-package ledger-mode
  :ensure t)


;; Haskell
(use-package intero-mode
  :ensure intero
  :hook haskell-mode)

(use-package hindent-mode
  :ensure hindent
  :hook haskell-mode)


;; Rust
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))


;; Z3
(use-package z3-mode
  :ensure t
  :mode "\\.z3\\'")


;; Documents processing
(use-package tex
  :ensure auctex
  :config
  (setq TeX-command-default "LatexMk"
        font-latex-fontify-script nil
        font-latex-fontify-sectioning 'color)
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura")))

(use-package auctex-latexmk
  :ensure t
  :init
  (with-eval-after-load 'tex
    (auctex-latexmk-setup)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "markdown"))


(provide 'init)
;;; init.el ends here
