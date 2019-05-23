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

; TODO: set some modes to insert-mode by default
;       - haskell-interactive-popup-errors
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

(use-package general
  :ensure t
  :config
  (general-create-definer leader-def :prefix "SPC")
  (leader-def 'normal
    "f r" 'counsel-recentf
    "f f" 'counsel-find-file
    "g s" 'magit-status
    "/"   'counsel-grep-or-swiper
    "b b" 'counsel-ibuffer
    "p f" 'find-file-in-project
    "p v" 'ffip-split-window-horizontally
    "p s" 'ffip-split-window-vertically)
  (general-create-definer localleader-def :prefix "SPC m")
  (localleader-def 'normal
    "j d" 'intero-goto-definition))

; TODO: redefine counsel-fzf to use fd instead (?)
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

(use-package find-file-in-project
  :ensure t
  :config
  (setq ffip-use-rust-fd t))

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
  (load-theme 'doom-one-light t)
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package rainbow-delimiters-mode
  :ensure rainbow-delimiters
  :hook prog-mode)


;; Proof General
(use-package proof-general
  :ensure t
  :init
  (add-hook 'coq-mode-hook #'company-coq-mode))

;; Ledger
(use-package ledger-mode
  :ensure t)


;; Haskell
(use-package intero
  :ensure t
  ; for some reason :hook does not work properly here
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
  :ensure t
  :mode "\\.z3\\'")


;; LaTeX
(use-package tex
  :ensure auctex
  :config
  (setq TeX-command-default "LatexMk"
        font-latex-fontify-script nil
        font-latex-fontify-sectioning 'color))

(use-package auctex-latexmk
  :ensure t
  :init
  (with-eval-after-load 'tex
    (auctex-latexmk-setup)))


(provide 'init)
;;; init.el ends here
