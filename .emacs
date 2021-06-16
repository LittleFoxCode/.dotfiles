;;; Code:
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq show-paren-mode t)    ; I like to be able to see the matching parenthesis!
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Padding around frame edges
(menu-bar-mode -1)          ; Disable menu bar

(column-number-mode)        ; Show column on modeline
(global-display-line-numbers-mode t)   ; Activate linenumbers globally
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))   ; Exclude line numbers from these major modes

(setq select-enable-clipboard t) ; Enalbes to paste from other window and vice versa.

;; Packages and package config
(require 'package)
 
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/write/notes/notes.org" "~/write/notes/körkort.org"))
 '(package-selected-packages
   '(org-web-tools request esxml org-yt lsp-mode org-bullets python-mode lsp-python-ms flycheck-kotlin flycheck rainbow-delimiters doom-themes eterm-256color telephone-line evil-collection evil magit projectile which-key doom-modeline use-package evil-visual-mark-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; TODO: Use Ivy or Helm??

;; Config for telephone-line
(use-package telephone-line
  :ensure t
  :custom
  (telephone-line-primary-left-separator 'telephone-line-cubed-left)
  (telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)
  (telephone-line-primary-right-separator 'telephone-line-cubed-right)
  (telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-height 15)
  :config (telephone-line-mode t))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; Whichkey config, for when you have a brainfart :^)
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/dev/")
    (setq projectile-project-search-path '("~/dev/")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package magit
  :ensure t)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)))

;; Org-mode cofig
(use-package org
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c b" . org-iswitchb)
  ("C-c c" . org-capture)
  :config
  (setq org-startup-truncated nil)
  (progn
  (setq org-publish-project-alist
   '(("lysblog" ;; my blog project (just a name)
         ;; Path to org files.
         :base-directory "~/write/blog/lysblog/_org"
         :base-extension "org"
         ;; Path to Jekyll Posts
         :publishing-directory "~/write/blog/lysblog/_posts/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t
         )))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
  
;; C-c C-k enables char mode for term when using applications with keys in termainal
;; C-c C-j to return to linemode afterwards
;; C-c C-p/C-c C-n to go back and forward in prompts
(use-package term
  :config
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%&>] *"))
;; Allows for better color in term-mode
(use-package eterm-256color
  :hook (term . eterm-256color-mode))



;;LANUGAGES checking and lsp


(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;LSP-mode config
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; kotlin config
;; requires to install ktlint https://github.com/pinterest/ktlint
(use-package flycheck-kotlin
  :after (flycheck)
  :init (flycheck-kotlin-setup))

;; python config
;; requires https://emacs-lsp.github.io/lsp-mode/page/lsp-pyls/
(use-package python-mode ;; Already installed by default
  :custom
  (python-shell-interpreter "python3"))

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp)))
  :init
  (setq lsp-python-ms-executable (executable-find "python-language-server")))
;;; .emacs ends here

