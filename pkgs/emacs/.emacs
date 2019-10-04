;;; package --- emacs init file
;;; Commentary:
;;; Code:

;; TODO
;; flx, smex, helm, ido

(progn
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (setq package-enable-at-startup nil)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(use-package auto-package-update
  :ensure t
  :init
  (setq auto-package-update-delete-old-versions t
	auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package whitespace
  :ensure t
  :init
  (setq whitespace-style '(face lines-tail)
	whitespace-line-column 79)
  :config
  (global-whitespace-mode 1)
  :bind ("C-c t w s" . 'whitespace-mode))
  
(use-package guru-mode
  :ensure t
  :config
  (guru-global-mode 1)
  :bind ("C-c t g" . 'guru-global-mode))

(use-package faces
  :config
  (set-face-attribute 'default nil :family "SF Mono" :weight 'bold))

(use-package dracula-theme
  :ensure t
  :init
  (defun set-dracula (&optional frame)
    (if (display-graphic-p frame)
        (load-theme 'dracula t)
      (disable-theme 'dracula)))
  :hook ((after-init . set-dracula)
         (after-make-frame-functions . set-dracula)
         (server-switch . set-dracula)))

(use-package flycheck
  :ensure t
  :init
  (setq flycheck-idle-change-delay 0.5)
  :config
  (global-flycheck-mode 1)
  :bind
  ("C-c t f" . 'flycheck-mode))

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  :config
  (global-company-mode))

(use-package files
  :config
  (setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t))))

(use-package paren
  :init
  (setq show-paren-delay 0.5)
  :config
  (show-paren-mode 1))

(use-package elec-pair
  :config
  (electric-pair-mode 1)
  :bind ("C-c t e p" . 'electric-pair-mode))

(use-package cc-vars
  :config
  (setq-default c-default-style "linux"
		c-basic-offset 4))

(use-package cc-styles
  :config
  (c-set-offset 'inline-open 0))
  
(use-package simple
  :config
  (setq-default backward-delete-char-untabify-method 'hungry)
  :bind ("C-c i" . overwrite-mode))

(use-package display-line-numbers
  :init
  (setq display-line-numbers-type 'relative
        display-line-numbers-current-absolute nil)
  (global-display-line-numbers-mode 1)
  :bind ("C-c t l n" . display-line-numbers-mode)
  :hook (doc-view-mode . (lambda () (display-line-numbers-mode 0))))

(use-package glasses
  :init
  (setq glasses-separate-parentheses-p nil)
  :hook (java-mode . glasses-mode))

(use-package doc-view
  :init
  (setq doc-view-resolution 300))

(use-package tool-bar
  :config
  (tool-bar-mode 0))

(use-package scroll-bar
  :config
  (scroll-bar-mode 0))

(use-package menu-bar
  :init
  (defun set-menu-bar (&optional frame)
    (if (display-graphic-p frame)
        (menu-bar-mode 1)
      (menu-bar-mode 0)))
  :hook ((after-init . set-menu-bar)
         (after-make-frame-functions . set-menu-bar)
         (server-switch . set-menu-bar)))

;; Defined in C source code.
(setq-default indent-tabs-mode nil
              truncate-lines 1)

(progn
  (defun switch-to-ansi-term ()
    "Open a running 'ansi-term' buffer in other window if one exists.
Otherwise start one."
    (interactive)
    (let ((ansi-term (get-buffer "*ansi-term*")))
      (if ansi-term
          (switch-to-buffer-other-window ansi-term)
        (let ((ansi-term (ansi-term shell-file-name)))
          (switch-to-buffer (other-buffer ansi-term t))
          (switch-to-buffer-other-window ansi-term)))))
  (global-set-key (kbd "C-c s a") 'switch-to-ansi-term))


(progn
  (defun mumble-or-n-p (prompt)
    "Ask user a y or n question.
Default answer is yes.
PROMPT is the string to display to ask the question"
    (if (string= "n"
                 (downcase
                  (read-from-minibuffer
                   (concat prompt "(Y or n) "))))
        nil
      t))
  (defalias 'yes-or-no-p 'mumble-or-n-p)
  (defalias 'y-or-n-p 'mumble-or-n-p))

(provide '.emacs)
;;; .emacs ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company use-package guru-mode flycheck dracula-theme auto-package-update))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
