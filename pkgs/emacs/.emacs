;;; package --- emacs init file
;;; Commentary:
;;; Code:

;; List of desired packages
(defvar package-list)
(setq package-list
      '(
        ;; Editing
        whitespace
        multiple-cursors
        move-text
        ;; Syntax checker
        flycheck
        ;; Auto completion
        company
        ;; Better C
        company-irony
        company-irony-c-headers
        irony-eldoc
        flycheck-irony
        ;; Common Lisp
        slime
        slime-company
        ;; Better D
        ;;company-dcd
        ;;d-mode
        ;; Better Clojure
        ;;clojure-mode
        ;;clojure-mode-extra-font-locking
        ;;cider
        ;;rainbow-blocks
        ;; Better typescript
        ;;typescript-mode
        ;;tide))
        ))

;; Package initialization
(package-initialize)
;; Temporary fix for emacs bug
(defvar package-archives)
(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(defvar package-archive-contents)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Package configuration
(defun package-config/whitespace ()
  "Whitespace configuration."
  (if (package-installed-p 'whitespace)
      (progn
        (defvar whitespace-style)
        (setq whitespace-style '(face lines-tail))
        (defvar whitespace-line-column)
        (setq whitespace-line-column 79)
        (global-whitespace-mode 1))))

(defun package-config/multiple-cursors ()
  "Multiple Cursors configuration."
  (if (package-installed-p 'multiple-ccursors)
      (global-set-key (kbd "C-c m c") 'mc/edit-lines)))

(defun package-config/move-text ()
  "Move Text configuration."
  (if (package-installed-p 'move-text)
      (move-text-default-bindings)))

(defun package-config/flycheck ()
  "Flycheck configuration.
Checkers: https://www.flycheck.org/en/latest/languages.html?highlight=bash."
  (if (package-installed-p 'flycheck)
      (progn
        (global-flycheck-mode)
        (set-variable 'flycheck-idle-change-delay 0.125))))

(defun package-config/company ()
  "Company configuration."
  (if (package-installed-p 'company)
      (progn
        (global-company-mode)
        (set-variable 'company-idle-delay 0.125)
        (set-variable 'company-minimum-prefix-length 2)
        (global-set-key (kbd "C-x C-x") 'company-complete)))
  (if (and
       (package-installed-p 'company)
       (package-installed-p 'company-irony)
       (package-installed-p 'company-irony-c-headers)
       (package-installed-p 'irony-eldoc)
       (package-installed-p 'flycheck)
       (package-installed-p 'flycheck-irony))
      (progn
        (add-hook 'c++-mode-hook 'irony-mode)
        (add-hook 'c-mode-hook 'irony-mode)
        (add-hook 'objc-mode-hook 'irony-mode)
        (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
        (defvar company-backends)
        (eval-after-load 'company
          '(add-to-list
            'company-backends
            '(company-irony-c-headers company-irony)))
        (eval-after-load 'flycheck
          '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
        (add-hook 'irony-mode-hook #'irony-eldoc))))

(defun package-config/slime ()
  "Slime configuration."
  (if (package-installed-p 'slime)
      (progn
        (defvar inferior-lisp-program)
        (setq inferior-lisp-program "/bin/sbcl")
        (defvar slime-contribs)
        (setq slime-contribs '(slime-fancy))
        (if (package-installed-p 'slime-company)
            (add-to-list 'slime-contribs 'slime-company t)))))

(add-hook 'after-init-hook
          (lambda ()
            (package-config/whitespace)
            (package-config/multiple-cursors)
            (package-config/move-text)
            (package-config/flycheck)
            (package-config/company)
            (package-config/slime)))

;; Move emacs temporary and autosave files to a temporary directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun smart-home ()
  "Go to first non indent, then line start."
  (interactive)
  (let ((old (point)))
    (handle-shift-selection)
    (back-to-indentation)
    (and (= old (point))
         (beginning-of-line))))

(defun open-init-file ()
  "Open this init file."
  (interactive)
  (find-file "~/.emacs"))

;; Code entry and UI/UX
(menu-bar-mode -1)
(global-font-lock-mode 0)
(defvar show-paren-delay)
(setq show-paren-delay 0)
(show-paren-mode t)
(column-number-mode 0)
(electric-pair-mode t)
(delete-selection-mode t)
(setq-default indent-tabs-mode nil)
(setq-default c-default-style "linux")
(setq-default c-basic-offset 4)
(setq-default backward-delete-char-untabify-method 'hungry)
(c-set-offset 'inline-open 0)

;; Keybinding
(if (fboundp 'smart-home)
    (global-set-key (kbd "<home>") 'smart-home))
(if (fboundp 'open-init-file)
    (global-set-key (kbd "C-c C-f C-g") 'open-init-file))
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-x c") 'compile)
(global-set-key (kbd "C-x <C-up>") 'windmove-up)
(global-set-key (kbd "C-x <C-left>") 'windmove-left)
(global-set-key (kbd "C-x <C-down>") 'windmove-down)
(global-set-key (kbd "C-x <C-right>") 'windmove-right)
(global-set-key (kbd "M-o m") 'newline-and-indent)
(global-set-key (kbd "C-c DEL") 'c-hungry-delete-backwards)
(global-set-key (kbd "C-c <deletechar>") 'c-hungry-delete-forward)

;; Recolor mode-line
(defvar mode-line-colors t)
(when mode-line-colors
  (progn
  (set-face-background 'mode-line "#FFFFFF")
  (set-face-foreground 'mode-line "#000000")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck-irony irony-eldoc company-irony-c-headers company-irony company flycheck move-text multiple-cursors))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide '.emacs)
;;; .emacs ends here
