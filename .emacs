;;  emacs -q -l path/.emacs

;; display startup time
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
		    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; default packages repositories and package inialization
(require 'package)
(setq package-archives '(("elpa"  . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(custom-set-variables
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("18bec4c258b4b4fb261671cf59197c1c3ba2a7a47cc776915c3e8db3334a0d25" default)))
 '(package-selected-packages (quote (company evil use-package smex dracula-theme))))
(custom-set-faces)

;; disable tool-bar and enable ido mode
(tool-bar-mode 0)
(ido-mode 1)

;; disable startup screen and backup files
(setq inhibit-startup-screen t)
(setq make-backup-files nil)

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; display line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; evil mode
(use-package evil
  :ensure t)

;; org mode
(use-package org
  :ensure t
  :init
  (setq org-support-shift-select t))

;; smex keybindings
(use-package smex
  :ensure t
  :config							     	       
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; autocomplete
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))
