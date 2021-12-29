;; my github profile: https://github.com/yanbentes

;; display startup time
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
		    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(custom-set-variables
 '(package-selected-packages (quote (eglot use-package smex evil dracula-theme company))))
(custom-set-faces)

;; disable startup screen and backup files
(setq inhibit-startup-screen t)
(setq make-backup-files nil)

(tool-bar-mode 0)
(scroll-bar-mode 1)
(set-fringe-mode 5)
(ido-mode 1)

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; display line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; dracula theme
(use-package dracula-theme
  :ensure t
  :init
  (load-theme 'dracula t))

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

;; eglot lsp client
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-10"))
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-to-list 'eglot-server-programs '((python-mode) "~/.local/bin/pylsp"))
  (add-hook 'python-mode-hook 'eglot-ensure))

;; company mode
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))
