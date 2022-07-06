;; minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;; display startup time
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
		    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)
(add-hook 'emacs-startup-hook #'fix-scratch)

(load "~/.emacs.d/early-init.el")
(load "~/.emacs.d/scratch.el")

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

;; setup package installation
(defun timu/packages-installed-p ()
  "Check if all packages in `timu-package-list' are installed."
  (cl-every #'package-installed-p timu-package-list))

(defun timu/require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package timu-package-list)
    (add-to-list 'timu-package-list package))
  (unless (package-installed-p package)
    (package-install package)))

(defun timu/require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'timu/require-package packages))

(defun timu/install-packages ()
  "Install all packages listed in `timu-package-list'."
  (unless (timu/packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Reloading packages DB...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (timu/require-packages timu-package-list)))

;; run package installation
(timu/install-packages)

;; custom variables in a different file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; move lines up and down
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;; case sensitive replace string
(defun with-case-fold-search (orig-fun &rest args)
  (let ((case-fold-search t))
    (apply orig-fun args)))

(advice-add 'replace-string :around #'with-case-fold-search)

;; emacs gui
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 1)
(set-fringe-mode 0)
(line-number-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode 0)

(setq inhibit-startup-screen t)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq org-startup-indented t)
(put 'dired-find-alternate-file 'disabled nil)

;; (load-theme 'dracula t)
(load-theme 'gruber-darker t)

(ido-mode 1)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'yasnippet)
(yas-global-mode 1)
(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))

(require 'dired-sidebar)
(global-set-key (kbd "C-x C-n") 'dired-sidebar-toggle-with-current-directory)

(require 'rainbow-mode)
(rainbow-mode 1)

(require 'all-the-icons)
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-lsp t)

(require 'dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(require 'lsp-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
;; (setq lsp-diagnostics-provider :none)
(setq lsp-headerline-breadcrumb-segments '(symbols))
(setq lsp-modeline-code-actions-segments '(name))

(require 'company)
(setq company-format-margin-function nil)

;; other keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-P") 'move-line-up)
(global-set-key (kbd "M-N") 'move-line-down)
(global-set-key (kbd "M-*") 'compile)
