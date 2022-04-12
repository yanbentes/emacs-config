;; https://github.com/yanbentes
(package-initialize)

(load "~/.emacs.d/early-init.el")

;; display startup time
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
		    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

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

(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(set-fringe-mode 0)
(ido-mode 1)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; display line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (load-theme 'monokai t)
(load-theme 'dracula t)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'yasnippet)
(yas-global-mode 1)
(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))

(require 'auto-complete)
(ac-config-default)

;; to use impatient mode with markdown preview do
;; M-x httpd-start
;; M-x impatient-mode
;; M-x imp-set-user-filter RET markdown-html RET
(require 'impatient-mode)
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(impatient-mode auto-complete dracula-theme monokai-theme smex markdown-mode lua-mode python-mode yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
