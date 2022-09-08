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

;; move cursor to new window automatically
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; emacs gui
(ido-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 1)
(set-fringe-mode 0)
(line-number-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode 0)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setq inhibit-startup-screen t)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq org-startup-indented t)
(put 'dired-find-alternate-file 'disabled nil)

;; (load-theme 'dracula t)
;; (load-theme 'gruber-darker t)
(load-theme 'doom-monokai-classic t)

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
(setq all-the-icons-scale-factor 1.2)

(require 'doom-modeline)
(doom-modeline-mode 1)
(doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host buffer-position selection-info)
    '(objed-state misc-info persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker "  "))

(require 'dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'html-mode-hook #'lsp)
(add-hook 'sh-mode-hook #'lsp)
(setq lsp-headerline-breadcrumb-segments '(symbols))
(setq lsp-modeline-code-actions-segments '(name))
(setq lsp-keep-workspace-alive nil)
(setq lsp-warn-no-matched-clients nil)

;; pylsp config
(setq lsp-pylsp-plugins-pydocstyle-enabled nil)
(setq lsp-pylsp-plugins-flake8-max-line-length 1000)

(require 'company)
(setq company-format-margin-function nil)

;; other keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-P") 'move-line-up)
(global-set-key (kbd "M-N") 'move-line-down)
(global-set-key (kbd "M-*") 'compile)
