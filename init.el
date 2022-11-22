;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;; Display startup time
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
(unless package-archive-contents
  (package-refresh-contents))

;; Setup package installation
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

;; Custom variables in a different file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Custom functions
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

;; duplicate lines
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

;; GUI tweaks
(ido-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 1)
(set-fringe-mode 5)
(line-number-mode 1)
(column-number-mode 1)
(delete-selection-mode 1)
(set-face-foreground 'vertical-border "#282828")
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; line numbers relative
(setq display-line-numbers-type (quote relative))
(global-display-line-numbers-mode t)

(setq inhibit-startup-screen t)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq org-startup-indented t)
(put 'dired-find-alternate-file 'disabled nil)

;; disable line numbers in some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
				markdown-mode-hook
				neotree-mode-hook
				dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Themes
(load-theme 'gruber-darker t)
;; (load-theme 'doom-monokai-classic t)
;; (load-theme 'doom-molokai t)

;; Packages configuration
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'yasnippet)
(yas-global-mode 1)
(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))

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
(add-hook 'sh-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'ts-mode-hook #'lsp)
(add-hook 'web-mode-hook #'lsp)

(setq lsp-headerline-breadcrumb-segments '(symbols))
(setq lsp-modeline-code-actions-segments '(name))
(setq lsp-keep-workspace-alive nil)
(setq lsp-warn-no-matched-clients nil)

;; clangd config
(setq lsp-clients-clangd-args
      '("--header-insertion=never" "--query-driver=/usr/bin/g++"))

;; pylsp config
(setq lsp-pylsp-plugins-pydocstyle-enabled nil)
(setq lsp-pylsp-plugins-flake8-max-line-length 100)

;; html-ls config
(setq lsp-html-format-enable nil)

(require 'company)
(global-company-mode)
(setq company-format-margin-function nil)

(require 'neotree)
(add-hook 'neotree-mode-hook 'doom-modeline-mode)
(add-hook 'neotree-mode-hook 'hl-line-mode)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-theme 'nerd)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(setq web-mode-engines-alist '(("django" . "\\.html\\'")))
(setq web-mode-markup-indent-offset 4)

;; Custom keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)
(global-set-key (kbd "M-*") 'compile)
(global-set-key (kbd "C-x n") 'make-empty-file)
(global-set-key (kbd "C-j") 'duplicate-line)
;; buffer move
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)
