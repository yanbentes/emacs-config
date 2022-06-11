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

(load "~/.emacs.d/early-init.el")

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

;; emacs gui
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(setq inhibit-startup-screen t)
(setq make-backup-files -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode 1)
(set-fringe-mode 10)
(ido-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode -1)

;; (load-theme 'monokai t)
;; (load-theme 'dracula t)
(load-theme 'gruber-darker t)

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

(require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)

(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)
(setq company-idle-delay 0)
(setq company-show-numbers nil)

(require 'keycast)
;; (keycast-mode t)
(set-face-attribute 'keycast-key nil
                    :weight 'normal
                    :box nil
                    :foreground "#FFF"
                    :background  "#1c1c1c")

(require 'rainbow-mode)
(rainbow-mode 1)

(require 'evil)
;; (evil-mode 1)

;; TODO org config
(require 'org)
(require 'org-contrib)
(setq org-startup-indented t)

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-agenda-files
      '("~/Documents/org/todos.org"))

(require 'doom-modeline)
(doom-modeline-mode 1)

;; other keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
