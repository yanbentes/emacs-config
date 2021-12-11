(tool-bar-mode 0)
(ido-mode 1)
(setq inhibit-startup-screen t)
;; stop creating ~ files
(setq make-backup-files nil) 

;; default packages repositories
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
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages (quote (use-package smex dracula-theme))))
(custom-set-faces)

;; smex keybindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(put 'downcase-region 'disabled nil)

