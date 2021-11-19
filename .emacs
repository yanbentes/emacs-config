;; disable tool bar
(tool-bar-mode 0)

;; selected theme
(custom-set-variables '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages (quote (use-package))))
(custom-set-faces)

;; default packages
(require 'package)
(setq package-archives '(("elpa"  . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; use-package
;;(require 'use-package)
;;(setq use-package-always-ensure t)
