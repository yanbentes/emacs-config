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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" default)))
 '(package-selected-packages (quote (evil monokai-theme smex use-package))))
(custom-set-faces)

;; smex keybindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(put 'downcase-region 'disabled nil)
