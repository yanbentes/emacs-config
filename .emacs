(require 'package)
;; include MELPA repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; Disables startup screen and tool bar
(setq inhibit-startup-screen t)
(tool-bar-mode 0)
