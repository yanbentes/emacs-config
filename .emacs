;; disable tool bar and startup screen
(tool-bar-mode 0)
(setq inhibit-startup-screen t)

;; default packages
(require 'package)
(setq package-archives '(("elpa"  . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package org)

(custom-set-variables
 '(custom-enabled-themes (quote (gruber-darker)))
 '(custom-safe-themes
   (quote
    ("03e26cd42c3225e6376d7808c946f7bed6382d795618a82c8f3838cd2097a9cc" default))))
(custom-set-faces)
