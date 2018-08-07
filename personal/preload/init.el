;;

;;
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)

;; prefer utf-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; my theme
;; (setq prelude-theme 'darkokai-theme)
(setq prelude-theme nil)
