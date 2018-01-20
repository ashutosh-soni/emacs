;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (cd "~/workspace")

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path (expand-file-name "modules" prelude-personal-dir))

;; install my packages
(defvar my-packages
  '(paredit
    aggressive-indent
    cider
    flycheck
    flycheck-pos-tip
    flycheck-joker
    clj-refactor
    clojure-mode-extra-font-locking
    magit-gitflow
    ;; company-quickhelp
    darkokai-theme
    highlight-symbol
    all-the-icons
    neotree))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; no need for ~ files when editing
(setq create-lockfiles nil)

;; disable flyspell
(setq prelude-flyspell nil)

;; disable auto save of prelude
(setq prelude-auto-save nil)

;; disable clean whitesapce on save
(setq prelude-clean-whitespace-on-save nil)

;; Show line numbers
(global-linum-mode)

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(require 'darkokai-theme)
(setq darkokai-mode-line-padding 1)
(load-theme 'darkokai t)

;; increase font size for better readability
(set-frame-font "Input-11")
(add-to-list 'default-frame-alist '(font . "Input-11"))

;;;;;;;;;; global key binding ;;;;;;;;;;

;; hot key for switching window
(global-set-key (kbd "M-p") 'ace-window)

;; short cut for vc refresh state
(global-set-key (kbd "C-x v 0") 'vc-refresh-state)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; ;; shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; comments
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(define-key flyspell-mode-map (kbd "C-;") nil)

;; enable git flow
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; enable popup doc on pause in company menu
;; (require 'company-quickhelp)
;; (company-quickhelp-mode 1)

;; override smart mode line configuration
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'dark)
(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/name-width 40)
(setq sml/mode-width 'full)
(add-hook 'after-init-hook #'sml/setup)

;; aggressive indent
;; (require 'aggressive-indent)
;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'css-mode #'aggressive-indent-mode)
;; (add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;; Syntax highlighting
(require 'highlight-symbol)
(global-set-key (kbd "C-.") 'highlight-symbol-at-point)
(define-key flyspell-mode-map (kbd "C-.") nil)

;; load icons
(require 'all-the-icons)

;; short cut for neotree toggle
(require 'neotree)
(setq neo-theme 'icons)
(global-set-key [f8] 'neotree-toggle)

(require 'smartparens)
(smartparens-global-mode)

(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

(require 'clj-refactor)
(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            ;; insert keybinding setup here
            (cljr-add-keybindings-with-prefix "C-c C-m")))

;; enable pretty lambda (replace fn keyword with greek letter)
(require 'clojure-pretty-lambda)
(add-hook 'clojure-mode-hook 'clojure-pretty-lambda-mode)
(add-hook 'cider-repl-mode-hook 'clojure-pretty-lambda-mode)

;; enable cider repl pprint using fipp
(setq cider-pprint-fn 'fipp)
(setq cider-repl-use-pretty-printing t)

;; add shot cut for cider-repl-clear-buffer
(define-key cider-repl-mode-map (kbd "C-c SPC") 'cider-repl-clear-buffer)

(require 'flycheck)
(require 'flycheck-pos-tip)
(require 'flycheck-joker)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode)

;; flycheck-pos-tip
(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(add-hook 'cider-mode-hook
          (lambda ()
            (setq next-error-function #'flycheck-next-error-function)))
