;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set package archive list
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path (expand-file-name "modules" prelude-personal-dir))

;; install my packages
(defvar my-packages
  '(org-plus-contrib
    org-bullets
    org-tree-slide
    ;; moom
    ;; org-jira

    paredit
    ;; aggressive-indent
    cider
    flycheck
    flycheck-pos-tip
    flycheck-joker
    clj-refactor
    clojure-mode-extra-font-locking
    magit-gitflow
    markdown-mode
    ;; company-quickhelp
    darkokai-theme
    highlight-symbol
    all-the-icons
    neotree
    rich-minority ;; smart-mode-line needs it ??
    smart-mode-line
    delight))

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
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode))


;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(require 'darkokai-theme)
(setq darkokai-mode-line-padding 1)
(load-theme 'darkokai t)


(defface org-block-begin-line
  '((t (:foreground "#008ED1" :background "#333")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block
  '((t (:foreground "#FFFFEA" :background "#000")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:foreground "#008ED1" :background "#333")))
  "Face used for the line delimiting the end of source blocks.")

(custom-theme-set-faces
 'darkokai
 '(org-block-begin-line
   ((t (:foreground "#008ED1" :background "#333"))))
 '(org-block
   ((t (:foreground "#FFFFEA" :background "#000"))))
 '(org-block-end-line
   ((t (:foreground "#008ED1" :background "#333")))))


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

;; aggressive indent
;; (require 'aggressive-indent)
;; (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'css-mode #'aggressive-indent-mode)

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
(require 'cider)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

(require 'clj-refactor)
(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            (yas-minor-mode 1) ; for adding require/use/import statements
            ;; insert keybinding setup here
            ;; NOTE: this choice leaves cider-macroexpand-1 unbound
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

;; cider mode use flycheck
(add-hook 'cider-mode-hook
          (lambda ()
            (setq next-error-function #'flycheck-next-error-function)))

;; cider repl mode hide line numbers
(add-hook 'cider-repl-mode
          (lambda ()
            (linum-mode -1)
            (display-line-numbers-mode -1)))

;; cider mode enable history file
(setq cider-repl-history-file "~/.cider-repl-history")

;;;; org mode config
(require 'org)
(require 'cider)
(require 'org-tree-slide)
;; (require 'moom)
;; (require 'org-jira)

;; (require 'ob-clojure-literate)
;; (add-hook 'org-mode-hook #'ob-clojure-literate-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)
   (shell . t)))
;; specify clojure back-end to use in org-mode
(setq org-babel-clojure-backend 'cider)
;; useful keybindings when using clojure in org-mode
(org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)
(org-defkey org-mode-map "\C-c\C-d" 'cider-doc)
(setq org-babel-clojure-sync-nrepl-timeout nil)
;; turn on visual-line-mode for org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (linum-mode -1)
            (display-line-numbers-mode -1)
            (whitespace-mode -1)
            (turn-on-visual-line-mode)))
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation nil)
(setq org-pretty-entities t)
(set-face-attribute 'org-meta-line nil
                    :height 0.8
                    :slant 'normal
                    ;; :foreground "black"
                    :weight 'light)
;; org-bullets mode
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; override smart mode line configuration
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'dark)
(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/name-width 40)
(setq sml/mode-width 'full)
(add-hook 'after-init-hook #'sml/setup)

;; hide minor modes
;; (require 'rich-minority)
;; (setq rm-blacklist ".*") ;; just hide all
;; (setq rm-whitelist nil)  ;; none to show

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq rm-blacklist                                        ;;
;;       (format "^ \\(%s\\)$"                               ;;
;;               (mapconcat #'identity                       ;;
;;                          '("Fly.*" "Projectile.*" "PgLn") ;;
;;                          "\\|")))                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'delight)
(delight '((guru-mode "" t)
           (paredit-mode "" t)
           (flycheck-mode "" t)
           (company-mode "" t)
           (helm-mode "" t)
           (editorconfig-mode "" t)
           (whitespace-mode "" t)
           (projectile-mode "" t)
           (smartparens-mode "" t)
           (prelude-mode "" t)
           (which-key-mode "" t)
           (beacon-mode "" t)
           (subword-mode "" t)
           (clj-refactor-mode "" t)
           (cider-mode "" t)
           (magit-gitflow-mode "" t)
           (abbrev-mode "" t)
           (subword-mode "" t)
           (visual-line-mode "" t)))

(require 'diminish)
(eval-after-load "guru-mode" '(diminish 'guru-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAMP
(customize-set-variable
 'tramp-ssh-controlmaster-options
 (concat
  "-o ControlPath=/tmp/ssh-master-%%r@%%h:%%p "
  "-o ControlMaster=auto "
  "-o ControlPersist=yes"))
