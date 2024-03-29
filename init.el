;; customize

(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load-file custom-file)

;; extra functions

(load-file "~/.emacs.d/emoji-utils.el")
(load-file "~/.emacs.d/project-utils.el")

;; options

(defcustom compilation-colors-hack nil
  "Non-nil if compilation colors are not shown as expected."
  :group 'display
  :type 'boolean
  )

(defcustom dead-keys-hack nil
  "Non-nil if dead keys does not work as expected."
  :group 'keyboard
  :type 'boolean
  )

(defcustom keyboard-layout 'qwerty
  "The choice of keyboard layout changes some key bindings to be more natural."
  :group 'keyboard
  :type '(choice
          (const :tag "Qwerty" qwerty)
          (const :tag "Dvorak" dvorak)
          )
  )

;; ui elements

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq inhibit-startup-screen t)

;; fixes

;;;; dead keys not working?
;;;; https://www.emacswiki.org/emacs/DeadKeys
(when dead-keys-hack
  (require 'iso-transl)
  )

;;;; compilation colors broken?
;;;; https://stackoverflow.com/a/3072831
(when compilation-colors-hack
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  )

;; package

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; themes and styles

(load-theme
 (if (display-graphic-p)
     'wombat
   'manoj-dark))
                                        ; highlight matching paren
(show-paren-mode t)
                                        ; show line and column number
(setq column-number-mode t)
                                        ; make trailing whitespace red
(setq-default show-trailing-whitespace t)

;; navigation

(when (require 'ido nil t)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode t)
  )

(windmove-default-keybindings)
;;;; https://orgmode.org/manual/Conflicts.html
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(when (require 'ace-window nil t)
  (cl-case keyboard-layout
    ('dvorak (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))
    ('qwerty (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
    )
  (define-key global-map (kbd "C-x o") 'ace-window)
  )

;; shortcuts

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; magit

(when (require 'magit nil t)
  (setq initial-buffer-choice (lambda () (magit-status-setup-buffer)))
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  )

;; avy

(when (require 'avy nil t)
  (when (eq keyboard-layout 'dvorak)
    (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))
  (define-key global-map (kbd "M-g M-c") 'avy-goto-char)
  (define-key global-map (kbd "M-g M-g") 'avy-goto-line)
  )

;; multiple cursors

(when (require 'multiple-cursors nil t)
  (define-key global-map (kbd "C-S-n") 'mc/mark-next-like-this)
  (define-key global-map (kbd "C-S-p") 'mc/mark-previous-like-this)
  (define-key global-map (kbd "C-S-a") 'mc/mark-all-like-this)
  )

;; formatting

(setq-default tab-width 6
              indent-tabs-mode nil)

(defun init-prog-mode ()
  (linum-mode)
  (font-lock-add-keywords nil '(("\\s< \\(idea\\)" 1 'font-lock-string-face prepend)
                                ("\\s< \\(note\\)" 1 'font-lock-keyword-face prepend)
                                ("\\s< \\(todo\\)" 1 'font-lock-constant-face prepend)))
  )
(add-hook 'prog-mode-hook 'init-prog-mode)

(when (require 'cc-mode nil t)
  (setq-default c-basic-offset 3)
  (defun init-cc-mode ()
    (setq tab-width 3
          indent-tabs-mode t
          c-default-style "linux"
          c-auto-align-backslashes nil)
    )
  (add-hook 'c-mode-common-hook 'init-cc-mode)
  (add-hook 'c-mode-common-hook #'linum-mode)
  (defun init-c++-mode ()
    ;; makes vvvv
    ;; void foo()
    ;;     {
    ;; ^^^^ into vvvv
    ;; void foo()
    ;; {
    ;; ^^^^
    (c-set-offset 'inline-open 0)
    )
  (add-hook 'c++-mode-hook 'init-c++-mode)
  )

(when (require 'cmake-mode nil t)
  (setq-default cmake-tab-width 3)
  (defun init-cmake-mode ()
    (setq tab-width 3
          indent-tabs-mode t)
    )
  (add-hook 'cmake-mode-hook 'init-cmake-mode)
  (when (require 'smart-tabs-mode nil t)
    (smart-tabs-add-language-support cmake cmake-mode-hook
      ((cmake-indent . cmake-tab-width))))
  )

(when (require 'smart-tabs-mode nil t)
  (smart-tabs-insinuate 'c 'c++ 'javascript 'python 'cmake)
  )

;; yasnippet

(when (require 'yasnippet nil t)
  (yas-global-mode t)
  )

;; company

(when (require 'company nil t)
  (setq company-idle-delay 0.1)
  (global-company-mode)
  )

(when (require 'company-emoji nil t)
  (setq company-emoji-insert-unicode nil)
  (setq company-emoji--possible-emojis (get-possible-emojis))
  (defun company-emoji--in-comment (command &optional arg &rest ignored)
    "Original function."
    (when (in-comment-p (point))
      (company-emoji command arg ignored)))
  (defun company-emoji--annotation (s)
    "Default behavior overridden."
    (let ((name (substring s 1 -1)))
      (when (cl-some (lambda (x) (member name x)) company-emoji--possible-emojis)
        (format " <GH>"))))
  (add-to-list 'company-backends 'company-emoji--in-comment)
  (add-to-list 'company-backends 'company-emoji t)
  ;;;; make sure dabbrev is last, else emojis will not work in e.g. git commits
  (delete 'company-dabbrev company-backends)
  (add-to-list 'company-backends 'company-dabbrev t)
  )

;; org mode

(setq
 org-src-fontify-natively t
 org-src-tab-acts-natively t
 org-confirm-babel-evaluate nil
 org-edit-src-content-indentation 0
 org-enforce-todo-dependencies t
 )

;; compile

(setq compilation-scroll-output 'first-error)

;; emojify

(when (require 'emojify nil t)
  (global-emojify-mode)
  (setq emojify-point-entered-behaviour 'uncover)
  )

;; lsp mode

(when (require 'lsp-mode nil t)
  (setq lsp-idle-delay 0.1)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-diagnostic-package :none)
  (setq lsp-completion-enable-additional-text-edit nil) ; disable adding includes
  (setq lsp-enable-on-type-formatting nil)
  (when (require 'cc-mode nil t)
    (add-hook 'c-mode-common-hook 'lsp)
    )
  (define-key lsp-mode-map (kbd "C-c") lsp-command-map)
  )

(when (require 'lsp-treemacs nil t)
  (setq treemacs-space-between-root-nodes nil)
  )

