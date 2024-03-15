;; Add the stable repository of Melpa to dw packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Configures use-package
(eval-when-compile (require 'use-package))

;; Forces use-package to download unknown packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Always display line numbers
(global-display-line-numbers-mode 1)

;; Always refresh buffers when their content changes on disk
(global-auto-revert-mode t)

;; Always start emacs full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Searches and loads agda-mode
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; This allows the documentation to be displayed in minibuffer by default
;; or in a dedicated buffer when it is visible
(defun my/eldoc-display-in-buffer-or-minibuffer (&rest args)
  (apply
   (if (and eldoc--doc-buffer
            (seq-some (lambda (w) (eq (window-buffer w) eldoc--doc-buffer)) (window-list)))
       'eldoc-display-in-buffer
     'eldoc-display-in-echo-area)
   args))

(setq eldoc-display-functions '(my/eldoc-display-in-buffer-or-minibuffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Below everything is handled by yse-package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensures all packages are kept up to date
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Highlightning current and inner parentheses
(use-package highlight-parentheses
  :init (show-paren-mode)
  :config (global-highlight-parentheses-mode)
  :custom ((highlight-parentheses-background-colors '("black"))
	   (Highlight-parentheses-colors '("red")))
  :diminish)

;; Splitting vertically by default
(setq split-width-threshold 0)

;; Using doom-vibrant as default theme
;; This should me modified for Agda where it does not fit too well
(use-package doom-themes
  :init
  (load-theme 'doom-vibrant))

;; Attempt at improving Emacs' scroll behaviour
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

;; Tuareg mode for caml files
(use-package tuareg
  :mode "\\.ml[iylp]?")

;; Yasnippet used globally. This is sometimes an issue
;; when tab is bound to several commands
(use-package yasnippet
  :config (yas-global-mode t))

;; Haskell mode for .hs files
(use-package haskell-mode
  :mode "\\.hs")

;; This is required to use the :diminish idiom in use-packages
;; which hides some packages in the lower emacs bar
(use-package diminish)

;; Using fira-code allows us to have nice ligatures, very
;; fancy in haskell for instance. This is useless in Agda though
;; where unicode is supported by default
(use-package fira-code-mode
  :config (global-fira-code-mode)
  :custom (fira-code-mode-disabled-ligatures '("x"))
  :diminish)

;; Markdown for .md files. This is especially useful for documentation
;; buffers coming for language servers
(use-package markdown-mode)

;; Using company for Haskell, agda and lisp.
;; Will possibly become global in the futur
(use-package company
  :hook ((haskell-mode agda2-mode emacs-lisp-mode) . company-mode))

;; Client for language servers, used for hls in this case.
;; This looks for a wrappers if any, and falls off to hls otherwise
;; in an attempt to find the right executable.
(use-package eglot
  :hook (haskell-mode . eglot-ensure)
  :bind (:map eglot-mode-map
	      ("C-e e" . eldoc-doc-buffer)
	      ("C-e o" . eglot-format-buffer)
	      ("C-e d" . xref-find-definitions)
	      ("C-e b" . xref-go-back)
	      ("C-e f" . xref-go-forward)
	      ("C-e n" . flymake-goto-next-error)
	      ("C-e p" . flymake-goto-prev-error)
	      ("C-e a" . eglot-code-actions)
	      ("C-e r" . eglot-rename)
	      )
  :config
  (let ((hls (if (executable-find "haskell-language-server-wrapper") "haskell-language-server-wrapper" "haskell-language-server")))
    (add-to-list 'eglot-server-programs `(haskell-mode ,hls "--lsp")))
  :custom (eglot-confirm-server-initiated-edits nil)
  )

;; This limits the number of buffers dired uses to a single one
;; Never quite understood why this is not the default behaviours
(use-package dired-single)

;; A simple mode for nix files.
(use-package nix-mode
  :mode "\\.nix\\'")
