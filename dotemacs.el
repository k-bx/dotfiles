;; useful stuff I keep forgetting:
;; - visual-line-mode
;; - visual-fill-column-mode
;; - display-line-numbers-mode
;; - toggle-truncate-lines

;; (setq debug-on-error t)
;; (setq debug-on-quit t)
(require 'package)
(setq package-archives
	  '(("elpa" . "https://tromey.com/elpa/")
		("gnu" . "https://elpa.gnu.org/packages/")
		;; ("marmalade" . "https://marmalade-repo.org/packages/")
		("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; auto-complete
(setq package-list 
  '(ac-geiser
    adaptive-wrap 
    add-node-modules-path 
    bind-key 
    color-theme 
    company
    ; company-ghc
    company-go 
    company-jedi 
    csharp-mode 
    csharp-mode 
    cubicaltt 
    dante 
    dash
    deft 
    dhall-mode 
    dockerfile-mode 
    drag-stuff 
    elm-mode 
    epc 
    esup
    exec-path-from-shell
    expand-region 
    feature-mode 
    flatbuffers-mode
    flx-ido 
    flycheck 
    flycheck-elm 
    flycheck-haskell
    flycheck-hdevtools 
    flycheck-rust 
    flymake 
    flymake-cursor 
    flymake-haskell-multi
    flymake-hlint
    format-all 
    ;; fuzzy-match
    geiser-guile
    ; ghc 
    go-mode 
    groovy-mode
    helm
	just-mode
    guess-style
    haml-mode 
    haskell-mode 
    hasklig-mode 
    ;; hexrgb
    idris-mode 
    ; intero 
    jedi 
    js2-mode
    json-reformat
    lsp-mode
    magit 
    markdown-mode 
    multiple-cursors 
    nix-mode 
    nlinum 
    ormolu 
    persistent-scratch 
    popwin
    ;; pretty-lambdada
    protobuf-mode
    projectile 
    projectile-ripgrep 
    proof-general 
    rainbow-delimiters
    ripgrep
    rust-mode 
    s 
    smex 
    smooth-scroll 
    smooth-scrolling
    solarized-theme 
    sql-indent 
    string-inflection 
    sublime-themes 
    sublimity 
    tabbar
    tide
    ; timu-macos-theme
    tss 
    tuareg 
    typescript-mode 
    use-package 
    visual-fill-column 
    web-mode
    wrap-region 
    yaml-mode 
    yasnippet 
    zenburn-theme
))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; (setq gc-cons-threshold 100000000)

;; (set-frame-font "Ubuntu Mono-9")
(set-frame-font "Ubuntu Mono-11")
;; (set-frame-font "Hasklig-8")
;; (set-frame-font "Hasklig-9.5")
;; (set-frame-font "Menlo-8")
(defun big0 () (interactive) (set-frame-font "Ubuntu Mono-9"))
(defun big1 () (interactive) (set-frame-font "Ubuntu Mono-10"))
(defun big2 () (interactive) (set-frame-font "Ubuntu Mono-11"))
(defun big3 () (interactive) (set-frame-font "Ubuntu Mono-12"))
(defun big4 () (interactive) (set-frame-font "Ubuntu Mono-13"))
(defun big5 () (interactive) (set-frame-font "Ubuntu Mono-14"))
(defun big6 () (interactive) (set-frame-font "Ubuntu Mono-15"))

(blink-cursor-mode 0)
;; (setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))

(when (eq system-type 'darwin)
  (set-frame-font "Menlo-12")
  (setq frame-title-format "")
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char)
  (when (not (eq window-system nil))
    (ns-set-resource nil "ApplePressAndHoldEnabled" "NO")))
(when (not (eq system-type 'darwin))
  (menu-bar-mode -1))

(setq ring-bell-function 'ignore)

(global-set-key [S-M-down] 'win-resize-mi2nimize-vert)
(global-set-key [S-M-up] 'win-resize-enlarge-vert)
(global-set-key [S-M-left] 'win-resize-minimize-horiz)
(global-set-key [S-M-right] 'win-resize-enlarge-horiz)
(global-set-key [S-M-up] 'win-resize-enlarge-horiz)
(global-set-key [S-M-down] 'win-resize-minimize-horiz)
(global-set-key [S-M-left] 'win-resize-enlarge-vert)
(global-set-key [S-M-right] 'win-resize-minimize-vert)
(global-set-key [?\C-,] 'previous-buffer)
(global-set-key [?\C-.] 'next-buffer)
(global-set-key [C-tab] 'shift-right)
(global-set-key [C-S-tab] 'shift-left)
(global-set-key [C-iso-lefttab] 'shift-left)

(when (not (eq window-system nil))
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(defun positionise ()
  (interactive)
  (if (window-system)
      (if (eq system-type 'darwin)
          (progn
            (set-frame-size (selected-frame) 85 45)
            (set-frame-position (selected-frame) 855 0))
        ;; (progn
        ;;   (set-frame-size (selected-frame) 54 28)
        ;;   (set-frame-position (selected-frame) -1 0))))
        (progn
          (set-frame-size (selected-frame) 100 50)
          (set-frame-position (selected-frame) -1 0)))))

(positionise)

(setq load-path (cons "~/.emacs.d/elisp" load-path))
(setq load-path (cons "~/.emacs.d/elisp/lilypond" load-path))
(require 'kb-utils)
(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(global-font-lock-mode t)

;; ;; cask
;; (require 'cask "~/.cask/cask.el")
;; (cask-initialize)

(require 'exec-path-from-shell)
(setq exec-path-from-shell-arguments '("-l"))
(exec-path-from-shell-initialize)

;; Change backup behavior to save in a directory, not in a miscellany
;; of files all over the place.
(setq backup-by-copying t
      backup-directory-alist
      '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 600
      kept-old-versions 200
      version-control t
      vc-make-backup-files t)
(setq auto-mode-alist (cons '("\.emacs" . lisp-mode) auto-mode-alist))
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(column-number-mode t)
(setq temporary-file-directory "~/tmp")
(add-hook 'after-save-hook 'autocompile-dotemacs)
(setq inhibit-splash-screen t)

;; navigation with M-`Arrow keys`
(windmove-default-keybindings 'meta)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-j") 'windmove-down)

;; Spaces instead of tabs
;; (setq c-basic-indent 4)
(setq tab-width 4)
;; (setq indent-tabs-mode nil)
;; (setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; (setq indent-tabs-mode nil)
;; (infer-indentation-style)
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)
(require 'guess-style)
(add-hook 'markdown-mode-common-hook 'guess-style-guess-all)
;; (global-guess-style-info-mode 1)

(global-set-key [f11] 'switch-full-screen)

(electric-indent-mode 0)

;; html to give you 4 spaces indent
(setq sgml-basic-offset 4)

;; copypaste to X buffer
(setq x-select-enable-clipboard t)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi)
  ;; (setq indent-tabs-mode t)
  (adaptive-wrap-prefix-mode t)
  (setq tab-width 4)
  (setq python-indent-offset 4)
  )
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; set a beautiful title bar
(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))
(setq frame-title-format
      '("%S: " (buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(global-auto-revert-mode)

;; Disable C-z madness
(global-set-key (kbd "C-z") 'undo)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; (require 'column-marker)
;; (add-hook 'text-mode-hook (lambda () (interactive) (column-marker-1 79)))
;; (add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 79)))
;; (add-hook 'html-mode-hook (lambda () (interactive) (column-marker-1 79)))
;; (add-hook 'haskell-mode-hook (lambda () (interactive) (column-marker-1 79)))

;;;; smooth scrolling
;; (require 'smooth-scroll)
;; (smooth-scroll-mode 1)
;; (setq smooth-scroll/vscroll-step-size 5)
;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (sublimity-mode 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(require 'flx-ido)
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

;; Use ido to list tags, but then select via etags-select (best of both worlds!)
;; (require 'etags-select)
(require 'helm)
(global-set-key (kbd "M-.") 'my-find-tag)
(global-set-key (kbd "M-,") 'helm-etags-select)
;; (global-set-key (kbd "M-,") 'my-find-tag)
;; (define-key global-map (kbd "RET") 'helm-etags-select)
(define-key global-map (kbd "M-*") 'pop-tag-mark)

(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-x g") 'magit-status)

(setq custom-theme-load-path (cons "~/.emacs.d/themes" custom-theme-load-path))

(defun disable-all-themes ()
  (interactive)
  (disable-theme 'k-bx)
  (disable-theme 'k-bx-2)
  (disable-theme 'solarized-light)
  (disable-theme 'solarized-dark)
  (disable-theme 'solarized-light-high-contrast)
  (disable-theme 'solarized-dark-high-contrast)
  (disable-theme 'solarized-zenburn))
(defun dark ()
  (interactive)
  (disable-all-themes)
  (customize-set-variable 'frame-background-mode 'dark)
  (load-theme 'k-bx-2 t)
  (load-theme 'k-bx t))
(defun dark2 ()
  (interactive)
  (disable-all-themes)
  (customize-set-variable 'frame-background-mode 'dark)
  (load-theme 'solarized-dark-high-contrast t))
(defun dark3 ()
  (interactive)
  (disable-all-themes)
  (customize-set-variable 'frame-background-mode 'dark)
  (load-theme 'zenburn t))
(defun dark4 ()
  (interactive)
  (disable-all-themes)
  (customize-set-variable 'frame-background-mode 'dark)
  (load-theme 'misterioso t))

(defun light ()
  (interactive)
  (disable-all-themes)
  (customize-set-variable 'frame-background-mode 'light)
  (load-theme 'solarized-light-high-contrast t))
(defun light2 ()
  (interactive)
  (disable-all-themes)
  (customize-set-variable 'frame-background-mode 'light)
  (load-theme 'solarized-selenized-white t))
  ;; (load-theme 'adwaita t))

(defun light3 ()
  (interactive)
  (disable-all-themes)
  (customize-set-variable 'frame-background-mode 'light)
  (load-theme 'light-blue t))
  ;; (load-theme 'mccarthy t))
(defun light4 ()
  (interactive)
  (disable-all-themes)
  (customize-set-variable 'frame-background-mode 'light)
  (load-theme 'ritchie t))

;; (dark)

(add-hook 'python-mode-hook (lambda () (turn-on-subword-mode)))

;; (require 'adaptive-wrap)

;;;; haskell-mode
;; (setq load-path (cons "~/workspace/haskell-mode" load-path))
(require 'haskell-mode)
(require 'ghcid)

;; (setq load-path (cons "~/workspace/ghc-mod/elisp" load-path))
;; (setq ghc-debug t)
;; (autoload 'ghc-init "ghc" nil t)
;; ;; (setq ghc-report-errors nil)
;; ;; (autoload 'ghc-debug "ghc" nil t)

(require 'ormolu)

  ;; (if (and (boundp 'dante-mode) (= dante-mode t))

(defun dante-enable-or-restart ()
  (interactive)
  (if (bound-and-true-p dante-mode)
      (progn
        (interactive)
        (message "> restarting dante")
        (dante-mode t)
        (dante-restart)
        )
      (progn
        (interactive)
        (message "> launching dante mode")
        (dante-mode t)
        ))
  )

(defun my-haskell-mode-hook ()
  (interactive)
  (turn-on-subword-mode)
  ;; (hasklig-mode)
  (interactive-haskell-mode)
  (adaptive-wrap-prefix-mode t)
  ;; (ghc-init)
  (local-set-key "\C-c\C-s" 'ormolu-format)
  ;; (local-set-key "\M-q" 'hindent-reformat-decl-or-fill)
  ;;(local-set-key "\C-c\C-c" 'haskell-compile)
  (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-compile)
  ;; (define-key haskell-mode-map (kbd "C-c C-x") 'dante-restart)
  (bind-key* (kbd "C-c C-x") 'dante-enable-or-restart)
  ;; (define-key haskell-process-cabal-build (kbd "C-c C-c") 'haskell-compile)
  ;; (local-set-key "\C-c\C-c" (lambda () (interactive) (haskell-compile)))
  ;; (intero-mode)
  ;; (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  ;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  ;; (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  ;; (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  ;; (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  ;; (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (lambda () (interactive) (column-marker-1 79))
  (setq haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans -fshow-loaded-modules"))
  )

(defun my-rust-mode-hook ()
  (interactive)
  ;; (local-set-key "\C-c\C-d" 'rust-compile)
  (bind-key* (kbd "C-c C-d") 'rust-check)
  (local-set-key "\C-c\C-s" 'rust-format-buffer)
  (setq indent-tabs-mode nil)
  (turn-on-subword-mode)
  )
(add-hook 'rust-mode-hook 'my-rust-mode-hook)

;; docs https://agda.readthedocs.io/en/latest/tools/emacs-mode.html
;; (setq load-path (cons "~/workspace/agda/src/data/emacs-mode" load-path))
;; (require 'agda2-mode)
;; (add-hook 'agda2-mode-hook
;;           '(lambda ()
;;             ; If you do not want to use any input method:
;;             (deactivate-input-method)
;;             ; (In some versions of Emacs you should use
;;             ; inactivate-input-method instead of
;;             ; deactivate-input-method.)

(require 'lsp-mode)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-elm))
(require 'elm-mode)
(defun my-elm-mode-hook ()
  (turn-on-subword-mode)
  (adaptive-wrap-prefix-mode t)
  (define-key elm-mode-map (kbd "\C-c\C-s") 'elm-mode-format-buffer)
  (setq elm-interactive-command '("elm" "repl")
        elm-reactor-command '("elm" "reactor")
        elm-compile-command '("elm" "make")
        elm-package-command '("elm" "package"))
  (elm-indent-simple-mode)
  (elm-oracle-setup-completion)
  )
(add-hook 'elm-mode-hook 'my-elm-mode-hook)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'lsp))

(setq flycheck-check-syntax-automatically '(save mode-enabled))

;;;; flycheck
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; (eval-after-load 'flycheck #'javascript-flow-flycheck-init)
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(define-key global-map (kbd "M-p") 'flycheck-previous-error)
(define-key global-map (kbd "M-n") 'flycheck-next-error)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
;; (with-eval-after-load 'flycheck
;;       '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

;; jsx

(setq-default web-mode-comment-formats
              '(("java"       . "/*")
                ("javascript" . "//")
                ("php"        . "/*")))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (add-node-modules-path)
  (company-mode +1))
;; ;; aligns annotation to the right hand side
;;(setq company-tooltip-align-annotations t)
;; formats the buffer before saving
;;(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(setq auto-mode-alist 
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

(require 'yasnippet)
(yas-global-mode 1)

;; projectile
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-enable-caching t)
(setq projectile-mode-line
         '(:eval (format " Projectile[%s]"
                        (projectile-project-name))))
(defun my-projectile-switch-project-action ()
  (interactive)
  (add-to-list 'tags-table-list (concat (projectile-project-root) "TAGS"))
  (call-interactively 'projectile-dired))
(setq projectile-switch-project-action 'my-projectile-switch-project-action)
(define-key global-map (kbd "C-c p s r") 'projectile-ripgrep)

;; markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(defun my-markdown-mode-hook ()
  (interactive)
  (adaptive-wrap-prefix-mode t)
  (toggle-word-wrap t)
  )
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

(require 'server)
(or (server-running-p) (server-start))

(require 'deft)

(global-set-key [?\C-x ?\O] 'frame-bck)

;;;; my python unittest integration
(require 'python-get-runcmd-for-current-unit-test)

;; (add-hook 'find-file-hook 'adaptive-wrap-prefix-mode)
(add-hook 'find-file-hook 'visual-line-mode-activate)
;; (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
(add-hook 'visual-line-mode-hook 'visual-line-mode-set-logical-line-movement)

(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)
(eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; ;; Auto complete
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (define-key ac-completing-map "\M-/" 'ac-stop)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;; (setq split-width-threshold 1)
;; (setq split-height-threshold nil)

(setq tramp-default-method "ssh")

;; (load-file "~/workspace/cubicaltt/cubicaltt.el")
(autoload 'cubicaltt-mode "cubicaltt" "cubical editing mode" t)
(setq auto-mode-alist (append auto-mode-alist '(("\\.ctt$" . cubicaltt-mode))))

;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))

(require 'lilypond-mode)
(setq auto-mode-alist (append auto-mode-alist '(("\\.ly$" . LilyPond-mode))))

; https://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("59820d0ca7ba29984ab4d255f9fcfaa28041ad6ae4b6ae2ffb2ccfbfaec210e4" "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "a3e99dbdaa138996bb0c9c806bc3c3c6b4fd61d6973b946d750b555af8b7555b" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "1a2fc02ca35192b1447eccd65f10a57373139cf2217b1b65e8d669b33714405b" "3fc732860755c3d1e0e707c56908e37e70df5ef6caef1a1239f476fb656ec8bb" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "00aad3ad602b79bc7132ce0ab68be7eefa4b05fbe9c7956636c98eb0260fb21c" "52fd62dad833cc5feed9ac6028f23d66b2ea00ca3d1c4f05d7a25c2eb0f7ab18" "b48904500d6e652a391f6bf25d52f1be11284ebfd5f7284e87f2b97b23a7dad6" "41c60cc8084c176dffefdea6cdf96a098da260624c454a610121571b7f0d5727" "27d99e38ac398c1d6a4b9fd027cc0cd2bbf53de9da13a1306f9ecdb70d8fa9dd" "144f297981a22c35e14fa0a73748c6d71a7accc031e2cb97785f3782ff236b1b" "0681a3c8f9b8187a63070e59672b1df428c1695c56f64bbe0b69b10b01291f67" "1927fbaf301d12eb45f8b68be6992dba75c0153a86a32cba0f1571c223e38c44" "807fcb675c5c20fac8a23854d07557050049a9502fc87166ac84c9bac6dc32b7" "fda96eb444c409c9d7a2eb1c5a08911dba6559c5e74cc378da397e390d6261b0" "ffb7dcb4f01bdb13825577fb686422dd9c2e65f56bc12029c9147db53c56fa56" "19148913c2ac4d941fac49c1a415293890f98037d53bb899635a98ff80cc2da0" "ccc7f955188fd39d347ea6f5345fed986ae66e054b3da9b23b5c336026e3c757" "59ed08918a5739ec41af343b67a5a4c7cb17b31da49ed409170d1b45c1ccd0cf" "f44a3a5f5f365768a8ac5e17a8b3a4ac05434fb3389afc6f531612382b4340b9" "22fdf3a487954a54eccf91748bef751877687178b936501a2079b51ae056b6cf" "ec5be26eee2af05cbfef6bc3cbc48b4eba01e05953f118bec1d67af3a8060983" "9c50343ced8809392120487b21ed9f66b5db218bd9a69f02cc58224b7452217f" "5065dfe366b9fe61379563d65506f4ae1f030b3a00f13caaa1dc797cb0a26f60" "0aff51567909d728332ba22631bcb2cc4890130dc1261447663ff6147506f847" "99b077e8d793e87e20447d4b52055b2def8c083d09a9800706fe691ac06ef956" "d9843dc0c8ad948f12d0cdd33c0eda1ac62e029f0ad6522aff61f0444506d13c" "168630860e1b184c500fecae8295d861355f09751ff7c763d658af2359999b1a" "9c6ebd6f2e8ed1717f0322f2b3f9dc0e5ef16ae54810648db053875d03fe3db6" "911718851dfaac9e14bfe277aedef9407b759689df8692dddb193fc370082c80" "4b31c03b485367cc56647504ceeca4d657db6d2d1dda295455a5c5259d8fcd75" "6450867a05ce9126b6e6592373b6bbaaf65e637bc27ecc6e9b0375b8cf2ab744" "51dc5b02318828dc0b725ab7c85d8d5cd5b179212bce4aefc3f9db2db00190f4" "32774da9de62df0730e898bd46a1b8be1bde53f236a715dde064c4c1c03a0681" "15569f9ded25f6f841f284eb34a55e17ee4c2b6aa2795158fff6229f585b0bb6" "0fc74cd0e50009abbb8777981ea54dffaa3840f29d1db65ad95f7ca83cbff929" "73078af00076117610bb88f3ef3fe85190a80a85b18a697a8c98d09f593bf4cd" "35193cc0ca34817887bd0a1f88a85679454f36630c3df59dad78e969af9c1c5e" "9eecc40c7205eb97d7c76cab1ce0dfff5ef08a1974830e552b6f48af2e6a2e69" "3fc730c9f3161df13f1ac4d88c7e5e4d4e3477cf8246cfdc794afdf2d72cc625" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "ad4962515d8774d09be38a375cd9c57070e2579bf6aa427b5d40729001f83ea2" "bf42c68919c09268cb40934a66bc75c785001f3872ab5ad85c74988e60809b29" default))
 '(deft-directory "~/Nextcloud/Notes")
 '(deft-extension "txt")
 '(deft-strip-title-regex "^[#* ]*")
 '(deft-text-mode 'markdown-mode)
 '(deft-use-filename-as-title t)
 '(dhall-format-at-save nil)
 '(etags-select-use-short-name-completion t)
 '(flx-ido-threshhold 6000000)
 '(flycheck-disabled-checkers
   '(haskell-ghc haskell-stack-ghc haskell-ghc rust-cargo rust rust-clippy html-tidy))
 '(frame-background-mode 'dark)
 '(global-visual-line-mode nil)
 '(grep-command "grep  -nH -e +")
 '(grep-find-command
   '("find . -type f -exec grep -nH -e  {} + | cut -c1-300" . 34))
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "cabal-dev" "dist" "build" ".cabal-sandbox" ".idea" "*.egg-info" ".stack-work"))
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.sublime-workspace" "*_flymake*"))
 '(grep-find-template
   "find . <X> -type f <F> -exec grep <C> -nH -e <R> {} + | cut -c 1-300")
 '(haskell-compile-cabal-build-alt-command
   "cd %s && stack build --fast --ghc-options=\"-j +RTS -A32M -RTS\" --no-interleaved-output")
 '(haskell-compile-cabal-build-command
   "cd %s && stack build --fast --ghc-options=\"-j +RTS -A32M -RTS\" --no-interleaved-output")
 '(haskell-indentation-ifte-offset 2)
 '(haskell-indentation-layout-offset 2)
 '(haskell-indentation-left-offset 2)
 '(haskell-indentation-starter-offset 2)
 '(haskell-indentation-where-post-offset 2)
 '(haskell-indentation-where-pre-offset 2)
 '(haskell-mode-hook 'my-haskell-mode-hook)
 '(haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans -fshow-loaded-modules"))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type 'stack-ghci)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save nil)
 '(ido-create-new-buffer 'always)
 '(inhibit-startup-echo-area-message "kb")
 '(intero-package-version "0.1.40")
 '(linum-format " %7i ")
 '(magit-diff-use-overlays nil)
 '(markdown-enable-wiki-links t)
 '(ormolu-extra-args '("--ghc-opt" "-XTypeApplications"))
 '(package-selected-packages
   '(just-mode timu-macos-theme lsp-mode ac-geiser geiser-guile helm etags-select flatbuffers-mode casharp-mode company-go go-mode csharp-mode use-package dhall-mode bind-key ormolu format-all dante adaptive-wrap proof-general hasklig-mode string-inflection flycheck-elm add-node-modules-path tide groovy-mode idris-mode multi-term projectile-ripgrep package-build shut-up epl git commander f dash s))
 '(projectile-generic-command
   "find . -type f -not -name \"*.hi\" -not -name \"*.o\" -not -name \"*.p_o\" -not -name \"*.p_hi\" -not -name \"*.pyc\" -not -path \"*/cabal-dev/*\" -not -path \"*/.cabal-sandbox/*\" -not -path \"*/dist/*\" -not -path \"*/build/*\" -not -path \"*/.git/*\" -not -path \"*/javadoc/*\" -print0")
 '(projectile-switch-project-hook
   '((lambda nil
	   (interactive)
	   (set-title
		(projectile-project-name)))))
 '(py-pychecker-command "pychecker.sh")
 '(py-pychecker-command-args '(""))
 '(python-check-command "pychecker.sh")
 '(python-indent-guess-indent-offset t)
 '(python-indent-offset 4)
 '(ripgrep-arguments '("-M200"))
 '(rust-cargo-default-arguments "--tests")
 '(safe-local-variable-values
   '((eval setenv "PYTHONPATH"
		   (concat here "" ":"
				   (getenv "PYTHONPATH")))
	 (eval setenv "PYTHONPATH"
		   (concat here "" ":" here "ec2" ":" here "lambdas" ":"
				   (getenv "PYTHONPATH")))
	 (eval setq flycheck-pylintrc
		   (here ".pylintrc"))
	 (eval setenv "PYTHONPATH"
		   (concat here "ec2" ":" here "lambdas" ":"
				   (getenv "PYTHONPATH")))
	 (eval setenv "PYTHONPATH"
		   (concat here "ec2" ":"
				   (getenv "PYTHONPATH")))
	 (eval setenv "PYTHONPATH"
		   (concat here "scripts/download-articles" ":"
				   (getenv "PYTHONPATH")))
	 (jedi:server-args)
	 (eval setq jedi:environment-root
		   (concat here "venv"))
	 (eval setq flycheck-python-pylint-executable
		   (concat here "venv/bin/pylint"))
	 (eval setq here
		   (locate-dominating-file
			(buffer-file-name)
			".dir-locals.el"))
	 (dante-repl-command-line "stack" "repl")
	 (dante-repl-command-line "nix-shell" "--run" "stack repl")
	 (eval setenv "NIX_PATH" "nixpkgs=https://github.com/nixos/nixpkgs/archive/681db603640dac395b0f76eb666f39019457131b.tar.gz")
	 (intero-targets "github-agent:lib")
	 (intero-targets "lambda-calculus-hs:exe:lambda-calculus-hs")
	 (dante-repl-command-line "stack" "repl" "--ghc-options=\"-fno-code\"" dante-target)
	 (dante-repl-command-line "stack" "repl" "--ghc-options=\"-j -fno-code -fobject-code\"" dante-target)
	 (dante-repl-command-line "stack" "repl" dante-target)
	 (dante-repl-command-line quote
							  ("stack" "repl" dante-target))
	 (intero-targets . "externalogic:lib")
	 (dante-repl-command-line
	  '("stack" "repl" dante-target))
	 (dante-repl-command-line
	  ("stack" "ghci"))
	 (dante-repl-command-line
	  '("stack" "ghci"))
	 (dante-repl-command-line
	  '("stack ghci"))
	 (dante-project-root "/home/kb/workspace/externalogic")
	 (haskell-process-args-ghci "ghci")
	 (haskell-process-path-ghci . "stack")
	 (haskell-process-type . ghci)
	 (hindent-style . "johan-tibell")
	 (haskell-process-use-ghci . t)
	 (haskell-indent-spaces . 2)))
 '(send-mail-function 'mailclient-send-it)
 '(show-paren-mode t)
 '(sql-postgres-login-params
   '((user :default "postgres")
	 (password :default "password")
	 (server :default "localhost")
	 (database :default "externalogic")))
 '(tabbar-separator '(0.8))
 '(tabbar-use-images nil)
 '(tags-add-tables t)
 '(tool-bar-mode nil)
 '(visual-line-fringe-indicators '(nil right-curly-arrow))
 '(warning-suppress-log-types '((comp)))
 '(web-mode-code-indent-offset 4))

(setq auto-window-vscroll nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(magit-log-head-label-wip ((t (:background "Grey95" :foreground "LightSkyBlue3")))))

(require 'expand-region)
;; emacs in terminal doesn't handle C-=
;; see https://github.com/magnars/expand-region.el/issues/59
(global-set-key (kbd "C-c =") 'er/expand-region)
(require 'string-inflection)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(customize-set-variable 'frame-background-mode 'light)
;; (dark)
(cd "~")
