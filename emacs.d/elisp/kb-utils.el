(defun un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
    Default for SEP is a hyphen \"-\".

    If third argument START is non-nil, convert words after that
    index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "-") 
                                     (downcase (match-string 0 s))) 
                             t nil s)))
    (downcase s)))

;;;;"I always compile my .emacs, saves me about two seconds
;;;;startuptime. But that only helps if the .emacs.elc is newer
;;;;than the .emacs. So compile .emacs if it's not."
(defun autocompile-dotemacs nil
  "compile itself if ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (if (string= (buffer-file-name) (expand-file-name (concat
                                                     default-directory ".emacs")))
      (byte-compile-file (buffer-file-name))))

;;;; Делаем чтоб Shift-M-`Arrow keys` ресайзило окна
(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
         (this-window-y-min (nth 1 win-edges))
         (this-window-y-max (nth 3 win-edges))
         (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the
middle"
  (let* ((win-edges (window-edges))
         (this-window-x-min (nth 0 win-edges))
         (this-window-x-max (nth 2 win-edges))
         (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 4) this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -1))
   (t (message "nil"))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 1))
   (t (message "nil"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -1))))

(defun win-resize-minimize-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 1))))

;;

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;;

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 2))

(defun shift-left ()
  (interactive)
  (shift-region -2))

(defun go-shift-region (distance)
  (interactive "nEnter distance: ")
  (shift-region distance))

;;

(defun frame-retitle (title)
  (modify-frame-parameters nil 
                           (list (cons 'name
                                       title))))

;;

(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

;;

(defun visual-line-mode-activate ()
  (when (and (buffer-file-name)
             (or (string-match "\\.py\\'" (buffer-file-name))
                 (string-match "\\.shpaml\\'" (buffer-file-name))
                 (string-match "\\.js\\'" (buffer-file-name))
                 (string-match "\\.feature\\'" (buffer-file-name))
                 (string-match "\\.hs\\'" (buffer-file-name))))
    (visual-line-mode)))

;;

;; (defun adaptive-wrap-mode-activate ()
;;   (when (or (string-match "\\.py\\'" buffer-file-name)
;;             (string-match "\\.shpaml\\'" buffer-file-name)
;;             (string-match "\\.html\\'" buffer-file-name))
;;     (adaptive-wrap-prefix-mode)))

;;

(defun frame-bck()
  (interactive)
  (other-window -1))

;;

(defun visual-line-mode-set-logical-line-movement ()
  (define-key visual-line-mode-map "\C-a"
    'beginning-of-line)
  (define-key visual-line-mode-map "\C-e"
    'end-of-line))

;;

(defun kb-tabbar-groups-function ()
  "Return the list of group names BUFFER belongs to.
 Return only one group for each buffer."
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1))
    '("Emacs Buffer"))
   ((eq major-mode 'dired-mode)
    '("Dired"))
   (t
    '("User Buffer"))
   ))

;;

(defun python-flymake-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "~/.emacs.d/pyflakespep8.py" (list local-file))))

(defun haskell-flymake-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "~/bin/flycheck_haskell.pl" (list local-file))))

(defun javascript-flow-flycheck-init ()
  (flycheck-define-checker javascript-flow
    "A JavaScript syntax and style checker using Flow.
See URL `http://flowtype.org/'."
    :command ("flow" source-original)
    :error-patterns
    ((error line-start
            (file-name)
            ":"
            line
            ":"
            (minimal-match (one-or-more not-newline))
            ": "
            (message (minimal-match (and (one-or-more anything) "\n")))
            line-end))
    :modes (web-mode js-mode))
  (add-to-list 'flycheck-checkers 'javascript-flow))

(defun jsxhint-checker-flycheck-init ()
  (flycheck-define-checker jsxhint-checker
    "A JSX syntax and style checker based on JSXHint."
    :command ("jsxhint" source)
    :error-patterns
    ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
    :modes (web-mode))
  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (equal web-mode-content-type "jsx")
		;; enable flycheck
                                        ; (flycheck-select-checker 'jsxhint-checker)
		)))
  (add-to-list 'flycheck-checkers 'jsxhint-checker))

;;

(defun turn-on-subword-mode ()
  (interactive)
  (subword-mode 1))

(defun turn-on-column-marker-mode ()
  (interactive)
  (column-marker-1 80))

;; (defun turn-on-flymake-mode ()
;;   (interactive)
;;   (flymake-mode 1))

;;

(defun my-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
              tags-completion-table)
    (etags-select-find (ido-completing-read "Tag: " tag-names nil nil
                                            (thing-at-point 'word)))))

(defun my-find-tag ()
  "Find a tag using ido"
  (interactive)
  (setq etags-select-source-buffer (buffer-name))
  (let* ((default (thing-at-point 'symbol))
         (tagname (completing-read
                   (format "Find tag (default %s): " default)
                   'etags-select-complete-tag nil nil nil 'find-tag-history default)))
    (etags-select-find tagname)))

;;

;; (defun hs-grep-type (typename)
;;   (interactive "sEnter type's grep: ")
;;   (projectile-grep (concat "(type|data) " typename)))

;; (defvar bzg-big-fringe-mode nil)
;; (define-minor-mode bzg-big-fringe-mode
;;   "Minor mode to hide the mode-line in the current buffer."
;;   :init-value nil
;;   :global t
;;   :variable bzg-big-fringe-mode
;;   :group 'editing-basics
;;   (if (not bzg-big-fringe-mode)
;;       (set-fringe-style nil)
;;     (set-fringe-mode
;;      (/ (- (frame-pixel-width)
;;            (* 100 (frame-char-width)))
;;         2))))


(defun set-title (title)
  (interactive "sEnter title: ")
  (setq frame-title-format title))

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(provide 'kb-utils)
