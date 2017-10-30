(unless (>= emacs-major-version 24)
  (error "requires Emacs 24 or later."))

(deftheme k-bx  "A dark color theme for Emacs based on Sublime Text 2")

(custom-theme-set-variables
  'k-bx
  '(linum-format " %7i "))

(let ((*background*         "#171717")
      (*comments*           "#ffffff")
      (*constant*           "#52e3f6")
      (*current-line*       "#151515")
      (*cursor-underscore*  "#fce94f")
      (*keywords*           "#ff007f")
      (*preprocessor*       "#52e3f6")

      ;; Sidebar line numbers
      (*line-number*        "#161A1F")
      (*line-fg*            "#666")

      (*type-face*          "#8ae234")
      (*method-declaration* "#edd400")
      (*mode-line-bg*       "#202020")
      (*mode-inactive-bg*   "#202020")
      (*mode-line-fg*       "grey90")
      (*mode-inactive-fg*   "grey60")
      (*normal*             "#cfbfad")
      (*number*             "#FC580C")
      (*operators*          "#cfbfad")
      (*warning*            "#FF6C60")
      (*regexp*             "#A63A62")
      (*string*             "#ece47e")
      (*variable*           "#8ae234")
      (*visual-selection*   "#cc9900")
      (*rainbow-depth-1*    "grey55")
      (*show-paren-background* "#729fcf"))

;; NOTES:
;;
;; nice blue #729fcf

;; (set-face-attribute 'region nil :background "#666")
;; (set-face-attribute 'secondary-selection nil :background "#666")
;; (set-face-attribute 'region nil :background "#cc9900")
;; (set-face-attribute 'secondary-selection nil :background "#cc9900")

  (custom-theme-set-faces
   'k-bx

   `(bold ((t (:bold t))))
   `(button ((t (:foreground ,*keywords* :underline t))))
   `(default ((t (:background ,*background* :foreground ,*normal*))))
   `(header-line ((t (:background ,*mode-line-bg* :foreground ,*normal*))))
   `(highlight ((t (:background ,*current-line*))))
   `(highlight-face ((t (:background ,*current-line*))))
   `(hl-line ((t (:background ,*current-line* :underline t))))
   `(info-xref ((t (:foreground ,*keywords* :underline t))))
   `(region ((t (:background ,*visual-selection* :inverse-video nil))))
   `(underline ((nil (:underline t))))
   `(secondary-selection ((t (:background ,*visual-selection*))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,*operators*))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,*comments*))))
   `(font-lock-comment-face ((t (:foreground ,*comments* :slant normal))))
   `(font-lock-constant-face ((t (:foreground ,*constant*))))
   `(font-lock-doc-face ((t (:foreground ,*string* :slant normal))))
   `(font-lock-doc-string-face ((t (:foreground ,*string*))))
   `(font-lock-function-name-face ((t (:foreground ,*method-declaration*))))
   `(font-lock-keyword-face ((t (:foreground ,*keywords*))))
   `(font-lock-negation-char-face ((t (:foreground ,*warning*))))
   `(font-lock-number-face ((t (:foreground ,*number*))))
   `(font-lock-preprocessor-face ((t (:foreground ,*preprocessor*))))
   `(font-lock-reference-face ((t (:foreground ,*constant*))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,*regexp*))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,*regexp*))))
   `(font-lock-string-face ((t (:foreground ,*string*))))
   `(font-lock-type-face ((t (:foreground ,*type-face*))))
   `(font-lock-variable-name-face ((t (:foreground ,*variable*))))
   `(font-lock-warning-face ((t (:foreground ,*warning*))))

   ;; GUI
   `(fringe ((t (:background ,*background*))))
   `(linum ((t (:background ,*line-number* :foreground ,*line-fg*))))
   `(minibuffer-prompt ((t (:foreground ,*variable*))))
   `(mode-line ((t (:background ,*mode-line-bg*
                    :foreground ,*mode-line-fg*
                    :inverse-video nil))))
   `(mode-line-inactive ((t (:background ,*mode-inactive-bg*
                             :foreground ,*mode-inactive-fg*
                             :inverse-video nil))))
   `(cursor ((t (:background ,*cursor-underscore*))))
   `(text-cursor ((t (:background ,*cursor-underscore*))))
   `(vertical-border ((t (:foreground ,*background*)))) ;; between splits

   ;; show-paren
   `(show-paren-mismatch ((t (:background ,*warning* :foreground ,*normal*
                              :weight bold))))
   `(show-paren-match ((t (:background ,*show-paren-background*
                           :foreground ,*normal* :weight bold))))

   ;; search
   `(isearch ((t (:background ,*regexp* :foreground ,*visual-selection*))))
   `(isearch-fail ((t (:background ,*warning*))))
   `(lazy-highlight ((t (:background ,*operators* :foreground ,*visual-selection*))))

   ;; rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,*rainbow-depth-1*))))
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'k-bx)
