(deftheme slick
  "A theme based the wheatgrass theme that comes with emacs.")

(let ((class    '((class color) (min-colors 89)))
      (graphic  '((type x w32 mac ns))))
  (custom-theme-set-faces
   'slick

   ;; Basic faces
   `(default ((,graphic (:foreground "gray80" :background "gray15"))))
   `(cursor ((,class (:background "red"))))
   `(error ((,class (:foreground "red"))))
   `(warning ((,class (:foreground "orange"))))
   `(success ((,class (:foreground "yellow green"))))
   `(fringe ((,class (:background "gray10"))))
   `(vertical-border ((,class (:foreground "gray25"))))
   `(header-line ((,class (:inherit mode-line :background "grey20" :foreground "grey90"
                           :box (:line-width 2 :color "grey20")))))

   ;; Compilation faces
   `(compilation-mode-line-fail ((,class (:foreground "dark green"))))
   `(compilation-mode-line-run  ((,class (:foreground "dark goldenrod"))))
   `(compilation-mode-line-exit ((,class (:foreground "SpringGreen4"))))

   ;; Highlighting faces
   `(highlight ((,class (:foreground "white" :background "dark green"))))
   `(region ((,class (:foreground "white" :background "dark green"))))
   `(secondary-selection ((,class (:background "dark slate gray"))))
   `(isearch ((,class (:foreground "white" :background "dark goldenrod"))))
   `(lazy-highlight ((,class (:background "gray25"))))

   ;; Font lock faces
   `(font-lock-preprocessor-face ((,class (:foreground "DarkGoldenrod" :weight bold))))
   `(font-lock-comment-face ((,class (:foreground "OliveDrab" :slant italic))))
   `(font-lock-string-face ((,class (:foreground "cadet blue"))))
   ;; `(font-lock-function-name-face ((,class (:foreground "IndianRed3" :weight bold))))
   ;; `(font-lock-builtin-face ((,class (:foreground "DarkGoldenrod"))))
   ;; `(font-lock-keyword-face ((,class (:inherit default :weight bold))))
   ;; `(font-lock-variable-name-face ((,class (:foreground "sienna1" :weight normal))))
   ;; `(font-lock-constant-face ((,class (:foreground "dark turquoise"))))

   ;; disable some font lock faces for less color.
   `(font-lock-function-name-face ((,class (:weight bold))))
   `(font-lock-keyword-face ((,class (:inherit default))))
   `(font-lock-type-face ((,class (:inherit default))))
   `(font-lock-constant-face ((,class (:inherit default))))
   `(font-lock-builtin-face ((,class (:inherit default))))
   `(font-lock-variable-name-face ((,class (:inherit default))))

   ;; Button and link faces
   `(link ((,class (:underline t :foreground "cyan"))))
   `(link-visited ((,class (:underline t :foreground "dark cyan"))))

   ;; Gnus faces
   `(gnus-header-content ((,class (:weight normal :foreground "yellow green"))))
   `(gnus-header-from ((,class (:foreground "pale green"))))
   `(gnus-header-subject ((,class (:foreground "pale turquoise"))))
   `(gnus-header-name ((,class (:foreground "dark sea green"))))
   `(gnus-header-newsgroups ((,class (:foreground "dark khaki"))))

   ;; Message faces
   `(message-header-name ((,class (:foreground "dark turquoise"))))
   `(message-header-cc ((,class (:foreground "yellow green"))))
   `(message-header-other ((,class (:foreground "dark khaki"))))
   `(message-header-subject ((,class (:foreground "pale turquoise"))))
   `(message-header-to ((,class (:foreground "pale green"))))
   `(message-cited-text ((,class (:foreground "SpringGreen3"))))
   `(message-separator ((,class (:foreground "deep sky blue"))))

   ;; Mode line style
   `(mode-line           ((,class (:background "tan4" :foreground "gray90" :box (:line-width 2 :color "tan4" :style released-button)))))
   `(mode-line-inactive  ((,class (:inherit mode-line :background "black" :foreground "grey40" :weight light :box (:line-width 2 :style released-button)))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground "white"))))
   `(mode-line-highlight ((,class (:background "tan" :foreground "black" :box nil))))

   ;; Ido-related faces
   `(flx-highlight-face ((,class (:foreground "tan3" :underline t))))
   `(ido-vertical-match-face ((,class (:foreground "tan3" :underline t))))

   ;; markdown
   `(markdown-pre-face ((,class (:background "gray20"))))

   ;; Misc faces
   `(hl-line ((,class (:background "gray18"))))
   `(compilation-error ((,class (:foreground "tan3" :weight bold :underline t))))
   `(eshell-prompt ((,class (:foreground "deep pink" :weight bold))))
   `(ido-subdir ((,class (:foreground "khaki4" :slant normal))))
   `(linum ((,class (:foreground "gray60" :inherit fringe))))
   `(magit-item-highlight ((,class (:background "grey20"))))
   `(magit-section-title ((,class (:foreground "tan3" :weight bold))))
   `(minibuffer-prompt ((,class (:foreground "deep pink" :weight bold))))
   `(show-paren-match ((,class (:background "gray50"))))
  ))

(provide-theme 'slick)
