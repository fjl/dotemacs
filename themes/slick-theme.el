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

   ;; Compilation
   `(compilation-mode-line-fail ((,class (:foreground "dark green"))))
   `(compilation-mode-line-run  ((,class (:foreground "dark goldenrod"))))
   `(compilation-mode-line-exit ((,class (:foreground "SpringGreen4"))))
   `(compilation-error ((,class (:foreground "tan3" :weight bold :underline t))))

   ;; Highlighting
   `(highlight ((,class (:foreground "white" :background "dark green"))))
   `(region ((,class (:foreground "white" :background "dark green"))))
   `(secondary-selection ((,class (:background "dark slate gray"))))
   `(isearch ((,class (:foreground "white" :background "dark goldenrod"))))
   `(lazy-highlight ((,class (:background "gray25"))))
   `(hl-line ((,class (:background "gray18"))))
   `(linum ((,class (:foreground "gray40" :inherit default))))
   `(show-paren-match ((,class (:background "gray50"))))

   ;; Font lock faces
   `(font-lock-preprocessor-face ((,class (:foreground "DarkGoldenrod" :weight bold))))
   `(font-lock-comment-face ((,class (:foreground "OliveDrab" :slant italic))))
   `(font-lock-string-face ((,class (:foreground "cadet blue"))))

   ;; Disable some font lock faces for less color.
   `(font-lock-function-name-face ((,class (:inherit default :weight bold :foreground nil))))
   `(font-lock-keyword-face ((,class (:inherit default :foreground nil))))
   `(font-lock-type-face ((,class (:inherit default :foreground nil))))
   `(font-lock-constant-face ((,class (:inherit default :foreground nil))))
   `(font-lock-builtin-face ((,class (:inherit default :foreground nil))))
   `(font-lock-variable-name-face ((,class (:inherit default))))

   ;; Button and link
   `(link ((,class (:underline t :foreground "tan3"))))
   `(w3m-anchor ((,class (:underline t :foreground "tan3"))))
   `(link-visited ((,class (:underline t :foreground "tan4"))))
   `(w3m-arrived-anchor ((,class (:underline t :foreground "tan4"))))

   ;; Gnus
   `(gnus-header-content ((,class (:weight normal :foreground "yellow green"))))
   `(gnus-header-from ((,class (:foreground "pale green"))))
   `(gnus-header-subject ((,class (:foreground "pale turquoise"))))
   `(gnus-header-name ((,class (:foreground "dark sea green"))))
   `(gnus-header-newsgroups ((,class (:foreground "dark khaki"))))

   ;; Message
   `(message-header-name ((,class (:foreground "dark turquoise"))))
   `(message-header-cc ((,class (:foreground "yellow green"))))
   `(message-header-other ((,class (:foreground "dark khaki"))))
   `(message-header-subject ((,class (:foreground "pale turquoise"))))
   `(message-header-to ((,class (:foreground "pale green"))))
   `(message-cited-text ((,class (:foreground "SpringGreen3"))))
   `(message-separator ((,class (:foreground "deep sky blue"))))

   ;; Mode line style
   `(mode-line           ((,class (:background "tan4" :foreground "gray90" :box (:line-width 1 :color "tan4" :style released-button)))))
   `(mode-line-inactive  ((,class (:inherit mode-line :background "black" :foreground "grey40" :weight light :box (:line-width 1 :style released-button)))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground "white"))))
   `(mode-line-highlight ((,class (:background "tan" :foreground "black" :box nil))))

   ;; Minibuffer
   `(ivy-remote ((,class (:foreground "deep sky blue"))))
   `(minibuffer-prompt ((,class (:foreground "deep pink" :weight bold))))

   ;; Markdown
   `(markdown-pre-face ((,class (:background "gray20"))))

   ;; Shell, etc.
   `(eshell-prompt ((,class (:foreground "deep pink" :weight bold))))
   `(term-color-black ((,class (:background "dim gray" :foreground "dim gray"))))

   ;; Magit
   `(smerge-refined-added ((,class (:inherit smerge-refined-change :underline "#22aa22"))))
   `(smerge-refined-removed ((,class (:inherit smerge-refined-change :underline "#aa2222"))))
   `(magit-item-highlight ((,class (:background "grey20"))))
   `(magit-section-title ((,class (:foreground "tan3" :weight bold))))
  ))

(provide-theme 'slick)
