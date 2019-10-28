(deftheme slick
  "A theme based the wheatgrass theme that comes with emacs.")

(defconst slick-background-color "gray15")
(defconst slick-faint-highlight-color "gray25")
(defconst slick-highlight-color "gray40")
(defconst slick-search-color "saddle brown")
(defconst slick-text-color "gray90")
(defconst slick-cursor-color "#CF1E13")

(let ((class    '((class color)))
      (graphic  '((type x w32 mac ns))))
  (custom-theme-set-faces
   'slick

   ;; Basic faces
   `(default ((,graphic (:foreground ,slick-text-color :background ,slick-background-color))
              (,class   (:foreground ,slick-text-color))))
   `(cursor ((,class (:background ,slick-cursor-color))))
   `(error ((,class (:foreground "red"))))
   `(warning ((,class (:foreground "orange"))))
   `(success ((,class (:foreground "yellow green"))))
   `(fringe ((,class (:background "gray10"))))
   `(vertical-border ((,class (:foreground "gray25"))))
   `(shadow ((,class (:inherit default :background nil :foreground "gray75"))))
   `(header-line ((,class (:inherit mode-line :background "gray20" :foreground "gray90"
                           :box (:line-width 2 :color "gray20")))))

   ;; Compilation
   `(compilation-mode-line-fail ((,class (:foreground "dark green"))))
   `(compilation-mode-line-run  ((,class (:foreground "dark goldenrod"))))
   `(compilation-mode-line-exit ((,class (:foreground "yellow green"))))
   `(compilation-error ((,class (:foreground "tan3" :weight bold :underline t))))

   ;; Highlighting
   `(highlight ((,class (:foreground "white" :background "dark green"))))
   `(region ((,class (:foreground "white" :background "dark green"))))
   `(secondary-selection ((,class (:background "dark slate gray"))))
   `(isearch ((,class (:foreground "white" :background ,slick-search-color))))
   `(lazy-highlight ((,class (:background ,slick-faint-highlight-color :foreground "gray90" :underline t :weight bold))))
   `(hl-line ((,class (:background "gray20"))))
   `(linum ((,class (:foreground ,slick-highlight-color :inherit default))))
   `(show-paren-match ((,class (:background "gray50"))))

   ;; Font lock faces
   `(font-lock-preprocessor-face ((,class (:foreground "DarkGoldenrod" :weight bold))))
   `(font-lock-comment-face ((,class (:foreground "OliveDrab" :slant italic))))
   `(font-lock-string-face ((,class (:inherit default :foreground "cadet blue"))))
   `(font-lock-warning-face ((,class (:inherit default :foreground "seashell3"))))

   ;; Disable some font lock faces for less color.
   `(font-lock-function-name-face ((,class (:inherit nil :weight bold))))
   `(font-lock-keyword-face ((,class (:inherit nil))))
   `(font-lock-type-face ((,class (:inherit nil))))
   `(font-lock-operator-face ((,class (:inherit nil))))
   `(font-lock-constant-face ((,class (:inherit nil))))
   `(font-lock-builtin-face ((,class (:inherit nil))))
   `(font-lock-variable-name-face ((,class (:inherit nil))))
   `(sh-heredoc ((,class (:inherit font-lock-string-face :background "gray20"))))

   ;; Button and link
   `(link ((,class (:underline t :foreground "tan3"))))
   `(w3m-anchor ((,class (:underline t :foreground "tan3"))))
   `(link-visited ((,class (:underline t :foreground "tan4"))))
   `(w3m-arrived-anchor ((,class (:underline t :foreground "tan4"))))
   `(custom-variable-tag ((,class (:foreground "tan2"))))

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
   `(mode-line-inactive  ((,class (:inherit mode-line :background "black" :foreground "gray40" :weight light :box (:line-width 1 :style released-button)))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground "white"))))
   `(mode-line-highlight ((,class (:background "tan" :foreground "black" :box nil))))
   `(which-func          ((,class (:foreground "chartreuse3"))))
   `(aw-mode-line-face   ((,class (:inherit mode-line-buffer-id
                                   :weight light
                                   :height 0.8
                                   :background ,slick-text-color
                                   :foreground ,slick-background-color))))

   ;; Minibuffer
   `(minibuffer-prompt ((,class (:foreground "deep pink" :weight bold))))

   ;; Markdown
   `(markdown-pre-face ((,class (:inherit fixed-pitch :foreground "gray75"))))
   `(markdown-inline-code-face ((,class (:inherit fixed-pitch :foreground "gray75"))))
   `(markdown-header-face-1 ((,class (:inherit markdown-header-face :height 1.2))))
   `(markdown-header-face-2 ((,class (:inherit markdown-header-face :height 1.1))))
   `(markdown-header-face-3 ((,class (:inherit markdown-header-face :height 1.0))))
   `(markdown-header-face-4 ((,class (:inherit markdown-header-face :height 1.0))))
   `(markdown-header-face-5 ((,class (:inherit markdown-header-face :height 1.0))))
   `(markdown-language-keyword-face ((,class (:inherit default :height 0.7))))

   ;; Org
   `(org-hide             ((,graphic (:foreground ,slick-background-color)) (t (:foreground "black"))))
   `(org-document-title   ((,class (:foreground "light blue"))))
   `(org-document-info    ((,class (:foreground "light blue"))))
   `(org-block            ((,class (:inherit fixed-pitch :foreground "gray75"))))
   `(org-table            ((,class (:inherit fixed-pitch :foreground "LightSkyBlue"))))
   `(org-meta-line        ((,class (:foreground "gray45" :height 0.7))))
   `(org-block-begin-line ((,class (:foreground "gray45" :height 0.7))))
   `(org-block-end-line   ((,class (:foreground "gray45" :height 0.7))))
   `(outline-1            ((,class (:weight bold :foreground ,slick-text-color))))
   `(outline-2            ((,class (:weight bold :foreground ,slick-text-color))))
   `(outline-3            ((,class (:weight bold :foreground ,slick-text-color))))

   ;; Shell, etc.
   `(eshell-prompt ((,class (:foreground "deep pink" :weight bold))))
   `(term-color-black ((,class (:background "dim gray" :foreground "dim gray"))))
   `(term-color-blue ((,class (:background "deep sky blue" :foreground "deep sky blue"))))

   ;; Magit
   `(smerge-refined-added ((,class (:inherit smerge-refined-change :underline "#22aa22"))))
   `(smerge-refined-removed ((,class (:inherit smerge-refined-change :underline "#aa2222"))))
   `(magit-item-highlight ((,class (:background ,slick-faint-highlight-color))))
   `(magit-section-title ((,class (:foreground "tan3" :weight bold))))
   `(magit-section-highlight ((,class (:background ,slick-faint-highlight-color))))
   `(magit-diff-added ((,class (:background "#225522" :foreground "#88ff88"))))
   `(magit-diff-added-highlight ((,class (:background "#307730" :foreground "#ccffcc"))))
   `(magit-diff-removed ((,class (:background "#552222" :foreground "#ff8888"))))
   `(magit-diff-removed-highlight ((,class (:background "#773333" :foreground "#ffcccc"))))
   `(magit-diff-context ((,class (:inherit default))))
   `(magit-diff-context-highlight ((,class (:inherit default))))

   ;; Ediff
   `(ediff-odd-diff-A ((,class (:background ,slick-faint-highlight-color))))
   `(ediff-odd-diff-B ((,class (:background ,slick-faint-highlight-color))))
   `(ediff-even-diff-A ((,class (:background ,slick-faint-highlight-color))))
   `(ediff-even-diff-B ((,class (:background ,slick-faint-highlight-color))))

   ;; avy/ivy/swiper
   `(avy-lead-face-0 ((,class (:foreground "black" :background "tan"))))
   `(avy-lead-face-1 ((,class (:foreground "black" :background "white"))))
   `(avy-lead-face-2 ((,class (:foreground "black" :background "red"))))
   `(avy-lead-face ((,class (:foreground "black" :background "tan3"))))

   `(ivy-remote ((,class (:foreground "deep sky blue"))))
   `(ivy-current-match ((,class (:foreground "white" :background ,slick-search-color))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground "gray90"))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground "gray90" :underline t))))

   `(swiper-line-face ((,class (:background ,slick-search-color :foreground "white"))))
   `(swiper-match-face-1 ((,class (:background "gray30" :foreground "gray90"))))
   `(swiper-match-face-2 ((,class (:background "gray30" :foreground "gray90" :weight bold :underline t))))

   ;; LSP
   `(lsp-face-highlight-textual ((,class (:background ,slick-faint-highlight-color))))

   ;; mu4e
   `(mu4e-header-highlight-face ((,class (:inherit region :underline nil :weight medium))))
   `(mu4e-modeline-face ((,class (:inherit nil :background nil :foreground ,slick-text-color))))
   `(mu4e-flagged-face ((,class (:foreground "tan3" :weight medium))))
   `(mu4e-cited-1-face ((,class (:foreground "gray70"))))
   `(mu4e-cited-2-face ((,class (:foreground "gray60"))))
   `(mu4e-cited-3-face ((,class (:foreground "gray50"))))
   `(mu4e-cited-4-face ((,class (:foreground "gray40"))))
   `(mu4e-cited-5-face ((,class (:foreground "gray30"))))

   ;; Weechat
   `(weechat-highlight-face ((,class (:background ,slick-faint-highlight-color :foreground "Cyan1"))))
))

(provide-theme 'slick)

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
