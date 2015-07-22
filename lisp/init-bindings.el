(require 'init-bootstrap)
(require 'init-commands)
(require 'init-autoloads)
(require 'cl-lib)

(eval-when-compile
  (require 'ace-window)
  (require 'company)
  (require 'expand-region)
  (require 'projectile)
  (require 'swiper)
  (require 'counsel)
  (require 'ivy)
  (require 'avy))

;; my commands
(global-set-key (kbd "<f10>") 'recompile)
(global-set-key (kbd "M-<f10>") 'compile)
(global-set-key (kbd "C-x 9") 'recompile)
(global-set-key (kbd "C-x M-9") 'compile)
(global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "M-o") 'select-mru-window)
(global-set-key (kbd "C-x C-j") 'fjl/join-next-line) ;; overwrites jabber commands

(define-key prog-mode-map (kbd "M-RET") 'fjl/comment-enter)
(define-key prog-mode-map (kbd "RET") 'newline)

(global-set-key (kbd "M-s .") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "M-s d") 'multi-isearch-glob)

;; dired binds C-x C-j in some local map, so it needs to be overridden here.
(define-key prog-mode-map (kbd "C-x C-j") 'fjl/join-next-line)

;; overwrites one of the many bindings for undo. not a problem so far.
(global-set-key (kbd "C-/") 'hippie-expand)
;; the above doesn't work in the terminal, these two do.
;; they overwrite dabbrev-expand, which is included in hippie-expand.
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-M-/") 'hippie-expand)

;; package commands
(global-set-key (kbd "<f12>") 'projectile-find-file)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c SPC") 'avy-goto-char)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-M-s") 'swiper)

(after-package ivy 
  ;; In ivy, never exit when pressing TAB too much.
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)
  ;; In ivy, swap C-j and RET to retain ido behavior.
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done))

(after-package company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; make paragraph keys easier to type, especially on small keyboards.
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

(define-key minibuffer-local-map [f3]
  (lambda () (interactive)
     (insert (buffer-name (window-buffer (minibuffer-selected-window))))))

;; compilation
(define-key compilation-mode-map (kbd "C-c C-q") '(lambda () (interactive) (quit-process)))

;; ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; the first few function keys launch eshell sessions
(dolist (k '(f1 f2 f3 f4 f5 f6))
  (global-set-key `[(control ,k)] 'fjl/eshell-hotkey)
  (global-set-key `[(meta ,k)] 'fjl/eshell-hotkey))
;; also bind to C-7 [1-9] for non-GUI emacs
(dolist (k '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (global-set-key `[(control ?x) ?7 ,k] 'fjl/eshell-hotkey)
  (global-set-key `[(control ?x) ?7 (meta ,k)] 'fjl/eshell-hotkey))

(global-set-key [f2] 'fjl/eshell-restart-command)

;; term
(add-hook 'term-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-y") 'term-paste)
            (local-set-key (kbd "C-x M-w") 'kill-ring-save)))

(when (memq window-system '(ns mac))
  ;; disable print-buffer on OS X
  (global-unset-key (kbd "s-p"))
  ;; enable CUA paste shortcut to make pasting from LaunchBar work
  (global-set-key (kbd "s-v") 'clipboard-yank)
  ;; enable fast frame switch on OS X
  (global-set-key (kbd "M-`") 'other-frame)
  (global-set-key (kbd "s-`") 'other-frame))

;; set up file-name > mode associations
(add-to-list 'auto-mode-alist '("\\.hbs" . web-mode))

(provide 'init-bindings)
