;; -*- emacs-lisp -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(Man-width 90)
 '(ag-reuse-buffers t)
 '(ag-reuse-window nil)
 '(ansi-color-for-comint-mode t)
 '(async-bytecomp-package-mode t)
 '(auth-sources nil)
 '(avy-background t)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup/"))))
 '(battery-load-low 15)
 '(battery-mode-line-format " Bat: %p")
 '(battery-mode-line-limit 15)
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "linux"))))
 '(calc-language-alist
   (quote
    ((latex-mode . latex)
     (tex-mode . tex)
     (plain-tex-mode . tex)
     (context-mode . tex)
     (nroff-mode . eqn)
     (pascal-mode . pascal)
     (c-mode . c)
     (c++-mode . c)
     (fortran-mode . fortran)
     (f90-mode . fortran)
     (texinfo-mode . calc-normal-language)
     (go-mode . c))))
 '(calc-settings-file "~/.emacs.d/settings-calc.eld")
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(comint-completion-autolist t)
 '(comint-input-autoexpand (quote history))
 '(comint-input-ignoredups t)
 '(comint-process-echoes nil)
 '(comment-empty-lines (quote eol))
 '(comment-fill-column 70)
 '(company-auto-complete (quote (quote company-explicit-action-p)))
 '(company-auto-complete-chars (quote (41)))
 '(company-go-insert-arguments nil)
 '(company-idle-delay 0.2)
 '(compilation-always-kill t)
 '(compilation-error-regexp-alist
   (quote
    (absoft ada aix ant bash borland python-tracebacks-and-caml comma cucumber msft edg-1 edg-2 epc ftnchek iar ibm irix jikes-file maven jikes-line gcc-include ruby-Test::Unit lcc makepp mips-1 mips-2 msft omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint go-test gnu java)))
 '(compilation-message-face (quote default))
 '(compilation-scroll-output (quote first-error))
 '(compilation-search-path (quote (nil ".")))
 '(compilation-window-height 20)
 '(completion-styles (quote (partial-completion emacs22)))
 '(custom-safe-themes t)
 '(darkroom-text-scale-increase 0)
 '(dired-async-mode t)
 '(display-battery-mode t)
 '(display-buffer-reuse-frames t)
 '(display-time-format " %-I:%M%p")
 '(display-time-mode t)
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(electric-indent-mode t)
 '(ember-completion-system (quote default))
 '(eshell-banner-message "")
 '(eshell-cmpl-autolist t)
 '(eshell-hist-ignoredups t)
 '(eshell-kill-processes-on-exit (quote ask))
 '(eshell-modify-global-environment t)
 '(eshell-modules-list
   (quote
    (eshell-alias eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-prompt eshell-script eshell-term eshell-tramp eshell-unix)))
 '(eshell-prompt-function (quote fjl/eshell-prompt))
 '(eshell-prompt-regexp "^[^>
]+ >> ")
 '(eshell-visual-subcommands
   (quote
    (("vagrant" "ssh")
     ("brew" "log")
     ("geth" "attach"))))
 '(expand-region-preferred-python-mode (quote fgallina-python))
 '(fill-column 90)
 '(font-lock-maximum-decoration 0)
 '(fuel-debug-confirm-restarts-p nil)
 '(fuel-factor-root-dir "~/src/factor")
 '(fuel-listener-factor-binary "~/src/factor/factor")
 '(fuel-listener-window-allow-split nil)
 '(git-commit-summary-max-length 80)
 '(gnutls-algorithm-priority "PFS")
 '(gnutls-min-prime-bits 1024)
 '(gnutls-trustfiles
   (quote
    ("/usr/local/etc/openssl/cert.pem" "/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/ca-bundle.pem" "/usr/ssl/certs/ca-bundle.crt")))
 '(gnutls-verify-error t)
 '(go-fontify-function-calls nil)
 '(godoc-at-point-function (quote godoc-gogetdoc))
 '(gofmt-show-errors nil)
 '(graphviz-dot-auto-indent-on-semi nil)
 '(grep-highlight-matches t)
 '(grep-use-null-device nil)
 '(history-delete-duplicates t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries nil)
 '(indicate-empty-lines nil)
 '(inhibit-default-init t)
 '(inhibit-startup-echo-area-message "fjl")
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote text-mode))
 '(initial-scratch-message nil)
 '(ivy-do-completion-in-region nil)
 '(ivy-minibuffer-faces
   (quote
    (ivy-minibuffer-match-face-1 ivy-minibuffer-match-face-2)))
 '(ivy-mode t)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote full))
 '(ivy-wrap t)
 '(mac-mouse-wheel-mode t)
 '(magit-auto-revert-immediately nil)
 '(magit-completing-read-function (quote ivy-completing-read))
 '(magit-display-buffer-function (quote magit-display-buffer-same-window-except-diff-v1))
 '(magit-restore-window-configuration nil)
 '(magit-revert-buffers -2 t)
 '(magit-revision-show-gravatars nil)
 '(markdown-header-scaling t)
 '(message-cite-style (quote message-cite-style-thunderbird))
 '(message-kill-buffer-on-exit t)
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(message-signature nil)
 '(message-signature-insert-empty-line nil)
 '(mouse-wheel-progressive-speed t)
 '(mouse-wheel-scroll-amount (quote (2 ((shift) . 1) ((control)))))
 '(multi-isearch-pause nil)
 '(next-screen-context-lines 8)
 '(ns-alternate-modifier (quote meta))
 '(ns-command-modifier (quote super))
 '(org-agenda-files
   (quote
    ("~/Documents/org/work.org" "~/Documents/org/personal.org")))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (shell . t))))
 '(org-capture-templates
   (quote
    (("t" "Work Task" entry
      (file "~/Documents/org/work.org")
      "* LATER %?" :prepend t :empty-lines 1))))
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/Documents/org/work.org")
 '(org-export-allow-bind-keywords t)
 '(org-indent-mode-turns-on-hiding-stars nil)
 '(org-replace-disputed-keys t)
 '(org-src-fontify-natively nil)
 '(org-startup-indented t)
 '(package-selected-packages
   (quote
    (ivy-hydra weechat graphviz-dot-mode org-mac-iCal cl-generic cm-mode levenshtein hydra ox-gfm virtualenvwrapper fuel darkroom ace-window ag company-go counsel dtrt-indent ember-mode erlang eshell-z expand-region fish-mode geiser gist go-eldoc go-scratch magit markdown-mode nginx-mode org projectile qml-mode restclient smex speed-type slime web-mode wgrep yaml-mode)))
 '(projectile-completion-system (quote ivy))
 '(projectile-global-mode t)
 '(projectile-ignored-project-function (quote file-remote-p))
 '(projectile-mode-line
   (quote
    (:eval
     (if
         (file-remote-p default-directory)
         " P"
       (format " P<%s>"
               (projectile-project-name))))))
 '(projectile-switch-project-action (lambda nil (dired ".")))
 '(python-shell-completion-native-disabled-interpreters (quote ("pypy" "python")))
 '(query-user-mail-address nil)
 '(recentf-auto-cleanup 300)
 '(recentf-exclude
   (quote
    ("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|BRANCH_DESCRIPTION\\)\\'" ".*~$" ".*\\.(png|jpg|jpeg|pdf)$")))
 '(recentf-max-saved-items 60)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs-recentf")
 '(safe-local-variable-values
   (quote
    ((eval c-add-style "ethereum"
           (quote
            ("bsd"
             (indent-tabs-mode . t)
             (tab-width . 4)
             (c-basic-offset . 4)
             (c-hanging-braces-alist
              (substatement-open before)
              (brace-list-open))
             (c-hanging-colons-alist
              (member-init-intro before)
              (inher-intro)
              (case-label after)
              (label after)
              (access-label after))
             (c-cleanup-list scope-operator empty-defun-braces defun-close-semi)
             (c-offsets-alist
              (arglist-close . 0)
              (arglist-cont . 0)
              (arglist-cont-nonempty lambda
                                     (anchor)
                                     (save-excursion
                                       (back-to-indentation)
                                       (if
                                           (and
                                            (looking-at "[ 	]*{")
                                            (ignore-errors
                                              (backward-sexp 2)
                                              (looking-at "\\[[^]]*\\][ 	]*[({][^}]*$")))
                                           0
                                         (quote +))))
              (substatement-open . 0)
              (inline-open . 0)
              (innamespace . 0))))
           t)
     (dtrt-indent-explicit-offset . t)
     (eval c-add-style "ethereum"
           (quote
            ("bsd"
             (indent-tabs-mode . t)
             (tab-width . 4)
             (c-basic-offset . 4)
             (c-hanging-braces-alist
              (substatement-open before)
              (brace-list-open))
             (c-hanging-colons-alist
              (member-init-intro before)
              (inher-intro)
              (case-label after)
              (label after)
              (access-label after))
             (c-cleanup-list scope-operator empty-defun-braces defun-close-semi)
             (c-offsets-alist
              (arglist-close . 0)
              (arglist-cont . 0)
              (arglist-cont-nonempty lambda
                                     (anchor)
                                     (save-excursion
                                       (back-to-indentation)
                                       (if
                                           (and
                                            (looking-at "[ 	]*{")
                                            (ignore-errors
                                              (backward-sexp 2)
                                              (looking-at "\\[[^]]*\\][ 	]*[({][^}]*$")))
                                           0
                                         (quote +))))
              (substatement-open . 0)
              (inline-open . 0))))
           t))))
 '(save-place t nil (saveplace))
 '(savehist-file "~/.emacs-history")
 '(savehist-mode t)
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil)
 '(shell-completion-fignore (quote ("~" "#" "%")))
 '(shell-prompt-pattern "^[^>#$\\n]+ >>? *")
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(show-trailing-whitespace nil)
 '(shr-color-visible-luminance-min 60)
 '(smex-completion-method (quote ivy))
 '(speedbar-ignored-modes (quote (fundamental-mode mu4e-compose-mode)))
 '(speedbar-show-unknown-files t)
 '(split-height-threshold 30)
 '(split-width-threshold 100)
 '(split-window-preferred-function (quote fjl/split-window-sensibly))
 '(swiper-faces (quote (swiper-match-face-1 swiper-match-face-2)))
 '(tab-always-indent (quote always))
 '(tab-width 4)
 '(temp-buffer-resize-mode t)
 '(term-input-ignoredups t)
 '(tls-checktrust t)
 '(tramp-backup-directory-alist (quote ((".*" . "~/.emacs.d/tramp-backup"))))
 '(tramp-connection-timeout 10)
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(use-file-dialog nil)
 '(user-mail-address "fjl@twurst.com")
 '(vc-follow-symlinks t)
 '(vc-handled-backends nil)
 '(visible-bell nil)
 '(web-mode-enable-engine-detection t)
 '(weechat-auto-monitor-buffers t)
 '(weechat-buffer-kill-buffers-on-disconnect t)
 '(weechat-color-list
   (quote
    (unspecified "black" "dark gray" "dark red" "red" "dark green" "light green" "brown" "yellow" "sky blue" "light blue" "dark magenta" "magenta" "dark cyan" "light cyan" "gray" "white")))
 '(weechat-complete-nick-prefix-and-postfix-alist (quote (("gitter.*" "@" . ""))))
 '(weechat-connect-hook (quote (weechat-re-monitor-buffers weechat-auto-monitor)))
 '(which-func-modes (quote (python-mode)))
 '(window-combination-resize t)
 '(window-min-width 90)
 '(window-sides-slots (quote (1 nil 3 nil)))
 '(winner-mode t)
 '(with-editor-emacsclient-executable nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(weechat-highlight-face ((t (:background "gray30" :foreground "Cyan1")))))
