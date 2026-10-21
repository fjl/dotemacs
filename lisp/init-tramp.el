;; -*- lexical-binding: t -*-

;; Don't let VC touch remote files; magit is used for that and every VC check costs
;; a remote git invocation. This used to be done by adding the TRAMP regexp to
;; `vc-ignore-dir-regexp', but project.el uses that regexp as the stop condition
;; of its root search, which made project detection impossible for remote files.
(define-advice vc-registered (:around (fn file) fjl/not-for-remote-files)
  (and (not (file-remote-p file)) (funcall fn file)))

(setq tramp-copy-size-limit (* 1024 1024)) ;; 1MB

(connection-local-set-profile-variables
 'fjl/tramp-direct-async
 '((tramp-direct-async-process . t)))
(dolist (method '("ssh" "scp" "sshx"))
  (connection-local-set-profiles
   `(:application tramp :protocol ,method)
   'fjl/tramp-direct-async))

(connection-local-set-profile-variables
 'fjl/tramp-dired
 '((dired-check-symlinks . nil)))
(connection-local-set-profiles
 '(:application tramp)
 'fjl/tramp-dired)

(provide 'init-tramp)
