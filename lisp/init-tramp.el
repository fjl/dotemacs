;; -*- lexical-binding: t -*-

(after-package vc-hooks
  (require 'tramp)
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)))

(connection-local-set-profile-variables
 'fjl/tramp-direct-async
 '((tramp-direct-async-process . t)))
(dolist (method '("ssh" "scp"))
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
