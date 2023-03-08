;; -*- lexical-binding: t -*-

(require 'erlang)

;;;###autoload
(defun fjl/erlang-mode-hook ()
  (erlang-font-lock-level-3)
  (if (and window-system (fboundp 'imenu-add-to-menubar))
      (imenu-add-to-menubar "Imenu")))

;;;###autoload
(add-hook 'erlang-mode-hook 'fjl/erlang-mode-hook)

