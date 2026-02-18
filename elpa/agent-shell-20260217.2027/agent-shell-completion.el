;;; agent-shell-completion.el --- Completion support for agent-shell. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'map)
(require 'agent-shell-project)

(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell--project-files "agent-shell-project")

(defvar agent-shell--state)

(defcustom agent-shell-file-completion-enabled t
  "Non-nil automatically enables file completion when starting shells."
  :type 'boolean
  :group 'agent-shell)

(defun agent-shell--completion-bounds (char-class trigger-char)
  "Find completion bounds for CHAR-CLASS, if TRIGGER-CHAR precedes them.
Returns alist with :start and :end if TRIGGER-CHAR is found before
the word, nil otherwise."
  (save-excursion
    (when-let* ((end (progn (skip-chars-forward char-class) (point)))
                (start (progn (skip-chars-backward char-class) (point)))
                ((eq (char-before start) trigger-char)))
      `((:start . ,start) (:end . ,end)))))

(defun agent-shell--capf-exit-with-space (_string _status)
  "Insert space after completion."
  (insert " "))

(defun agent-shell--file-completion-at-point ()
  "Complete project files after @."
  (when-let* ((bounds (agent-shell--completion-bounds "[:alnum:]/_.-" ?@))
              (files (agent-shell--project-files)))
    (list (map-elt bounds :start) (map-elt bounds :end)
          files
          :exclusive 'no
          :company-kind (lambda (f) (if (string-suffix-p "/" f) 'folder 'file))
          :exit-function #'agent-shell--capf-exit-with-space)))

(defun agent-shell--command-completion-at-point ()
  "Complete available commands after /."
  (when-let* ((bounds (agent-shell--completion-bounds "[:alnum:]_-" ?/))
              (commands (with-current-buffer (agent-shell--shell-buffer
                                              :no-error t :no-create t)
                          (map-elt agent-shell--state :available-commands)))
              (descriptions (mapcar (lambda (c)
                                      (cons (map-elt c 'name)
                                            (map-elt c 'description)))
                                    commands)))
    (list (map-elt bounds :start) (map-elt bounds :end)
          (mapcar #'car descriptions)
          :exclusive t
          :annotation-function
          (lambda (name)
            (when-let* ((desc (map-elt descriptions name)))
              (concat "  " desc)))
          :company-kind (lambda (_) 'function)
          :exit-function #'agent-shell--capf-exit-with-space)))

(defun agent-shell--trigger-completion-at-point ()
  "Trigger completion when @ or / is typed at a word boundary.
Only triggers when the character is at line start or after whitespace,
preventing spurious completions mid-word or in paths."
  (when (and (memq (char-before) '(?@ ?/))
             (or (= (point) (1+ (line-beginning-position)))
                 (memq (char-before (1- (point))) '(?\s ?\t ?\n))))
    (completion-at-point)))

(define-minor-mode agent-shell-completion-mode
  "Toggle agent shell completion with @ or / prefix."
  :lighter " @/Compl"
  (if agent-shell-completion-mode
      (progn
        (add-hook 'completion-at-point-functions #'agent-shell--file-completion-at-point nil t)
        (add-hook 'completion-at-point-functions #'agent-shell--command-completion-at-point nil t)
        (add-hook 'post-self-insert-hook #'agent-shell--trigger-completion-at-point nil t))
    (remove-hook 'completion-at-point-functions #'agent-shell--file-completion-at-point t)
    (remove-hook 'completion-at-point-functions #'agent-shell--command-completion-at-point t)
    (remove-hook 'post-self-insert-hook #'agent-shell--trigger-completion-at-point t)))

(provide 'agent-shell-completion)

;;; agent-shell-completion.el ends here
