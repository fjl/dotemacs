;;; agent-shell-qwen.el --- Qwen Code agent configurations -*- lexical-binding: t; -*-

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
;; This file includes Qwen Code-specific configurations.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell--interpolate-gradient "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(cl-defun agent-shell-qwen-make-authentication (&key login none)
  "Create Qwen Code authentication configuration.

LOGIN when non-nil indicates to use login-based authentication (default).
NONE when non-nil disables authentication.

Only one of LOGIN or NONE should be provided, never both."
  (when (and login none)
    (error "Cannot specify both :login and :none - choose one"))
  (unless (or login none)
    (error "Must specify either :login or :none"))
  (cond
   (login `((:login . ,login)))
   (none `((:none . t)))))

(defcustom agent-shell-qwen-authentication
  (agent-shell-qwen-make-authentication :login t)
  "Configuration for Qwen Code authentication.

For OAuth login-based authentication:

  (setq agent-shell-qwen-authentication
        (agent-shell-qwen-make-authentication :login t))

For no authentication (when using alternative authentication methods):

  (setq agent-shell-qwen-authentication
        (agent-shell-qwen-make-authentication :none t))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-qwen-command
  '("qwen" "--experimental-acp")
  "Command and parameters for the Qwen Code client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-qwen-environment
  nil
  "Environment variables for the Qwen Code client.

This should be a list of environment variables to be used when
starting the Qwen Code client process.

Example usage to set custom environment variables:

  (setq agent-shell-qwen-environment
        (`agent-shell-make-environment-variables'
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-qwen-make-agent-config ()
  "Create a Qwen Code CLI agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'qwen-code
   :mode-line-name "Qwen Code"
   :buffer-name "Qwen Code"
   :shell-prompt "qwen> "
   :shell-prompt-regexp "qwen> "
   :icon-name "qwen.png"
   :welcome-function #'agent-shell-qwen--welcome-message
   :needs-authentication (not (map-elt agent-shell-qwen-authentication :none))
   :authenticate-request-maker (lambda ()
                                 (cond
                                  ((map-elt agent-shell-qwen-authentication :login)
                                   (acp-make-authenticate-request :method-id "qwen-oauth"))
                                  ((map-elt agent-shell-qwen-authentication :none)
                                   nil)
                                  (t
                                   (user-error "Unknown authentication: %s" agent-shell-qwen-authentication))))
   :client-maker (lambda (buffer)
                   (agent-shell-qwen-make-client :buffer buffer))
   :install-instructions "See https://github.com/QwenLM/qwen-code for installation."))

(defun agent-shell-qwen-start ()
  "Start an interactive Qwen Code CLI agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-qwen-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-qwen-make-client (&key buffer)
  "Create a Qwen Code client with BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-qwen-command)
                                :command-params (cdr agent-shell-qwen-command)
                                :environment-variables agent-shell-qwen-environment
                                :context-buffer buffer))

(defun agent-shell-qwen--welcome-message (config)
  "Return Qwen Code ASCII art as welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-qwen--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-qwen--ascii-art ()
  "Generate Qwen Code ASCII art with Qwen-branded colors."
  ;; Based on:
  ;; https://github.com/QwenLM/qwen-code/tree/main/packages/cli/src/ui/components/Header.tsx
  ;; https://github.com/QwenLM/qwen-code/tree/main/packages/cli/src/ui/components/AsciiArt.ts
  ;; https://github.com/QwenLM/qwen-code/tree/main/packages/cli/src/ui/themes/theme.ts
  (let* ((text (string-trim "
██╗       ██████╗ ██╗    ██╗███████╗███╗   ██╗
╚██╗     ██╔═══██╗██║    ██║██╔════╝████╗  ██║
 ╚██╗    ██║   ██║██║ █╗ ██║█████╗  ██╔██╗ ██║
 ██╔╝    ██║▄▄ ██║██║███╗██║██╔══╝  ██║╚██╗██║
██╔╝     ╚██████╔╝╚███╔███╔╝███████╗██║ ╚████║
╚═╝       ╚══▀▀═╝  ╚══╝╚══╝ ╚══════╝╚═╝  ╚═══╝" "\n"))
         (is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (gradient-colors (if is-dark
                              '("#FF6B35" "#F7931E" "#FFD23F")
                            '("#E85D04" "#F48C06" "#FAA307")))
         (lines (split-string text "\n"))
         (result ""))
    (dolist (line lines)
      (let ((line-length (length line))
            (propertized-line ""))
        (dotimes (i line-length)
          (let* ((char (substring line i (1+ i)))
                 (progress (/ (float i) line-length))
                 (color (agent-shell--interpolate-gradient gradient-colors progress)))
            (setq propertized-line
                  (concat propertized-line
                          (propertize char 'font-lock-face `(:foreground ,color :inherit fixed-pitch))))))
        (setq result (concat result propertized-line "\n"))))
    (string-trim-right result)))

(provide 'agent-shell-qwen)

;;; agent-shell-qwen.el ends here
