(require 'mu4e)
(require 's)
(require 'cl-lib)
(require 'init-commands)

(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "Private"
        :match-func (lambda (msg)
                      (when msg
                        (or
                         (s-prefix-p "/twurst.com" (mu4e-message-field msg :maildir))
                         (mu4e-message-contact-field-matches msg :to "fjl@twurst.com")
                         (mu4e-message-contact-field-matches msg :to "br@twurst.com"))))
        :vars '((user-mail-address   . "fjl@twurst.com")
                (user-full-name      . "Felix Lange")
                (mu4e-inbox-folder   . "/twurst.com/INBOX")
                (mu4e-refile-folder  . "/twurst.com/archive")
                (mu4e-sent-folder    . "/twurst.com/Sent Messages")
                (mu4e-drafts-folder  . "/twurst.com/Drafts")
                (mu4e-trash-folder   . "/twurst.com/Deleted Messages")
                (mu4e-junk-folder    . "/twurst.com/Junk")))
       (make-mu4e-context
        :name "Ethereum"
        :match-func (lambda (msg)
                      (when msg
                        (or
                         (s-prefix-p "/ethereum.org" (mu4e-message-field msg :maildir))
                         (mu4e-message-contact-field-matches msg :to "felix@ethdev.com")
                         (mu4e-message-contact-field-matches msg :to "fjl@ethereum.org"))))
        :vars '((user-mail-address   . "fjl@ethereum.org")
                (user-full-name      . "Felix Lange")
                (mu4e-inbox-folder   . "/ethereum.org/INBOX")
                (mu4e-refile-folder  . "/ethereum.org/[Gmail].All Mail")
                (mu4e-sent-folder    . "/ethereum.org/[Gmail].Sent Mail")
                (mu4e-trash-folder   . "/ethereum.org/[Gmail].Trash")
                (mu4e-drafts-folder  . "/ethereum.org/[Gmail].Drafts")
                (mu4e-junk-folder    . "/ethereum.org/[Gmail].Spam")))))

(defun fjl/mu4e-expand-mailbox-alt (var)
  (let ((folders
         (cl-reduce (lambda (folders account)
                      (let ((b (assoc var (mu4e-context-vars account))))
                        (if (cdr b)
                            (cons (concat "maildir:\"" (cdr b) "\"") folders)
                          folders)))
                    mu4e-contexts :initial-value nil)))
    (concat "(" (s-join " OR " folders) ")")))

(defun fjl/mu4e-expand-bookmark (bm)
  "Expands <in x> in mu4e bookmark definitions."
  (replace-regexp-in-string
   "<in \\([^>]*\\)>"
   (lambda (s) (fjl/mu4e-expand-mailbox-alt (intern (match-string 1 s))))
   bm))

(defun fjl/mu4e-sent-folder ()
  (if (string= (message-sendmail-envelope-from) "fjl@ethereum.org")
      'delete ;; for Gmail
    'sent))

(setq mu4e-sent-messages-behavior #'fjl/mu4e-sent-folder)

;; Message view setup

(defun fjl/mu4e-view-mode-hook ()
  (setq word-wrap t)
  (setq truncate-lines nil))

(add-hook 'mu4e-view-mode-hook 'fjl/mu4e-view-mode-hook)

;; Commands and Bindings

(define-key mu4e-headers-mode-map (kbd "C-c C-u") 'mu4e-update-index)
(define-key mu4e-view-mode-map (kbd "C-c C-u") 'mu4e-update-index)

;; Patch the trash mark action to not mark the message as trashed.
;; mbsync interprets 'trashed' as 'deleted', so the messages are
;; expunged. Move them to the trash folder instead.
;;
;; Also mark them as read when moving.
(defvar fjl/mu4e-trash-mark
  `(trash :char "d"
          :prompt "dtrash"
          :dyn-target ,(lambda (target msg) (mu4e-get-trash-folder msg))
          :action ,(lambda (docid msg target) (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-N"))))
(setq mu4e-marks
      (mapcar (lambda (m)
                (if (eq (car m) 'trash) fjl/mu4e-trash-mark m))
              mu4e-marks))

;; Misc Settings

(setq mu4e-attachment-dir "~/Downloads/"
      mu4e-change-filenames-when-moving t ;; for mbsync
      mu4e-completing-read-function 'ivy-completing-read
      mu4e-compose-context-policy 'ask-if-none
      mu4e-compose-format-flowed t
      mu4e-compose-signature nil
      mu4e-compose-signature-auto-include nil
      mu4e-context-policy 'pick-first
      mu4e-headers-include-related t
      mu4e-headers-skip-duplicates t
      mu4e-maildir "~/Mail"
      mu4e-use-fancy-chars nil
      mu4e-user-mail-address-list '("fjl@twurst.com" "br@twurst.com" "felix@ethdev.com" "fjl@ethereum.org")
      mu4e-user-mailing-lists '(("go-ethereum.ethereum.github.com" . "geth"))
      mu4e-view-show-addresses t
      mu4e-view-show-images t)

(setq mu4e-bookmarks
      (list
       (make-mu4e-bookmark
        :name  "Unread"
        :query '(fjl/mu4e-expand-bookmark "flag:unread AND NOT <in mu4e-trash-folder>")
        :key ?U)
       (make-mu4e-bookmark
        :name  "Unread (nolist)"
        :query '(fjl/mu4e-expand-bookmark "flag:unread AND NOT <in mu4e-trash-folder> AND NOT flag:list")
        :key ?u)
       (make-mu4e-bookmark
        :name "Today's messages"
        :query '(fjl/mu4e-expand-bookmark "date:today..now AND NOT <in mu4e-trash-folder>")
        :key ?t)
       (make-mu4e-bookmark
        :name "Last 7 days"
        :query '(fjl/mu4e-expand-bookmark "date:7d..now AND NOT <in mu4e-trash-folder>")
        :key ?w)
       (make-mu4e-bookmark
        :name "INBOX"
        :query '(fjl/mu4e-expand-bookmark "<in mu4e-inbox-folder>")
        :key ?I)
       (make-mu4e-bookmark
        :name "INBOX (nolist)"
        :query '(fjl/mu4e-expand-bookmark "<in mu4e-inbox-folder> AND NOT flag:list")
        :key ?i)
       (make-mu4e-bookmark
        :name "Archive"
        :query '(fjl/mu4e-expand-bookmark "<in mu4e-refile-folder>")
        :key ?a)))

(provide 'init-mu4e)
