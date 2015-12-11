(require 'mu4e)
(require 'mu4e-multi)
(require 's)
(require 'cl-lib)
(require 'init-commands)

;;; Multi-account setup

(setq mu4e-multi-account-alist
      '(("twurst.com"
         (mu4e-inbox-folder   . "/twurst.com/INBOX")
         (mu4e-archive-folder . "/twurst.com/archive")
         (mu4e-sent-folder    . "/twurst.com/Sent Messages")
         (mu4e-drafts-folder  . "/twurst.com/Drafts")
         (mu4e-trash-folder   . "/twurst.com/Deleted Messages")
         (mu4e-junk-folder    . "/twurst.com/Junk")
         (user-mail-address   . "fjl@twurst.com"))
        ("ethdev.com"
         (mu4e-inbox-folder   . "/ethdev.com/Inbox")
         (mu4e-archive-folder . "/ethdev.com/[Gmail]/Archive")
         (mu4e-sent-folder    . "/ethdev.com/[Gmail]/Sent Mail")
         (mu4e-trash-folder   . "/ethdev.com/[Gmail]/Trash")
         (mu4e-drafts-folder  . "/ethdev.com/[Gmail]/Drafts")
         (mu4e-junk-folder    . "/ethdev.com/[Gmail]/Spam")
         (user-mail-address   . "felix@ethdev.com"))))

(setq mu4e-multi-default-account "twurst.com")

(mu4e-multi-enable)

(defun fjl/mu4e-expand-mailbox-alt (var)
  (let ((folders
         (cl-reduce (lambda (folders account)
                      (let ((b (assoc var (cdr account))))
                        (if (cdr b)
                            (cons (concat "maildir:\"" (cdr b) "\"") folders)
                          folders)))
                    mu4e-multi-account-alist :initial-value nil)))
    (concat "(" (s-join " OR " folders) ")")))

(defun fjl/mu4e-expand-bookmark (bm)
  "Expands <in x> in mu4e bookmark definitions."
  (cons (replace-regexp-in-string "<in \\([^>]*\\)>"
                                  (lambda (s)
                                    (fjl/mu4e-expand-mailbox-alt (intern (match-string 1 s))))
                                  (car bm))
        (cdr bm)))

;; Message view setup

(defun fjl/mu4e-view-mode-hook ()
  (setq word-wrap t)
  (setq truncate-lines nil))

(add-hook 'mu4e-view-mode-hook 'fjl/mu4e-view-mode-hook)

;; Commands and Bindings

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

(defun fjl/render-html-mail ()
  "Render HTML mail with shr."
  (require 'shr)
  (let ((dom (libxml-parse-html-region (point-min) (point-max)))
        ;; Disable image loading.
        (shr-inhibit-images t)
        ;; Tell shr to fit into the wrap width.
        (shr-width 99))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))

;; Misc Settings

(setq mu4e-attachment-dir "/Users/fjl/Downloads"
      mu4e-use-fancy-chars 'threads
      mu4e-view-show-images t
      mu4e-view-show-addresses t
      mu4e-change-filenames-when-moving t ;; for mbsync
      mu4e-completing-read-function 'ivy-completing-read
      mu4e-compose-signature nil
      mu4e-headers-skip-duplicates t
      mu4e-headers-include-related t
      mu4e-compose-signature-auto-include nil
      mu4e-html2text-command 'fjl/render-html-mail ;; or "w3m -T text/html -S -O utf8 -dump -cols 90"
      mu4e-get-mail-command "mbsync -aVX | cat" ;; | cat to get rid of tty progress indicator
      mu4e-maildir "~/Mail"
      mu4e-user-mailing-lists '(("go-ethereum.ethereum.github.com" . "geth")))

(setq mu4e-bookmarks
      (mapcar 'fjl/mu4e-expand-bookmark
              '(("flag:unread AND NOT <in mu4e-trash-folder>" "Unread messages" ?u)
                ("date:today..now AND NOT <in mu4e-trash-folder>" "Today's messages" ?t)
                ("date:7d..now AND NOT <in mu4e-trash-folder>" "Last 7 days" ?w)
                ("mime:image/*" "Messages with images" ?p)
                ("<in mu4e-inbox-folder>" "INBOX" ?I)
                ("<in mu4e-archive-folder>" "Archive" ?a)
                ("<in mu4e-inbox-folder> AND NOT flag:list" "INBOX (no list mail)" ?i))))

(provide 'init-mu4e)
