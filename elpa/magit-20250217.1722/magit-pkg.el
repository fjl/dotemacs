;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250217.1722"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.0.2.0")
    (llama         "0.6.0")
    (magit-section "4.3.0")
    (seq           "2.24")
    (transient     "0.8.4")
    (with-editor   "3.4.3"))
  :url "https://github.com/magit/magit"
  :commit "8c27c910cab356173165d03edc9fb7b494574d51"
  :revdesc "8c27c910cab3"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
