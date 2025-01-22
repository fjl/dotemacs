;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250120.1245"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.0.1.0")
    (dash          "2.19.1")
    (magit-section "4.2.0")
    (seq           "2.24")
    (transient     "0.8.2")
    (with-editor   "3.4.3"))
  :url "https://github.com/magit/magit"
  :commit "e9614a336ffef61e8660188a95ae5d78674ed36d"
  :revdesc "e9614a336ffe"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
