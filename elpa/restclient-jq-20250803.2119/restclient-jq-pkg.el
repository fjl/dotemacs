;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "restclient-jq" "20250803.2119"
  "Support for setting restclient vars from jq expressions."
  '((restclient "20200502.831")
    (jq-mode    "0.4.1")
    (emacs      "24.4"))
  :url "https://github.com/pashky/restclient.el"
  :commit "6764278a3d63520eaf117344d8dc23654b640645"
  :revdesc "6764278a3d63"
  :keywords '("tools" "comm" "http" "jq")
  :authors '(("Cameron Dorrat" . "cdorrat@gmail.com"))
  :maintainers '(("Cameron Dorrat" . "cdorrat@gmail.com")))
