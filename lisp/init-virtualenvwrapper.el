(require 'virtualenvwrapper)

;;;###autoload
(add-hook 'eshell-mode-hook 'venv-initialize-eshell)
