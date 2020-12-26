;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; customize
(setq custom-file "custom.el")
(load-file custom-file)
