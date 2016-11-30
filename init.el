(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq tls-checktrust t)
(setq gnutls-verify-error t)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives (list '("gnu" . "https://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
(package-install 'use-package))
(setq use-package-always-ensure t)

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
