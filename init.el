;;; -*- lexical-binding: t; -*-

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'tls)
(require 'gnutls)
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
(require 'use-package)
(setq use-package-always-ensure t)

(setq load-prefer-newer t)
;; TODO see https://ryuslash.org/dotfiles/emacs/init.html#orgheadline39 to fix compilation (I hope)
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
