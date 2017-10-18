;;; -*- lexical-binding: t; -*-

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(defvar package-enable-at-startup)
(setq package-enable-at-startup nil)

(setq load-prefer-newer t)

;; TODO see https://ryuslash.org/dotfiles/emacs/init.html#orgheadline39 to fix compilation (I hope)
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
