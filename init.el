;;; -*- lexical-binding: t; -*-

(let ((now (current-time)))
  (message "Init start...")

  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file 'noerror)

  (defvar package-enable-at-startup)
  (setq package-enable-at-startup nil)

  (setq load-prefer-newer t)

  (require 'cl-macs)

  (declare-function org-babel-tangle-file "ob-tangle")
  (let* ((emacs-d "~/.emacs.d/")
         (conforg (concat emacs-d "config.org"))
         (confel (concat emacs-d "config.el")))
    (unless (file-newer-than-file-p confel conforg)
      (require 'ob-tangle)
      (org-babel-tangle-file conforg confel "emacs-lisp"))
    (load-file confel))

  (let ((elapsed (float-time (time-subtract (current-time) now))))
    (message "Init done (%.3fs)" elapsed)))
