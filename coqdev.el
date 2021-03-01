;;; coqdev.el --- Emacs helpers for Coq development  -*- lexical-binding:t -*-

;; Copyright (C) 2018 The Coq Development Team

;; Maintainer: coqdev@inria.fr

;;; Commentary:

;; Helpers to set compilation commands, proof general variables, etc
;; for Coq development

;; You can disable individual features without editing this file by
;; using `remove-hook', for instance
;; (remove-hook 'hack-local-variables-hook #'coqdev-setup-compile-command)

;;; Installation:

;; To use this, with coqdev.el located at /path/to/coqdev.el, add the
;; following to your init:

;; (add-to-list 'load-path "/path/to/coqdev/")
;; (require 'coqdev)

;; If you load this file from a git repository, checking out an old
;; commit will make it disappear and cause errors for your Emacs
;; startup. To ignore those errors use (require 'coqdev nil t). If you
;; check out a malicious commit Emacs startup would allow it to run
;; arbitrary code, to avoid this you can copy coqdev.el to any
;; location and adjust the load path accordingly (of course if you run
;; ./configure to compile Coq it is already too late).

;;; Code:

(require 'seq)
(require 'subr-x)

(defun coqdev-default-directory ()
  "Return the Coq repository containing `default-directory'."
  (let ((dir (seq-some
              (lambda (f) (locate-dominating-file default-directory f))
              '("META.coq" "META.coq.in"))))
    (when dir (expand-file-name dir))))

(defun coqdev-setup-compile-command ()
  "Setup `compile-command' for Coq development."
  (let ((dir (coqdev-default-directory)))
    (when dir (setq-local compile-command (concat "cd " (shell-quote-argument dir) "; make ")))))
(add-hook 'hack-local-variables-hook #'coqdev-setup-compile-command)

(defvar camldebug-command-name) ; from camldebug.el (caml package)
(defvar ocamldebug-command-name) ; from ocamldebug.el (tuareg package)
(defun coqdev-setup-camldebug ()
  "Setup ocamldebug for Coq development.

Specifically `camldebug-command-name' and `ocamldebug-command-name'."
  (let ((dir (coqdev-default-directory)))
    (when dir
      (setq-local camldebug-command-name
                  (concat dir "dev/ocamldebug-coq"))
      (setq-local ocamldebug-command-name
                  (concat dir "dev/ocamldebug-coq")))))
(add-hook 'hack-local-variables-hook #'coqdev-setup-camldebug)

(defun coqdev-setup-tags ()
  "Setup `tags-file-name' for Coq development."
  (let ((dir (coqdev-default-directory)))
    (when dir (setq-local tags-file-name (concat dir "TAGS")))))
(add-hook 'hack-local-variables-hook #'coqdev-setup-tags)

(defvar coq-prog-args)
(defvar coq-prog-name)

;; Lets us detect whether there are file local variables
;; even though PG sets it with `setq' when there's a _Coqproject.
;; Also makes sense generally, so might make it into PG someday.
(make-variable-buffer-local 'coq-prog-args)
(setq-default coq-prog-args nil)

(defun coqdev-setup-proofgeneral ()
  "Setup Proofgeneral variables for Coq development.

Note that this function is executed before _Coqproject is read if it exists."
  (let ((dir (coqdev-default-directory)))
    (when dir
      (setq-local coq-prog-name (concat dir "_build/default/dev/shim/coqtop-prelude")))))
(add-hook 'hack-local-variables-hook #'coqdev-setup-proofgeneral)

(defvar coqdev-ocamldebug-command "dune exec -- dev/dune-dbg"
  "Command run by `coqdev-ocamldebug'")

(declare-function comint-check-proc "comint")
(declare-function tuareg--split-args "tuareg")
(declare-function ocamldebug-filter "ocamldebug")
(declare-function ocamldebug-sentinel "ocamldebug")
(declare-function ocamldebug-mode "ocamldebug")
(declare-function ocamldebug-set-buffer "ocamldebug")
(defun coqdev-ocamldebug ()
  "Runs a command in an ocamldebug buffer."
  (interactive)
  (require 'ocamldebug)
  (let* ((dir (read-directory-name "Run from directory: "
                                   (coqdev-default-directory)))
         (name "ocamldebug-coq")
         (buffer-name (concat "*" name "*")))
    (pop-to-buffer buffer-name)
    (unless (comint-check-proc buffer-name)
      (setq default-directory dir)
      (setq coqdev-ocamldebug-command
            (read-from-minibuffer "Command to run: "
                                  coqdev-ocamldebug-command))
      (let* ((cmdlist (tuareg--split-args coqdev-ocamldebug-command))
             (cmdlist (mapcar #'substitute-in-file-name cmdlist)))
        (apply #'make-comint name
               (car cmdlist)
               nil
               (cdr cmdlist))
        (set-process-filter (get-buffer-process (current-buffer))
                            #'ocamldebug-filter)
        (set-process-sentinel (get-buffer-process (current-buffer))
                              #'ocamldebug-sentinel)
        (ocamldebug-mode)))
    (ocamldebug-set-buffer)
    (insert "source dune_db_409")))

;; Provide correct breakpoint setting in dune wrapped libraries
;; (assuming only 1 library/dune file)
(defun coqdev--read-from-file (file)
  "Read FILE as a list of sexps. If invalid syntax, return nil and message the error."
  (with-temp-buffer
    (save-excursion
      (insert "(\n")
      (insert-file-contents file)
      (goto-char (point-max))
      (insert "\n)\n"))
    (condition-case err
        (read (current-buffer))
      ((error err) (message "Error reading file %S: %S" file err)))))

(defun coqdev--find-single-library (sexps)
  "If list SEXPS has a single element whose `car' is \"library\", return it.
Otherwise return `nil'."
  (let ((libs (seq-filter (lambda (elt) (equal (car elt) 'library)) sexps)))
    (and (null (cdr libs)) (car libs))))

(defun coqdev--dune-library-name (lib)
  "With LIB a dune-syntax library stanza, get its name as a string."
  (let ((field (or (seq-find (lambda (field) (and (consp field) (equal (car field) 'name))) lib)
                   (seq-find (lambda (field) (and (consp field) (equal (car field) 'public\_name))) lib))))
    (symbol-name (car (cdr field)))))

(defun coqdev--upcase-first-char (arg)
  "Set the first character of ARG to uppercase."
  (concat (upcase (substring arg 0 1)) (substring arg 1 (length arg))))

(defun coqdev--real-module-name (filename)
  "Return module name for ocamldebug, taking into account dune wrapping.
(for now only understands dune files with a single library stanza)"
  (let ((mod (substring filename (string-match "\\([^/]*\\)\\.ml$" filename) (match-end 1)))
        (dune (concat (file-name-directory filename) "dune")))
    (if (file-exists-p dune)
        (if-let* ((lib (coqdev--find-single-library (coqdev--read-from-file dune)))
                  (is-wrapped (null (seq-contains-p lib '(wrapped false))))
                  (libname (coqdev--dune-library-name lib)))
            (concat libname "__" (coqdev--upcase-first-char mod))
          mod)
      mod)))

(with-eval-after-load 'ocamldebug
  (defun ocamldebug-module-name (arg)
    (coqdev--real-module-name arg)))

;; This Elisp snippet adds a regexp parser for the format of Anomaly
;; backtraces (coqc -bt ...), to the error parser of the Compilation
;; mode (C-c C-c: "Compile command: ..."). File locations in traces
;; are recognized and can be jumped from easily in the *compilation*
;; buffer.
(defvar compilation-error-regexp-alist-alist)
(defvar compilation-error-regexp-alist)
(with-eval-after-load 'compile
  (add-to-list
   'compilation-error-regexp-alist-alist
   '(coq-backtrace
     "^ *\\(?:raise\\|frame\\) @ file \\(\"?\\)\\([^,\" \n\t<>]+\\)\\1,\
      lines? \\([0-9]+\\)-?\\([0-9]+\\)?\\(?:$\\|,\
      \\(?: characters? \\([0-9]+\\)-?\\([0-9]+\\)?:?\\)?\\)"
     2 (3 . 4) (5 . 6)))
  (add-to-list 'compilation-error-regexp-alist 'coq-backtrace))

(defvar bug-reference-bug-regexp)
(defvar bug-reference-url-format)
(defun coqdev-setup-bug-reference-mode ()
  "Setup `bug-reference-bug-regexp' and `bug-reference-url-format' for Coq.

This does not enable `bug-reference-mode'."
  (let ((dir (coqdev-default-directory)))
    (when dir
      (setq-local bug-reference-bug-regexp "#\\(?2:[0-9]+\\)")
      (setq-local bug-reference-url-format "https://github.com/coq/coq/issues/%s")
      (when (derived-mode-p 'prog-mode) (bug-reference-prog-mode 1)))))
(add-hook 'hack-local-variables-hook #'coqdev-setup-bug-reference-mode)

(provide 'coqdev)
;;; coqdev ends here
