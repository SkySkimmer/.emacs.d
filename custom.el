(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (fish-mode agda2-mode beginend qml-mode biblio company-bibtex company-flx diff-hl yaml-mode ivy-hydra wgrep-ag wgrep hydra company-coq company nv-delete-back rainbow-mode counsel-projectile projectile systemd dired ws-butler which-key use-package undo-tree tuareg try org-bullets markdown-mode magit highlight-symbol flycheck-ocaml expand-region counsel auctex ace-window)))
 '(safe-local-variable-values
   (quote
    ((eval let
           ((unimath-topdir
             (expand-file-name
              (locate-dominating-file buffer-file-name "UniMath"))))
           (setq fill-column 100)
           (make-local-variable
            (quote coq-use-project-file))
           (setq coq-use-project-file nil)
           (setq-local coq-prog-args
                       (\`
                        ("-emacs" "-noinit" "-indices-matter" "-type-in-type" "-w" "-notation-overridden,-local-declaration,+uniform-inheritance,-deprecated-option" "-Q"
                         (\,
                          (concat unimath-topdir "UniMath"))
                         "UniMath"))))
     (git-commit-major-mode . git-commit-elisp-text-mode)
     (eval let
           ((unimath-topdir
             (expand-file-name
              (locate-dominating-file buffer-file-name "UniMath"))))
           (setq fill-column 100)
           (make-local-variable
            (quote coq-prog-args))
           (setq coq-prog-args
                 (\`
                  ("-emacs" "-noinit" "-indices-matter" "-Q"
                   (\,
                    (concat unimath-topdir "UniMath"))
                   "UniMath" "-w" "-notation-overridden,-local-declaration,+uniform-inheritance,-deprecated-option")))
           (if
               (equal buffer-file-name
                      (concat unimath-topdir "UniMath/Foundations/Resizing.v"))
               (setq coq-prog-args
                     (cons "-type-in-type" coq-prog-args))))
     (eval flycheck-cask-setup)
     (eval font-lock-add-keywords nil
           (quote
            (("defexamples\\|def-example-group\\| => \\| !!> \\| ~>"
              (0
               (quote font-lock-keyword-face)))
             ("(defexamples[[:blank:]]+\\(.*\\)"
              (1
               (quote font-lock-function-name-face))))))
     (compilation-search-path "/home/gaetan/dev/coq/coq")
     (camldebug-command-name . "/home/gaetan/dev/coq/coq/dev/ocamldebug-coq")
     (bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)")
     (eval let
           ((default-directory
              (locate-dominating-file buffer-file-name ".dir-locals.el")))
           (setq-local coq-prog-name
                       (expand-file-name "../bin/coqtop")))
     (eval let
           ((default-directory
              (locate-dominating-file buffer-file-name ".dir-locals.el")))
           (setq-local coq-prog-args
                       (\`
                        ("-coqlib"
                         (\,
                          (expand-file-name ".."))
                         "-R"
                         (\,
                          (expand-file-name "../theories"))
                         "Coq")))
           (setq-local coq-prog-name
                       (expand-file-name "../bin/coqtop")))
     (eval let*
           ((project-root
             (locate-dominating-file buffer-file-name "_CoqProject"))
            (dependencies-folder
             (expand-file-name "dependencies" project-root))
            (coq-path
             (split-string
              (or
               (getenv "COQPATH")
               "")
              ":" t)))
           (unless
               (memql dependencies-folder coq-path)
             (setenv "COQPATH"
                     (mapconcat
                      (function identity)
                      (cons dependencies-folder coq-path)
                      ":"))))
     (eval progn
           (let
               ((coq-root-directory
                 (when buffer-file-name
                   (locate-dominating-file buffer-file-name ".dir-locals.el")))
                (coq-project-find-file
                 (and
                  (boundp
                   (quote coq-project-find-file))
                  coq-project-find-file)))
             (setq tags-file-name
                   (concat coq-root-directory "TAGS")
                   camldebug-command-name
                   (concat coq-root-directory "dev/ocamldebug-coq"))
             (unless coq-project-find-file
               (setq compile-command
                     (concat "make -C " coq-root-directory)))
             (when coq-project-find-file
               (setq default-directory coq-root-directory))))
     (coq-prog-name . "/home/gaetan/Aarhus/HoTT-master/hoqtop")
     (coq-prog-args "-profile-ltac")
     (coq-prog-args "-bt")
     (coq-prog-name . "~/dev/coq/coq/bin/coqtop")
     (eval let
           ((default-directory
              (locate-dominating-file buffer-file-name ".dir-locals.el")))
           (setq-local coq-prog-args
                       (\`
                        ("-coqlib"
                         (\,
                          (expand-file-name ".."))
                         "-R"
                         (\,
                          (expand-file-name "."))
                         "Coq")))
           (setq-local coq-prog-name
                       (expand-file-name "../bin/coqtop")))
     (coq-prog-name . "../HoTT-master//hoqtop")
     (coq-prog-args "-emacs" "-boot")
     (eval let
           ((default-directory
              (locate-dominating-file buffer-file-name ".dir-locals.el")))
           (make-local-variable
            (quote coq-prog-args))
           (setq coq-prog-args
                 (\`
                  ("-indices-matter" "-boot" "-nois" "-coqlib"
                   (\,
                    (expand-file-name ".."))
                   "-R"
                   (\,
                    (expand-file-name "."))
                   "Coq" "-emacs"))))
     (coq-prog-args "-emacs" "-indices-matter")
     (eval let
           ((default-directory
              (locate-dominating-file buffer-file-name ".dir-locals.el")))
           (make-local-variable
            (quote coq-prog-name))
           (setq coq-prog-name
                 (expand-file-name "../hoqtop")))
     (coq-prog-name . "/home/gaetan/Aarhus/HoTT/hoqtop")
     (TeX-master . "Reference-Manual")
     (TeX-master . "main")
     (eval progn
           (let
               ((m31-root-directory
                 (when buffer-file-name
                   (locate-dominating-file buffer-file-name ".dir-locals.el")))
                (m31-project-find-file
                 (and
                  (boundp
                   (quote m31-project-find-file))
                  m31-project-find-file)))
             (when m31-root-directory
               (setq tags-file-name
                     (concat m31-root-directory "TAGS"))
               (add-to-list
                (quote compilation-search-path)
                m31-root-directory)
               (if
                   (not m31-project-find-file)
                   (setq compile-command
                         (concat "make -C " m31-root-directory))))
             (setq m31-executable
                   (concat m31-root-directory "andromeda.native"))))
     (eval progn
           (let
               ((coq-root-directory
                 (when buffer-file-name
                   (locate-dominating-file buffer-file-name ".dir-locals.el")))
                (coq-project-find-file
                 (and
                  (boundp
                   (quote coq-project-find-file))
                  coq-project-find-file)))
             (set
              (make-local-variable
               (quote tags-file-name))
              (concat coq-root-directory "TAGS"))
             (setq camldebug-command-name
                   (concat coq-root-directory "dev/ocamldebug-coq"))
             (unless coq-project-find-file
               (set
                (make-local-variable
                 (quote compile-command))
                (concat "make -C " coq-root-directory))
               (set
                (make-local-variable
                 (quote compilation-search-path))
                (cons coq-root-directory nil)))
             (when coq-project-find-file
               (setq default-directory coq-root-directory))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
