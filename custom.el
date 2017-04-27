(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (diff-hl yaml-mode ivy-hydra wgrep-ag wgrep hydra company-coq company nv-delete-back rainbow-mode counsel-projectile projectile systemd dired ws-butler which-key use-package undo-tree tuareg try org-bullets markdown-mode magit highlight-symbol flycheck-ocaml expand-region counsel auctex ace-window)))
 '(safe-local-variable-values
   (quote
    ((coq-prog-name . "../HoTT-master//hoqtop")
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
