(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((eval progn
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
 )
