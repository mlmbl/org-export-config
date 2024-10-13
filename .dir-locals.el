((org-mode
  (eval . (let ((project-dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
            (when project-dir
              ;; lisp ディレクトリ内のすべての .el または .elc ファイルをロード
              (dolist (file (directory-files (expand-file-name "lisp" project-dir) t "\\.el\\(c\\)?$"))
                (load file)))))
  (eval . (setq org-export-async-init-file
                (expand-file-name "org-export-async-init.el" (file-name-directory (or load-file-name buffer-file-name)))))))
