((org-mode
  (eval . (setq org-export-async-init-file
                (expand-file-name "org-export-async-init.el" (file-name-directory (or load-file-name buffer-file-name)))))
  (eval . (setq org-export-in-background t))))

