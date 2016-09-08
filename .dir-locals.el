((org-mode . ((eval . (load-file "../posts-config.el"))
              (eval .
                    (let ((root-dir (locate-dominating-file buffer-file-name ".dir-locals.el")))
                      (zezin-set-posts-info (concat root-dir "org")
                                            (concat root-dir "_posts")
                                            root-dir))))))
