((nil . ((eval .
               (let ((root-dir (locate-dominating-file buffer-file-name ".dir-locals.el")))
                 (setq org-publish-project-alist
                       `(("org-zezin"
                          :base-directory ,(concat root-dir "org")
                          :base-extension "org"
                          :publishing-directory ,(concat root-dir "_posts")
                          :recursive t
                          :publishing-function org-html-publish-to-html
                          :headline-levels 6
                          :html-extension "html"
                          :body-only t
                          :with-toc nil
                          :section-numbers nil
                          :table-of-contents nil
                          :author "Guilherme Henrique"
                          :email "gjhenrique@gmail.com")
                         ("org-static-zezin"
                          :base-directory ,(concat root-dir "org")
                          :base-extension "css\\|js\\|png\\|jpg\\|ico\\|gif\\|pdf\\|mp3\\|flac\\|ogg\\|swf\\|php\\|markdown\\|md\\|html\\|htm\\|sh\\|xml\\|gz\\|bz2\\|vcf\\|zip\\|txt\\|tex\\|otf\\|ttf\\|eot\\|rb\\|yml\\|htaccess\\|gitignore\\|svg"
                          :publishing-directory ,root-dir
                          :recursive t
                          :publishing-function org-publish-attachment)
                         ("zezin" :components ("org-zezin" "org-static-zezin")))))))))
