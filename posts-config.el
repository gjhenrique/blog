(defun zezin-set-posts-info (org-path posts-path res-path)
  (setq org-publish-project-alist
        `(("org-zezin"
           :base-directory ,org-path
           :base-extension "org"
           :publishing-directory ,posts-path
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 6
           :html-extension "html"
           :body-only t
           :with-toc nil
           :section-numbers nil
           :table-of-contents nil
           :author "Guilherme Henrique"
           :email "me@gjhenrique.com")
          ("org-static-zezin"
           :base-directory ,org-path
           :base-extension "css\\|js\\|png\\|jpg\\|ico\\|gif\\|pdf\\|mp3\\|flac\\|ogg\\|swf\\|php\\|markdown\\|md\\|html\\|htm\\|sh\\|xml\\|gz\\|bz2\\|vcf\\|zip\\|txt\\|tex\\|otf\\|ttf\\|eot\\|rb\\|yml\\|htaccess\\|gitignore\\|svg"
           :publishing-directory ,res-path
           :recursive t
           :publishing-function org-publish-attachment)
          ("zezin" :components ("org-zezin" "org-static-zezin")))))

;; Shinning face
;; Used in the meta post
(defface shinning-face
  '((t (:background "white" :foreground "red")))
  "Face to highlight the Axe word")

(defun axe-highlight ()
  (font-lock-add-keywords nil
			  '(("\\<\\(Axe\\|axe\\)\\>" 1
			     'shinning-face t))))

(add-hook 'ruby-mode-hook 'axe-highlight)
(add-hook 'python-mode-hook 'axe-highlight)
