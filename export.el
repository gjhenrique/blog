(setq org-html-htmlize-output-type 'css)

; Execute graphviz in our posts ;)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "dot")))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)


(load-file "posts-config.el")
(zezin-set-posts-info "org" "_posts" ".")
