(require 'package)

(package-initialize)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-refresh-contents)

(package-install 'org-plus-contrib)
(package-install 'htmlize)
(package-install 'rainbow-delimiters)

(require 'org)
(require 'htmlize)
(require 'rainbow-delimiters)

(defun org-publish-all-with-different-directory ()
  (setq org-publish-timestamp-directory ".timestamps/")
  (org-publish-all))

(with-eval-after-load 'rainbow-delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "dot")))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (setq org-html-htmlize-output-type 'css)
  (load-file "posts-config.el")
  (zezin-set-posts-info "org" "_posts" "."))
