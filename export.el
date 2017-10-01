(require 'package)

(package-initialize)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-refresh-contents)

(package-install 'org-plus-contrib)
(package-install 'htmlize)
(package-install 'rainbow-delimiters)
(package-install 'yaml-mode)

(require 'org)
(require 'htmlize)
(require 'rainbow-delimiters)
(require 'yaml-mode)

(defun org-publish-with-different-timestamp-directory ()
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
  (setq org-export-with-sub-superscripts nil)
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (setq org-html-htmlize-output-type 'css)
  (load-file "posts-config.el")
  (zezin-set-posts-info "org" "_posts" "."))
