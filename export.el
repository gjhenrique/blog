(require 'package)

(package-initialize)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-refresh-contents)

(package-install 'org-plus-contrib)
(package-install 'htmlize)
(package-install 'rainbow-delimiters)
(package-install 'yaml-mode)
(package-install 'clojure-mode)
(package-install 'go-mode)
(package-install 'toml-mode)
(package-install 'dockerfile-mode)

(require 'org)
(require 'htmlize)
(require 'rainbow-delimiters)
(require 'yaml-mode)
(require 'clojure-mode)
(require 'go-mode)
(require 'toml-mode)
(require 'dockerfile-mode)

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
  (setq org-html-html5-fancy t
        org-html-doctype "html5")
  (load-file "posts-config.el")
  (let ((root-dir (locate-dominating-file buffer-file-name ".dir-locals.el")))
    (zezin-set-posts-info (concat root-dir "org") (concat root-dir "content/posts") (concat root-dir "assets")))

  (defun zezin-rewrite-link (orig-fun &rest args )
    "Replaces org-html-link images with absolue URLs"
    (let ((res (apply orig-fun args)))
      (if (and (stringp res) (string-match-p "<img src=" res))
          (replace-regexp-in-string "./res" "/res" res)
        res)))

  (advice-add #'org-html-link :around #'zezin-rewrite-link))
