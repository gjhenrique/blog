(require 'package)

(package-initialize)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))

(package-refresh-contents)

(package-install 'htmlize)
(package-install 'rainbow-delimiters)
(package-install 'yaml-mode)
(package-install 'clojure-mode)
(package-install 'go-mode)
(package-install 'toml-mode)
(package-install 'dockerfile-mode)
(package-install 'highlight-numbers)

(require 'org)
(require 'htmlize)
(require 'rainbow-delimiters)
(require 'yaml-mode)
(require 'clojure-mode)
(require 'go-mode)
(require 'toml-mode)
(require 'dockerfile-mode)
(require 'highlight-numbers)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(defun zezin-load-theme ()
  (package-install 'doom-themes)
  (require 'doom-themes)
  (load-theme 'doom-one 'no-confirm) )

(defun zezin-generate-themefile ()
  (zezin-load-theme)

  ;; Fix "Invalid face: tab-line-tab"
  (require 'tab-line)

  ;; Looks like a bug where default face inherit is nil
  (set-face-attribute 'default nil :inherit 'unspecified)

  (org-html-htmlize-generate-css)
  (with-current-buffer "*html*"
    (message "%s" (buffer-string))))

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
  (zezin-set-posts-info "org" "content/posts" "static")

  (defun zezin-rewrite-link (orig-fun &rest args )
    "Replaces org-html-link images with absolue URLs"
    (let ((res (apply orig-fun args)))
      (if (and (stringp res) (string-match-p "<img src=" res))
          (replace-regexp-in-string "./res" "/res" res)
        res)))

  (advice-add #'org-html-link :around #'zezin-rewrite-link))
