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
(package-install 'highlight-quoted)
(package-install 'typescript-mode)

(require 'org)
(require 'htmlize)
(require 'css-mode)
(require 'diff-mode)

(require 'rainbow-delimiters)
(require 'yaml-mode)
(require 'clojure-mode)
(require 'go-mode)
(require 'toml-mode)
(require 'dockerfile-mode)
(require 'highlight-numbers)
(require 'highlight-quoted)
(require 'typescript-mode)

(defun zezin-load-theme ()
  (package-install 'doom-themes)
  (require 'doom-themes)
  (load-theme 'doom-one 'no-confirm))

(defun zezin-fix-htmlize-quirks ()
  ;; Fix "Invalid face: tab-line-tab hl-line"
  (require 'tab-line)
  (require 'hl-line)

  ;; Looks like a bug where default face inherit is nil
  (set-face-attribute 'default nil :inherit 'unspecified))

(defun zezin-generate-themefile ()
  (zezin-load-theme)
  (zezin-fix-htmlize-quirks)

  (org-html-htmlize-generate-css))

(defun zezin-generate-ones ()
  (zezin-fix-htmlize-quirks)

  (with-temp-buffer
    (let ((fl (face-list))
          (htmlize-css-name-prefix "org-")
          (htmlize-output-type 'css)
          f i)
      (while (setq f (pop fl)
                   i (and f (face-attribute f :inherit)))
        (when (and (symbolp f) (or (not i) (not (listp i))))
          (insert (org-add-props (copy-sequence "1") nil 'face f))))
      (htmlize-region (point-min) (point-max)))))

(with-eval-after-load 'highlight-quoted
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

(with-eval-after-load 'rainbow-delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(with-eval-after-load 'highlight-numbers
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(zezin-load-theme)

(with-eval-after-load 'org
  (setq org-export-with-sub-superscripts nil)
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-html5-fancy t
        org-html-doctype "html5")
  (load-file "posts-config.el")
  (zezin-set-posts-info "org" "content/posts" "static")

  ;; Rewrites ./res/img.png to /res/img.png
  (defun zezin-rewrite-link (orig-fun &rest args )
    "Replaces org-html-link images with absolue URLs"
    (let ((res (apply orig-fun args)))
      (if (and (stringp res) (string-match-p "<img src=" res))
          (replace-regexp-in-string "./res" "/res" res)
        res)))

  (advice-add #'org-html-link :around #'zezin-rewrite-link))
