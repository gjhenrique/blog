# (blog 'zezin)

House of [gjhenrique.com](https://gjhenrique.com)

## From org-mode to HTML
It uses the [built-in publishing management system](https://orgmode.org/manual/Publishing.html) from org-mode. [org-publish-project-alist](https://github.com/gjhenrique/blog/blob/main/posts-config.el#L2) defines where org-mode should look into to convert org and resource files and the respective folders.

Calling `(org-publish-all)` converts the posts from org to HTML and moves the resources, like images and custom JS.
This project includes a `.dir-locals.el` file for local development that sets this variable automatically.

## Hugo
This blog uses hugo with [listed theme](https://github.com/ronv/listed) to generate the static site.
It's blazing fast.

## CI

The steps using Github Actions are as follows:

1. Export the posts from an emacs container containing org-mode, org-babel and packages that define the colors from code
1. Calls hugo to create the static site
1. Publish the files to Cloudflare Pages With the wrangler tool. `main` branch deploys to production env automatically, and all other branches to preview. Cloudflare Access restricts access to the preview environment.

## Syntax coloring

Creating the CSS file to code snippets follows the process:

1. Export the theme file. Use `emacs -q -l /app/export.el -f zezin-generate-themefile` to get the file with the CSS rules. [htmlize](https://github.com/hniksic/emacs-htmlize) creates the [doom-one theme CSS file](https://github.com/gjhenrique/blog/blob/main/syntax-extractor/doom-one.css).
1. Trim only the posts' CSS rules with [purgecss](https://purgecss.com/). Call `node syntax-extractor`
1. Override the ones you don't like via _custom.scss
1. These CSS rules will style the generated `#+BEGIN_SRC` org section.

It works with any [Emacs theme](https://emacsthemes.com/popular/index.html) exportable via htmlize.

## Posts
- [Initial setup on Jekyll and Gitlab Pages](https://gjhenrique.com/meta.html)
