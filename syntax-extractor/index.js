const { PurgeCSS } = require("purgecss");
const { resolve } = require('path')
const fs = require('fs')

const postsPath  = resolve(__dirname, '../content/posts');
const completeSyntaxFile  = resolve(__dirname, 'base16-default-oceanic.css');
const syntaxFinalFile = resolve(__dirname, '../assets/scss/_syntax.scss');

const purge = async () => {
  const result = await new PurgeCSS().purge({
    content: [`${postsPath}/**/*.html`],
    css: [completeSyntaxFile],
    blocklist: ["a"]
  });

  const finalCss = result.map(r => r.css).join();
  fs.writeFileSync(syntaxFinalFile, finalCss);
}

(async () => {
  await purge()
})()
