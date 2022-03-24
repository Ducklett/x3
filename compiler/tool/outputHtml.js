const fs = require('fs')
const { format } = require('./format')

function markHtml(kind, text) {
    return `<span class="${kind}">${text}</span>`
}

module.exports = {
    outputHtml(syntaxTree) {
        const src = format(syntaxTree, markHtml)
        const lines = src.split('\n')
        const renderedSource = lines
            .map((line, i) => `<span class="line-nr">${(i + 1).toString().padStart(Math.ceil(lines.length.toString().length / 4) * 4, ' ')}</span> ${line}`)
            .join('\n')
        const html = `<head></head>
<body>
<style>
    body {
        margin:0;
        min-height: 100vh;
        display:grid;
        place-content:center;
    }
    .code {
        font-size:14px;
        padding:12px;
        border-radius: 8px;
        background: #282C34;
        tab-size:4;
        display:inline-block;
        min-width: 600px;
        box-shadow: 3px 3px 4px 0px rgb(0 0 0 / 20%);
    }
    .keyword { color: #C678DD; }
    .symbol { color: #D46C75; }
    .symbol-const { color: #E5C07B; }
    .symbol-code { color: #61AFEF; }
    .operator { color: #56B6C2; }
    .pseudo-operator { color: #ABB2BF; }
    .comment { color: #7F848E; }
    .number { color: #D19A66; }
    .string { color: #98C379; }
    .tag { color: #E5C07B; }
    .line-nr { color: #495162; }
</style>
<div class="code"><pre><code>${renderedSource}</code></pre></div>
</body>`
        fs.writeFileSync('out.html', html)
    }
}
