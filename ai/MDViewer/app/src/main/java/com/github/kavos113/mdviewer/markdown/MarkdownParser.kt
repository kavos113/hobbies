package com.github.kavos113.mdviewer.markdown

import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.gfm.strikethrough.StrikethroughExtension
import com.vladsch.flexmark.ext.gfm.tasklist.TaskListExtension
import com.vladsch.flexmark.ext.tables.TablesExtension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.data.MutableDataSet

/**
 * flexmark-java を使用してマークダウンテキストを HTML に変換するパーサー
 */
class MarkdownParser {
  private val options = MutableDataSet().apply {
    set(
      Parser.EXTENSIONS,
      listOf(
        TablesExtension.create(),
        StrikethroughExtension.create(),
        TaskListExtension.create(),
        AutolinkExtension.create()
      )
    )
    set(HtmlRenderer.SOFT_BREAK, "<br />\n")
  }

  private val parser: Parser = Parser.builder(options).build()
  private val renderer: HtmlRenderer = HtmlRenderer.builder(options).build()

  /**
   * マークダウンテキストを HTML 文字列に変換する
   *
   * @param markdown マークダウン形式のテキスト
   * @return HTML 文字列（body の中身のみ）
   */
  fun parse(markdown: String): String {
    val document = parser.parse(markdown)
    return renderer.render(document)
  }

  /**
   * マークダウンテキストをスタイル付きの完全な HTML ページに変換する
   *
   * @param markdown マークダウン形式のテキスト
   * @param title ページタイトル
   * @return 完全な HTML ドキュメント文字列
   */
  fun parseToFullHtml(markdown: String, title: String = ""): String {
    val bodyHtml = parse(markdown)
    return buildFullHtml(bodyHtml, title)
  }

  companion object {
    /**
     * HTML body コンテンツをスタイル付きの完全な HTML ドキュメントでラップする
     */
    fun buildFullHtml(bodyHtml: String, title: String = ""): String {
      return """
                <!DOCTYPE html>
                <html lang="ja">
                <head>
                    <meta charset="UTF-8">
                    <meta name="viewport" content="width=device-width, initial-scale=1.0">
                    <title>${title}</title>
                    <style>
                        ${CSS_STYLE}
                    </style>
                </head>
                <body>
                    <article class="markdown-body">
                        ${bodyHtml}
                    </article>
                </body>
                </html>
            """.trimIndent()
    }

    private val CSS_STYLE = """
            * {
                box-sizing: border-box;
            }
            body {
                font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
                font-size: 16px;
                line-height: 1.6;
                color: #24292e;
                background-color: #ffffff;
                margin: 0;
                padding: 16px;
                word-wrap: break-word;
            }
            .markdown-body {
                max-width: 100%;
            }
            h1, h2, h3, h4, h5, h6 {
                margin-top: 24px;
                margin-bottom: 16px;
                font-weight: 600;
                line-height: 1.25;
            }
            h1 {
                font-size: 2em;
                padding-bottom: 0.3em;
                border-bottom: 1px solid #eaecef;
            }
            h2 {
                font-size: 1.5em;
                padding-bottom: 0.3em;
                border-bottom: 1px solid #eaecef;
            }
            h3 { font-size: 1.25em; }
            h4 { font-size: 1em; }
            h5 { font-size: 0.875em; }
            h6 { font-size: 0.85em; color: #6a737d; }
            p {
                margin-top: 0;
                margin-bottom: 16px;
            }
            a {
                color: #0366d6;
                text-decoration: none;
            }
            a:hover {
                text-decoration: underline;
            }
            strong {
                font-weight: 600;
            }
            code {
                padding: 0.2em 0.4em;
                margin: 0;
                font-size: 85%;
                background-color: rgba(27,31,35,0.05);
                border-radius: 3px;
                font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
            }
            pre {
                padding: 16px;
                overflow: auto;
                font-size: 85%;
                line-height: 1.45;
                background-color: #f6f8fa;
                border-radius: 6px;
                margin-top: 0;
                margin-bottom: 16px;
            }
            pre code {
                padding: 0;
                margin: 0;
                font-size: 100%;
                background-color: transparent;
                border-radius: 0;
            }
            blockquote {
                padding: 0 1em;
                color: #6a737d;
                border-left: 0.25em solid #dfe2e5;
                margin: 0 0 16px 0;
            }
            ul, ol {
                padding-left: 2em;
                margin-top: 0;
                margin-bottom: 16px;
            }
            li {
                margin-top: 0.25em;
            }
            li + li {
                margin-top: 0.25em;
            }
            hr {
                height: 0.25em;
                padding: 0;
                margin: 24px 0;
                background-color: #e1e4e8;
                border: 0;
            }
            table {
                border-spacing: 0;
                border-collapse: collapse;
                margin-top: 0;
                margin-bottom: 16px;
                width: 100%;
                overflow: auto;
            }
            table th, table td {
                padding: 6px 13px;
                border: 1px solid #dfe2e5;
            }
            table th {
                font-weight: 600;
                background-color: #f6f8fa;
            }
            table tr {
                background-color: #ffffff;
                border-top: 1px solid #c6cbd1;
            }
            table tr:nth-child(2n) {
                background-color: #f6f8fa;
            }
            img {
                max-width: 100%;
                height: auto;
            }
            .task-list-item {
                list-style-type: none;
                margin-left: -1.5em;
            }
            .task-list-item input[type="checkbox"] {
                margin-right: 0.5em;
            }
            del {
                text-decoration: line-through;
            }
            @media (prefers-color-scheme: dark) {
                body {
                    color: #c9d1d9;
                    background-color: #0d1117;
                }
                h1, h2 {
                    border-bottom-color: #30363d;
                }
                h6 { color: #8b949e; }
                a { color: #58a6ff; }
                code {
                    background-color: rgba(110,118,129,0.4);
                }
                pre {
                    background-color: #161b22;
                }
                blockquote {
                    color: #8b949e;
                    border-left-color: #3b434b;
                }
                hr {
                    background-color: #30363d;
                }
                table th, table td {
                    border-color: #30363d;
                }
                table th {
                    background-color: #161b22;
                }
                table tr {
                    background-color: #0d1117;
                    border-top-color: #30363d;
                }
                table tr:nth-child(2n) {
                    background-color: #161b22;
                }
            }
        """.trimIndent()
  }
}
