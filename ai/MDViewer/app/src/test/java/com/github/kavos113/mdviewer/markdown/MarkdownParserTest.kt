package com.github.kavos113.mdviewer.markdown

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

/**
 * MarkdownParser のユニットテスト
 *
 * 様々なマークダウン要素が正しく HTML に変換されることを検証する
 */
class MarkdownParserTest {

    private lateinit var parser: MarkdownParser

    @Before
    fun setUp() {
        parser = MarkdownParser()
    }

    // ========== 見出し ==========

    @Test
    fun `heading level 1`() {
        val result = parser.parse("# 見出し1")
        assertContains(result, "<h1>見出し1</h1>")
    }

    @Test
    fun `heading level 2`() {
        val result = parser.parse("## 見出し2")
        assertContains(result, "<h2>見出し2</h2>")
    }

    @Test
    fun `heading level 3`() {
        val result = parser.parse("### 見出し3")
        assertContains(result, "<h3>見出し3</h3>")
    }

    @Test
    fun `heading levels 4 to 6`() {
        val result4 = parser.parse("#### 見出し4")
        val result5 = parser.parse("##### 見出し5")
        val result6 = parser.parse("###### 見出し6")
        assertContains(result4, "<h4>見出し4</h4>")
        assertContains(result5, "<h5>見出し5</h5>")
        assertContains(result6, "<h6>見出し6</h6>")
    }

    // ========== 段落と改行 ==========

    @Test
    fun `simple paragraph`() {
        val result = parser.parse("これは段落です。")
        assertContains(result, "<p>これは段落です。</p>")
    }

    @Test
    fun `multiple paragraphs`() {
        val markdown = """
            |最初の段落。
            |
            |二番目の段落。
        """.trimMargin()
        val result = parser.parse(markdown)
        assertContains(result, "<p>最初の段落。</p>")
        assertContains(result, "<p>二番目の段落。</p>")
    }

    // ========== インラインスタイル ==========

    @Test
    fun `bold text`() {
        val result = parser.parse("これは**太字**です。")
        assertContains(result, "<strong>太字</strong>")
    }

    @Test
    fun `italic text`() {
        val result = parser.parse("これは*斜体*です。")
        assertContains(result, "<em>斜体</em>")
    }

    @Test
    fun `bold and italic combined`() {
        val result = parser.parse("***太字斜体***")
        assertContains(result, "<em><strong>太字斜体</strong></em>")
    }

    @Test
    fun `inline code`() {
        val result = parser.parse("これは`code`です。")
        assertContains(result, "<code>code</code>")
    }

    @Test
    fun `strikethrough text`() {
        val result = parser.parse("これは~~取り消し線~~です。")
        assertContains(result, "<del>取り消し線</del>")
    }

    // ========== リンクと画像 ==========

    @Test
    fun `link`() {
        val result = parser.parse("[Google](https://www.google.com)")
        assertContains(result, "<a href=\"https://www.google.com\">Google</a>")
    }

    @Test
    fun `image`() {
        val result = parser.parse("![代替テキスト](https://example.com/image.png)")
        assertContains(result, "<img src=\"https://example.com/image.png\" alt=\"代替テキスト\"")
    }

    @Test
    fun `autolink`() {
        val result = parser.parse("https://www.google.com を参照")
        assertContains(result, "<a href=\"https://www.google.com\">https://www.google.com</a>")
    }

    // ========== リスト ==========

    @Test
    fun `unordered list`() {
        val markdown = """
            |- 項目1
            |- 項目2
            |- 項目3
        """.trimMargin()
        val result = parser.parse(markdown)
        assertContains(result, "<ul>")
        assertContains(result, "<li>項目1</li>")
        assertContains(result, "<li>項目2</li>")
        assertContains(result, "<li>項目3</li>")
        assertContains(result, "</ul>")
    }

    @Test
    fun `ordered list`() {
        val markdown = """
            |1. 項目1
            |2. 項目2
            |3. 項目3
        """.trimMargin()
        val result = parser.parse(markdown)
        assertContains(result, "<ol>")
        assertContains(result, "<li>項目1</li>")
        assertContains(result, "<li>項目2</li>")
        assertContains(result, "<li>項目3</li>")
        assertContains(result, "</ol>")
    }

    @Test
    fun `nested list`() {
        val markdown = """
            |- 親項目
            |  - 子項目1
            |  - 子項目2
        """.trimMargin()
        val result = parser.parse(markdown)
        assertContains(result, "<ul>")
        assertContains(result, "<li>親項目")
        assertContains(result, "<li>子項目1</li>")
        assertContains(result, "<li>子項目2</li>")
    }

    @Test
    fun `task list`() {
        val markdown = """
            |- [x] 完了タスク
            |- [ ] 未完了タスク
        """.trimMargin()
        val result = parser.parse(markdown)
        assertContains(result, "type=\"checkbox\"")
        assertContains(result, "checked")
        assertContains(result, "完了タスク")
        assertContains(result, "未完了タスク")
    }

    // ========== コードブロック ==========

    @Test
    fun `fenced code block`() {
        val markdown = """
            |```kotlin
            |fun main() {
            |    println("Hello")
            |}
            |```
        """.trimMargin()
        val result = parser.parse(markdown)
        assertContains(result, "<pre>")
        assertContains(result, "<code")
        assertContains(result, "fun main()")
        assertContains(result, "println")
    }

    @Test
    fun `indented code block`() {
        val markdown = "    val x = 42\n    println(x)"
        val result = parser.parse(markdown)
        assertContains(result, "<pre>")
        assertContains(result, "<code>")
    }

    // ========== 引用 ==========

    @Test
    fun `blockquote`() {
        val result = parser.parse("> これは引用です。")
        assertContains(result, "<blockquote>")
        assertContains(result, "これは引用です。")
        assertContains(result, "</blockquote>")
    }

    @Test
    fun `nested blockquote`() {
        val markdown = """
            |> 外側の引用
            |> > 内側の引用
        """.trimMargin()
        val result = parser.parse(markdown)
        assertTrue(
            "ネストされた blockquote が含まれること",
            result.contains("<blockquote>") && result.contains("外側の引用") && result.contains("内側の引用")
        )
    }

    // ========== 水平線 ==========

    @Test
    fun `horizontal rule`() {
        val result = parser.parse("---")
        assertContains(result, "<hr")
    }

    // ========== テーブル ==========

    @Test
    fun `simple table`() {
        val markdown = """
            || 名前 | 年齢 |
            || --- | --- |
            || 太郎 | 25 |
            || 花子 | 30 |
        """.trimMargin()
        val result = parser.parse(markdown)
        assertContains(result, "<table>")
        assertContains(result, "<th>名前</th>")
        assertContains(result, "<th>年齢</th>")
        assertContains(result, "<td>太郎</td>")
        assertContains(result, "<td>25</td>")
        assertContains(result, "<td>花子</td>")
        assertContains(result, "<td>30</td>")
        assertContains(result, "</table>")
    }

    @Test
    fun `table with alignment`() {
        val markdown = """
            || Left | Center | Right |
            || :--- | :---: | ---: |
            || A | B | C |
        """.trimMargin()
        val result = parser.parse(markdown)
        assertContains(result, "<table>")
        assertContains(result, "align=\"left\"")
        assertContains(result, "align=\"center\"")
        assertContains(result, "align=\"right\"")
    }

    // ========== 複合テスト ==========

    @Test
    fun `complex markdown document`() {
        val markdown = """
            |# プロジェクト README
            |
            |## 概要
            |
            |これは**サンプルプロジェクト**です。
            |詳細は[公式ドキュメント](https://example.com)を参照してください。
            |
            |## 機能一覧
            |
            |- マークダウンの表示
            |- ~~旧機能~~ 新機能
            |- `コード`のハイライト
            |
            |## インストール
            |
            |```bash
            |npm install sample-project
            |```
            |
            |> **注意**: このプロジェクトは開発中です。
            |
            || 機能 | 状態 |
            || --- | --- |
            || 基本機能 | 完了 |
            || 拡張機能 | 開発中 |
        """.trimMargin()
        val result = parser.parse(markdown)

        // 見出しの確認
        assertContains(result, "<h1>プロジェクト README</h1>")
        assertContains(result, "<h2>概要</h2>")
        assertContains(result, "<h2>機能一覧</h2>")
        assertContains(result, "<h2>インストール</h2>")

        // インラインスタイルの確認
        assertContains(result, "<strong>サンプルプロジェクト</strong>")
        assertContains(result, "<del>旧機能</del>")
        assertContains(result, "<code>コード</code>")

        // リンクの確認
        assertContains(result, "<a href=\"https://example.com\">公式ドキュメント</a>")

        // リストの確認
        assertContains(result, "<ul>")
        assertContains(result, "<li>マークダウンの表示</li>")

        // コードブロックの確認
        assertContains(result, "<pre>")
        assertContains(result, "npm install sample-project")

        // 引用の確認
        assertContains(result, "<blockquote>")

        // テーブルの確認
        assertContains(result, "<table>")
        assertContains(result, "<th>機能</th>")
        assertContains(result, "<td>完了</td>")
    }

    // ========== parseToFullHtml テスト ==========

    @Test
    fun `parseToFullHtml contains html structure`() {
        val result = parser.parseToFullHtml("# テスト", "テストページ")
        assertContains(result, "<!DOCTYPE html>")
        assertContains(result, "<html lang=\"ja\">")
        assertContains(result, "<title>テストページ</title>")
        assertContains(result, "<h1>テスト</h1>")
        assertContains(result, "</html>")
    }

    @Test
    fun `parseToFullHtml contains css styles`() {
        val result = parser.parseToFullHtml("テスト")
        assertContains(result, "<style>")
        assertContains(result, "font-family")
        assertContains(result, "markdown-body")
    }

    // ========== エッジケース ==========

    @Test
    fun `empty string`() {
        val result = parser.parse("")
        assertEquals("", result)
    }

    @Test
    fun `plain text without markdown`() {
        val result = parser.parse("ただのテキスト")
        assertContains(result, "<p>ただのテキスト</p>")
    }

    @Test
    fun `html tags are handled`() {
        val result = parser.parse("テスト<b>太字</b>テスト")
        // flexmark はデフォルトで生の HTML をパススルーする
        assertContains(result, "テスト")
    }

    // ========== ヘルパー ==========

    private fun assertContains(html: String, expected: String) {
        assertTrue(
            "HTML に '$expected' が含まれること\n実際の出力: $html",
            html.contains(expected)
        )
    }
}
