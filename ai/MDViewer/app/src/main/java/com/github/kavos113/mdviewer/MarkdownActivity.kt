package com.github.kavos113.mdviewer

import android.net.Uri
import android.os.Bundle
import android.webkit.WebView
import android.webkit.WebViewClient
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.material3.TopAppBar
import androidx.compose.material3.TopAppBarDefaults
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.viewinterop.AndroidView
import com.github.kavos113.mdviewer.markdown.MarkdownParser
import com.github.kavos113.mdviewer.ui.theme.MDViewerTheme
import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * マークダウンファイルの内容を表示する Activity
 *
 * Intent で受け取った URI からマークダウンファイルを読み込み、
 * HTML に変換して WebView で表示する。
 */
class MarkdownActivity : ComponentActivity() {
  @OptIn(ExperimentalMaterial3Api::class)
  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    enableEdgeToEdge()

    val uri: Uri? = intent.data
    val markdownText = uri?.let { readTextFromUri(it) } ?: ""
    val fileName = uri?.lastPathSegment ?: "Markdown"

    val parser = MarkdownParser()
    val html = parser.parseToFullHtml(markdownText, fileName)

    setContent {
      MDViewerTheme {
        Scaffold(
          modifier = Modifier.fillMaxSize(),
          topBar = {
            TopAppBar(
              title = { Text(text = fileName) },
              navigationIcon = {
                IconButton(onClick = { finish() }) {
                  Icon(
                    painter = painterResource(id = android.R.drawable.ic_menu_close_clear_cancel),
                    contentDescription = "閉じる"
                  )
                }
              },
              colors = TopAppBarDefaults.topAppBarColors(
                containerColor = MaterialTheme.colorScheme.primaryContainer,
                titleContentColor = MaterialTheme.colorScheme.onPrimaryContainer
              )
            )
          }
        ) { innerPadding ->
          MarkdownWebView(
            html = html,
            modifier = Modifier
              .fillMaxSize()
              .padding(innerPadding)
          )
        }
      }
    }
  }

  /**
   * URI からテキストを読み込む
   */
  private fun readTextFromUri(uri: Uri): String {
    return try {
      contentResolver.openInputStream(uri)?.use { inputStream ->
        BufferedReader(InputStreamReader(inputStream)).use { reader ->
          reader.readText()
        }
      } ?: ""
    } catch (e: Exception) {
      ""
    }
  }
}

/**
 * WebView を使ってHTML コンテンツを表示する Composable
 */
@Composable
fun MarkdownWebView(html: String, modifier: Modifier = Modifier) {
  AndroidView(
    factory = { context ->
      WebView(context).apply {
        webViewClient = WebViewClient()
        settings.apply {
          @Suppress("SetJavaScriptEnabled")
          javaScriptEnabled = false
          loadWithOverviewMode = true
          useWideViewPort = true
          builtInZoomControls = true
          displayZoomControls = false
        }
        loadDataWithBaseURL(null, html, "text/html", "UTF-8", null)
      }
    },
    update = { webView ->
      webView.loadDataWithBaseURL(null, html, "text/html", "UTF-8", null)
    },
    modifier = modifier
  )
}
