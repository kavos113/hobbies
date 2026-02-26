package com.github.kavos113.mdviewer

import android.content.Intent
import android.net.Uri
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.material3.Button
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.material3.TopAppBar
import androidx.compose.material3.TopAppBarDefaults
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import com.github.kavos113.mdviewer.ui.theme.MDViewerTheme

/**
 * マークダウンファイルの選択画面
 */
class MainActivity : ComponentActivity() {

  private val openDocumentLauncher = registerForActivityResult(
    ActivityResultContracts.OpenDocument()
  ) { uri: Uri? ->
    uri?.let { openMarkdownViewer(it) }
  }

  @OptIn(ExperimentalMaterial3Api::class)
  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    enableEdgeToEdge()
    setContent {
      MDViewerTheme {
        Scaffold(
          modifier = Modifier.fillMaxSize(),
          topBar = {
            TopAppBar(
              title = { Text(text = "MD Viewer") },
              colors = TopAppBarDefaults.topAppBarColors(
                containerColor = MaterialTheme.colorScheme.primaryContainer,
                titleContentColor = MaterialTheme.colorScheme.onPrimaryContainer
              )
            )
          }
        ) { innerPadding ->
          FileSelectScreen(
            onSelectFile = { selectMarkdownFile() },
            modifier = Modifier.padding(innerPadding)
          )
        }
      }
    }
  }

  private fun selectMarkdownFile() {
    openDocumentLauncher.launch(arrayOf("text/markdown", "text/plain", "text/x-markdown", "*/*"))
  }

  private fun openMarkdownViewer(uri: Uri) {
    val intent = Intent(this, MarkdownActivity::class.java).apply {
      data = uri
      addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION)
    }
    startActivity(intent)
  }
}

/**
 * ファイル選択画面の Composable
 */
@Composable
fun FileSelectScreen(onSelectFile: () -> Unit, modifier: Modifier = Modifier) {
  Column(
    modifier = modifier.fillMaxSize(),
    verticalArrangement = Arrangement.Center,
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    Icon(
      painter = painterResource(id = android.R.drawable.ic_menu_edit),
      contentDescription = null,
      modifier = Modifier.size(64.dp),
      tint = MaterialTheme.colorScheme.primary
    )
    Text(
      text = "マークダウンファイルを選択",
      style = MaterialTheme.typography.headlineSmall,
      modifier = Modifier.padding(top = 16.dp, bottom = 24.dp)
    )
    Button(onClick = onSelectFile) {
      Text(text = "ファイルを開く")
    }
  }
}

@Preview(showBackground = true)
@Composable
fun FileSelectScreenPreview() {
  MDViewerTheme {
    FileSelectScreen(onSelectFile = {})
  }
}