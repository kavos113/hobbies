import './style.css' // 必要ならスタイル読み込み

// DOM要素の取得と型アサーション
const selectorInput = document.getElementById('selector') as HTMLInputElement
const extractBtn = document.getElementById('extractBtn') as HTMLButtonElement
const resultArea = document.getElementById('result') as HTMLTextAreaElement

extractBtn.addEventListener('click', async () => {
  const selector = selectorInput.value
  if (!selector) {
    resultArea.value = 'セレクタを入力してください'
    return
  }

  // 現在のアクティブなタブを取得
  const [tab] = await chrome.tabs.query({ active: true, currentWindow: true })
  if (!tab.id) return

  // ページ内でスクリプトを実行
  chrome.scripting.executeScript(
    {
      target: { tabId: tab.id },
      func: getPageContent,
      args: [selector]
    },
    (results) => {
      if (chrome.runtime.lastError) {
        resultArea.value = 'エラー: ' + chrome.runtime.lastError.message
        return
      }

      const htmlContent = results[0]?.result

      if (htmlContent) {
        // ★将来ここにMarkdown変換処理を入れる場所★
        // import TurndownService from 'turndown';
        // const service = new TurndownService();
        // const markdown = service.turndown(htmlContent);

        resultArea.value = htmlContent
      } else {
        resultArea.value = '要素が見つかりませんでした'
      }
    }
  )
})

// ブラウザのページ側で実行される関数
// DOM要素からHTMLを取得して返します
function getPageContent(selector: string): string | null {
  const element = document.querySelector(selector)
  return element ? element.innerHTML : null
}
