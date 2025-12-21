import { extractResponses } from './extract'
import './style.css' // 必要ならスタイル読み込み

// DOM要素の取得と型アサーション
const extractBtn = document.getElementById('extractBtn') as HTMLButtonElement

extractBtn.addEventListener('click', async () => {
  const [tab] = await chrome.tabs.query({ active: true, currentWindow: true })
  if (!tab.id) return

  chrome.scripting.executeScript(
    {
      target: { tabId: tab.id },
      func: extractResponses,
      args: []
    },
    (results) => {
      const blob = new Blob([JSON.stringify(results)], { type: 'application/json' })
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = 'data.json'
      a.click()
      URL.revokeObjectURL(url)
    }
  )
})
