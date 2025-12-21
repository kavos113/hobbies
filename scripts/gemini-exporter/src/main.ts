import { extractResponses, buildResponses, filterResponses } from './extract'
import { unified } from 'unified'
import rehypeParse from 'rehype-parse'
import rehypeRemark from 'rehype-remark'
import remarkStringify from 'remark-stringify'
import './style.css' // 必要ならスタイル読み込み

const converter = unified().use(rehypeParse).use(rehypeRemark).use(remarkStringify)

// DOM要素の取得と型アサーション
const extractBtn = document.getElementById('extractBtn') as HTMLButtonElement

extractBtn.addEventListener('click', async () => {
  const [tab] = await chrome.tabs.query({ active: true, currentWindow: true })
  if (!tab.id) return

  const results = await chrome.scripting.executeScript({
    target: { tabId: tab.id },
    func: extractResponses,
    args: []
  })
  if (!results || results.length === 0) return

  const rawData = results[0].result
  if (!rawData || rawData.length === 0) return

  const processedResults = await Promise.all(
    rawData.map(async (data) => {
      const userQueryMd = await converter.process(data.userQuery)
      const modelResponseMd = await converter.process(data.modelResponse)
      return {
        userQuery: String(userQueryMd),
        modelResponse: String(modelResponseMd)
      }
    })
  )

  const blob = new Blob([buildResponses(filterResponses(processedResults))], {
    type: 'text/markdown'
  })
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a')
  a.href = url
  a.download = 'data.md'
  a.click()
  URL.revokeObjectURL(url)
})
