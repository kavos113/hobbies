import { unified } from 'unified'
import rehypeParse from 'rehype-parse'
import rehypeRemark from 'rehype-remark'
import remarkStringify from 'remark-stringify'

const converter = unified().use(rehypeParse).use(rehypeRemark).use(remarkStringify)

export interface Response {
  userQuery: string
  modelResponse: string
}

export const extractContent = (selector: string): string | null => {
  const element = document.querySelector(selector)
  return element ? element.innerHTML : null
}

export const extractResponses = async (): Promise<Response[]> => {
  const conversations = document.getElementsByClassName('conversation-container')
  const promises = Array.from(conversations).map(async (conv) => {
    const userQuery = conv.getElementsByTagName('user-query')
    const modelResponse = conv.getElementsByTagName('model-response')

    const userQueryMd = await converter.process(userQuery.length > 0 ? userQuery[0].innerHTML : '')
    const modelResponseMd = await converter.process(
      modelResponse.length > 0 ? modelResponse[0].innerHTML : ''
    )

    return {
      userQuery: String(userQueryMd),
      modelResponse: String(modelResponseMd)
    }
  })
  return Promise.all(promises)
}
