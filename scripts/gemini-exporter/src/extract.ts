export interface Response {
  userQuery: string
  modelResponse: string
}

export const extractContent = (selector: string): string | null => {
  const element = document.querySelector(selector)
  return element ? element.innerHTML : null
}

export const extractResponses = (html: string): Response[] => {
  const conversations = document.getElementsByClassName('conversation-container')
  return conversations.map((conv) => {
    const userQuery = conv.getElementsByTagName('user-query')
    const modelResponse = conv.getElementsByTagName('model-response')

    return {
      userQuery,
      modelResponse
    }
  })
}
