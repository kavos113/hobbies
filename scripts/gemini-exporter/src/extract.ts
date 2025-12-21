export interface Response {
  userQuery: string
  modelResponse: string
}

export const extractContent = (selector: string): string | null => {
  const element = document.querySelector(selector)
  return element ? element.innerHTML : null
}

export const extractResponses = (): Response[] => {
  const conversations = document.getElementsByClassName('conversation-container')
  return Array.from(conversations).map((conv) => {
    const userQuery = conv.getElementsByTagName('user-query')
    const modelResponse = conv.getElementsByTagName('model-response')

    return {
      userQuery: userQuery.length > 0 ? userQuery[0].innerHTML : '',
      modelResponse: modelResponse.length > 0 ? modelResponse[0].innerHTML : ''
    }
  })
}
