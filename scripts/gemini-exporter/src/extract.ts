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
      userQuery: String(userQuery ? userQuery[0].innerHTML : ''),
      modelResponse: String(modelResponse ? modelResponse[0].innerHTML : '')
    }
  })
}

export const filterResponses = (responses: Response[]): Response[] => {
  return responses.map((res) => {
    res.userQuery = res.userQuery
      .trim()
      .replaceAll(/\n[\t\r\f ]*<!---->[\t\r\f ]*\n/g, '')
      .replaceAll("<!---->", '')
      .replaceAll(/\n[ ]*([^\n]+)\n\n[ ]*```/g, "```$1")
    res.modelResponse = res.modelResponse
      .trim()
      .replaceAll(/\n[\t\r\f ]*<!---->[\t\r\f ]*\n/g, '')
      .replaceAll("<!---->", '')
      .replaceAll(/\n[ ]*([^\n]+)\n\n[ ]*```/g, "```$1")
    return res
  })
}

export const buildResponses = (responses: Response[]): string => {
  return responses
    .map((res) => `## User Query\n\n${res.userQuery}\n\n## Model Response\n\n${res.modelResponse}`)
    .join('\n\n---\n\n')
}
