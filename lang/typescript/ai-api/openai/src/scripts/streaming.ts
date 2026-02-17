import OpenAI from "openai";
import { MODEL, PROMPT } from "../constants/settings.js";

const client = new OpenAI({
  apiKey: process.env.OPENAI_API_KEY,
  baseURL: process.env.OPENAI_BASE_URL,
});

const stream = await client.responses.create({
  model: MODEL,
  input: PROMPT,
  stream: true,
});

for await (const message of stream) {
  if (message.type === "response.output_text.delta") {
    process.stdout.write(message.delta);
  }
}
