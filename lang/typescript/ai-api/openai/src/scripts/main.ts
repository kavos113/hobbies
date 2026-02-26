import OpenAI from "openai";
import { MODEL, PROMPT } from "../constants/settings.js";

const client = new OpenAI({
  apiKey: process.env.OPENAI_API_KEY,
  baseURL: process.env.OPENAI_BASE_URL,
});

const response = await client.responses.create({
  model: MODEL,
  input: PROMPT,
});

console.log(response);
