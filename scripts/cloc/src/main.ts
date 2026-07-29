import { readFile } from "fs/promises";
import { generateStats } from "./svg";

export type LanguageStat = {
  name: string;
  lines: number;
  color: string | null;
};

type LineInfo = {
  name: string;
  line: number;
};

const loadLineInfo = async (): Promise<LineInfo[]> => {
  const data = await readFile("./lang.json", "utf-8");
  const info: LineInfo[] = JSON.parse(data);

  return info;
};

const loadColorInfo = async (): Promise<Record<string, string>> => {
  const data = await readFile("./colors.json", "utf-8");
  const info: Record<string, string> = JSON.parse(data);

  return info;
};

export const loadLanguageInfo = async (): Promise<
  Record<string, LanguageStat>
> => {
  const lineInfo = await loadLineInfo();
  const colorInfo = await loadColorInfo();

  return lineInfo.reduce(
    (acc, info) => {
      acc[info.name] = {
        name: info.name,
        lines: info.line,
        color: colorInfo[info.name] || null,
      };
      return acc;
    },
    {} as Record<string, LanguageStat>,
  );
};

loadLanguageInfo().then((info) => {
  generateStats(info, "./stats.svg");
});
