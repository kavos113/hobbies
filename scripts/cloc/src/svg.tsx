import satori from "satori";
import { LanguageStat } from "./main";
import fs from "fs/promises";

type Lang = {
  name: string;
  lines: number;
  percentage: string;
  color: string | null;
};

const LangInfo = ({ lang }: { lang: Lang }) => {
  const lineString = lang.lines.toLocaleString();
  const parcentString = `${lang.percentage.toString()}%`;

  return (
    <div
      style={{
        display: "flex",
        alignItems: "center",
      }}
    >
      <div
        style={{
          width: "10px",
          height: "10px",
          borderRadius: "50%",
          backgroundColor: lang.color || "#000000",
          marginRight: "8px",
        }}
      />
      <div
        style={{
          fontWeight: 600,
          marginRight: "6px",
          fontSize: "14px",
          width: "120px",
        }}
      >
        {lang.name}
      </div>
      <div
        style={{
          marginRight: "6px",
          fontSize: "14px",
          width: "60px",
        }}
      >
        {lineString}
      </div>
      <div
        style={{
          color: "#8b949e",
          fontSize: "14px",
          width: "40px",
        }}
      >
        {parcentString}
      </div>
    </div>
  );
};

const LangStats = ({
  stats,
  width = 600,
}: {
  stats: Lang[];
  width?: number;
}) => {
  const half = stats.length / 2;
  const first = stats.slice(0, half);
  const second = stats.slice(half);

  return (
    <div
      style={{
        display: "flex",
        flexDirection: "column",
        width: `${width}px`,
        padding: "20px",
        border: "1px solid #e1e4e8",
        borderRadius: "6px",
        backgroundColor: "#fefefe",
      }}
    >
      <h3
        style={{
          margin: "0 0 16px 0",
          fontSize: "20px",
          fontWeight: 600,
        }}
      >
        Language Stats
      </h3>

      <div
        style={{
          display: "flex",
          width: "100%",
          height: "12px",
          borderRadius: "6px",
          overflow: "hidden",
          backgroundColor: "#f5f5f5",
        }}
      >
        {stats.slice(0, 12).map((lang, index) => {
          const borderRight =
            index < stats.length - 1 ? "1px solid #ffffff" : "none";

          return (
            <div
              key={lang.name}
              style={{
                flex: lang.percentage,
                backgroundColor: lang.color || "#000000",
                borderRight: borderRight,
                boxSizing: "border-box",
              }}
            />
          );
        })}
      </div>

      <div
        style={{
          display: "flex",
          flexDirection: "row",
          marginTop: "16px",
          fontSize: "14px",
        }}
      >
        <div
          style={{
            display: "flex",
            flexDirection: "column",
            flex: 1,
          }}
        >
          {first.map((lang) => (
            <LangInfo key={lang.name} lang={lang} />
          ))}
        </div>
        <div
          style={{
            display: "flex",
            flexDirection: "column",
            flex: 1,
          }}
        >
          {second.map((lang) => (
            <LangInfo key={lang.name} lang={lang} />
          ))}
        </div>
      </div>
    </div>
  );
};

export const generateStats = async (
  stats: Record<string, LanguageStat>,
  outPath: string,
): Promise<void> => {
  const totalLines = Object.values(stats).reduce(
    (acc, stat) => acc + stat.lines,
    0,
  );

  const printStats = Object.values(stats)
    .sort((a, b) => b.lines - a.lines)
    .map((stat) => {
      const percentage =
        (stat.lines / totalLines) * 100 >= 10
          ? ((stat.lines / totalLines) * 100).toFixed(1)
          : `${((stat.lines / totalLines) * 100).toFixed(2)}`;
      return {
        name: stat.name,
        lines: stat.lines,
        percentage: percentage,
        color: stat.color,
      };
    });

  const svg = await satori(LangStats({ stats: printStats, width: 600 }), {
    width: 600,
    fonts: [
      {
        name: "Roboto",
        data: await fs.readFile("./fonts/Roboto-Regular.ttf"),
        weight: 400,
        style: "normal",
      },
      {
        name: "Roboto",
        data: await fs.readFile("./fonts/Roboto-Bold.ttf"),
        weight: 600,
        style: "normal",
      },
    ],
  });

  await fs.writeFile(outPath, svg);
};
