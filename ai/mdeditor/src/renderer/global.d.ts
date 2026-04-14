export {};

declare global {
  interface Window {
    api: {
      openFile: () => Promise<{
        canceled: boolean;
        filePath: string | null;
        content: string;
      }>;
      saveFile: (filePath: string, content: string) => Promise<{
        canceled: boolean;
        filePath: string | null;
      }>;
      saveFileAs: (content: string) => Promise<{
        canceled: boolean;
        filePath: string | null;
      }>;
    };
  }
}
