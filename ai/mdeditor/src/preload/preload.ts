import { contextBridge, ipcRenderer } from 'electron';

type OpenResult = {
  canceled: boolean;
  filePath: string | null;
  content: string;
};

type SaveResult = {
  canceled: boolean;
  filePath: string | null;
};

export type { OpenResult, SaveResult };

contextBridge.exposeInMainWorld('api', {
  openFile: () => ipcRenderer.invoke('file-open') as Promise<OpenResult>,
  saveFile: (filePath: string, content: string) =>
    ipcRenderer.invoke('file-save', filePath, content) as Promise<SaveResult>,
  saveFileAs: (content: string) => ipcRenderer.invoke('file-save-as', content) as Promise<SaveResult>,
});
