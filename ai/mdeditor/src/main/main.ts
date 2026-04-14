import path from 'node:path';
import { app, BrowserWindow, dialog, ipcMain } from 'electron';
import { promises as fs, existsSync } from 'node:fs';

type OpenResult = {
  canceled: boolean;
  filePath: string | null;
  content: string;
};

type SaveResult = {
  canceled: boolean;
  filePath: string | null;
};

const isDev = !app.isPackaged;
const markdownFilters = [
  { name: 'Markdown', extensions: ['md', 'markdown', 'txt'] },
  { name: 'All Files', extensions: ['*'] },
];

let mainWindow: BrowserWindow | null = null;

const getPreloadPath = (): string => {
  if (isDev) {
    return path.join(__dirname, '..', 'preload', 'preload.js');
  }
  const appPreloadPath = path.join(app.getAppPath(), 'out', 'preload', 'preload.js');
  if (existsSync(appPreloadPath)) {
    return appPreloadPath;
  }
  return path.join(process.resourcesPath, 'app.asar', 'out', 'preload', 'preload.js');
};

const createWindow = async () => {
  mainWindow = new BrowserWindow({
    width: 1200,
    height: 800,
    backgroundColor: '#0f172a',
    webPreferences: {
      preload: getPreloadPath(),
      contextIsolation: true,
      nodeIntegration: false,
    },
  });

  if (isDev) {
    await mainWindow.loadURL('http://localhost:5173');
    mainWindow.webContents.openDevTools({ mode: 'detach' });
  } else {
    await mainWindow.loadFile(path.join(app.getAppPath(), 'dist', 'index.html'));
  }

  mainWindow.on('closed', () => {
    mainWindow = null;
  });
};

const handleFileOpen = async (): Promise<OpenResult> => {
  if (!mainWindow) {
    return { canceled: true, filePath: null, content: '' };
  }

  const { canceled, filePaths } = await dialog.showOpenDialog(mainWindow, {
    title: 'Open markdown file',
    filters: markdownFilters,
    properties: ['openFile'],
  });

  if (canceled || filePaths.length === 0) {
    return { canceled: true, filePath: null, content: '' };
  }

  const filePath = filePaths[0];
  try {
    const content = await fs.readFile(filePath, 'utf8');
    return { canceled: false, filePath, content };
  } catch {
    return { canceled: true, filePath: null, content: '' };
  }
};

const handleFileSave = async (filePath: string | null, content: string): Promise<SaveResult> => {
  if (!filePath || !mainWindow) {
    return { canceled: true, filePath: null };
  }
  try {
    await fs.writeFile(filePath, content, 'utf8');
    return { canceled: false, filePath };
  } catch {
    return { canceled: true, filePath: null };
  }
};

const handleFileSaveAs = async (content: string): Promise<SaveResult> => {
  if (!mainWindow) {
    return { canceled: true, filePath: null };
  }

  const { canceled, filePath } = await dialog.showSaveDialog(mainWindow, {
    title: 'Save markdown file',
    defaultPath: 'untitled.md',
    filters: markdownFilters,
  });

  if (canceled || !filePath) {
    return { canceled: true, filePath: null };
  }

  try {
    await fs.writeFile(filePath, content, 'utf8');
    return { canceled: false, filePath };
  } catch {
    return { canceled: true, filePath: null };
  }
};

const registerHandlers = () => {
  ipcMain.handle('file-open', handleFileOpen);
  ipcMain.handle('file-save', (_event, filePath: string | null, content: string) =>
    handleFileSave(filePath, content),
  );
  ipcMain.handle('file-save-as', (_event, content: string) => handleFileSaveAs(content));
};

const bootstrap = async () => {
  registerHandlers();
  await createWindow();
};

app.whenReady().then(bootstrap);

app.on('activate', () => {
  if (BrowserWindow.getAllWindows().length === 0) {
    void bootstrap();
  }
});

app.on('window-all-closed', () => {
  app.quit();
});
