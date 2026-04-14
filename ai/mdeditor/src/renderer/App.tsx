import { useEffect, useState } from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { markdown } from '@codemirror/lang-markdown';
import ReactMarkdown from 'react-markdown';
import remarkGfm from 'remark-gfm';

type FileState = {
  path: string | null;
  content: string;
  dirty: boolean;
};

export function App() {
  const [file, setFile] = useState<FileState>({ path: null, content: '# Welcome\n\nType **markdown** here.', dirty: false });
  const [theme, setTheme] = useState<'light' | 'dark'>('light');
  const [error, setError] = useState('');

  useEffect(() => {
    const root = document.documentElement;
    root.setAttribute('data-theme', theme);
  }, [theme]);

  useEffect(() => {
    const handler = (event: BeforeUnloadEvent) => {
      if (!file.dirty) return;
      event.preventDefault();
      event.returnValue = '';
    };
    window.addEventListener('beforeunload', handler);
    return () => window.removeEventListener('beforeunload', handler);
  }, [file.dirty]);

  const markDirty = (content: string) => {
    setFile((prev) => ({ ...prev, content, dirty: true }));
  };

  const onNew = () => {
    if (file.dirty && !window.confirm('未保存の変更があります。破棄して新規作成しますか？')) {
      return;
    }
    setFile({ path: null, content: '', dirty: false });
  };

  const onOpen = async () => {
    if (file.dirty && !window.confirm('未保存の変更があります。破棄して開き直しますか？')) {
      return;
    }
    try {
      const result = await window.api.openFile();
      if (result.canceled) return;
      setFile({ path: result.filePath, content: result.content, dirty: false });
      setError('');
    } catch (ex) {
      setError(`ファイルを開けませんでした: ${String(ex)}`);
    }
  };

  const onSave = async () => {
    try {
      let nextPath = file.path;
      if (!nextPath) {
        const saveAs = await window.api.saveFileAs(file.content);
        if (saveAs.canceled || !saveAs.filePath) return;
        nextPath = saveAs.filePath;
      } else {
        const saved = await window.api.saveFile(nextPath, file.content);
        if (saved.canceled) {
          return;
        }
      }
      setFile((prev) => ({ ...prev, path: nextPath, dirty: false }));
      setError('');
    } catch (ex) {
      setError(`保存できませんでした: ${String(ex)}`);
    }
  };

  const onSaveAs = async () => {
    try {
      const saved = await window.api.saveFileAs(file.content);
      if (saved.canceled || !saved.filePath) return;
      setFile((prev) => ({ ...prev, path: saved.filePath, dirty: false }));
      setError('');
    } catch (ex) {
      setError(`保存できませんでした: ${String(ex)}`);
    }
  };

  return (
    <div className="app">
      <header className="toolbar">
        <button onClick={onNew}>新規</button>
        <button onClick={onOpen}>開く</button>
        <button onClick={onSave}>保存</button>
        <button onClick={onSaveAs}>名前を付けて保存</button>
        <button onClick={() => setTheme((current) => (current === 'light' ? 'dark' : 'light'))}>
          テーマ: {theme === 'light' ? 'ライト' : 'ダーク'}
        </button>
        <div className="toolbar-status">
          {file.path ? file.path.split(/[\\/]/).pop() : '無題'}
          {file.dirty ? ' *' : ''}
        </div>
      </header>

      <div className="content">
        <section className="editor-pane">
          <CodeMirror
            value={file.content}
            extensions={[markdown()]}
            height="100%"
            onChange={markDirty}
          />
        </section>
        <section className="preview-pane">
          <ReactMarkdown remarkPlugins={[remarkGfm]}>{file.content}</ReactMarkdown>
        </section>
      </div>

      {error && <div className="error-banner">{error}</div>}
    </div>
  );
}
