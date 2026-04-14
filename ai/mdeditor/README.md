# Simple Markdown Editor

Windows 向けの Electron + TypeScript + React デスクトップアプリケーションです。  
MVP 機能として「新規作成 / 開く / 保存 / 名前を付けて保存 / リアルタイムプレビュー / テーマ切替」を実装しています。

## 使い方

```bash
pnpm install
pnpm run dev
```

`pnpm run dev` は以下を同時起動します。

1. `vite` 開発サーバー
2. `electron` 本体（`out/main`/`out/preload` を再生成して起動）

`5173` が他アプリと重なって起動できない場合は、
```bash
pnpm run dev:alt
```
を実行して `5174` を使って起動できます。

`Electron failed to install correctly` が出る場合は、`pnpm rebuild electron` でバイナリを再取得してください（必要なら再インストールと同時に再実行してください）。

## 配布

```bash
pnpm run dist
```

## 開発時注意

- Windows 向けに最適化しています。
- 実行・保存はすべて IPC（`main` と `renderer` 間）で安全な境界を保っています。
