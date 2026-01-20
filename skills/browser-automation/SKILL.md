---
name: browser-automation
description: Playwright MCPを使用したブラウザ自動化。ページ操作、スクリーンショット、フォーム入力のパターン集
allowed-tools: Bash, Read
---

# Browser Automation with Playwright MCP

Playwright MCPサーバーを使用したブラウザ自動化パターン集。

## 利用可能なMCPツール

### ナビゲーション
- `mcp__playwright__browser_navigate` - URLに移動
- `mcp__playwright__browser_navigate_back` - 戻る

### ページ情報取得
- `mcp__playwright__browser_snapshot` - アクセシビリティスナップショット（操作に最適）
- `mcp__playwright__browser_take_screenshot` - 視覚的なスクリーンショット
- `mcp__playwright__browser_console_messages` - コンソールログ
- `mcp__playwright__browser_network_requests` - ネットワークリクエスト

### インタラクション
- `mcp__playwright__browser_click` - クリック
- `mcp__playwright__browser_type` - テキスト入力
- `mcp__playwright__browser_hover` - ホバー
- `mcp__playwright__browser_drag` - ドラッグ&ドロップ
- `mcp__playwright__browser_select_option` - ドロップダウン選択
- `mcp__playwright__browser_press_key` - キー押下

### フォーム
- `mcp__playwright__browser_fill_form` - 複数フィールド一括入力
- `mcp__playwright__browser_file_upload` - ファイルアップロード

### ダイアログ
- `mcp__playwright__browser_handle_dialog` - alert/confirm/prompt処理

### タブ管理
- `mcp__playwright__browser_tabs` - タブの一覧/作成/閉じる/選択

### その他
- `mcp__playwright__browser_evaluate` - JavaScript実行
- `mcp__playwright__browser_wait_for` - 待機
- `mcp__playwright__browser_close` - ブラウザを閉じる
- `mcp__playwright__browser_resize` - ウィンドウサイズ変更

## 基本ワークフロー

### 1. ページを開いて分析
```
1. browser_navigate でURLに移動
2. browser_snapshot でページ構造を取得
3. スナップショットから操作対象の ref を特定
4. browser_click / browser_type で操作
```

### 2. スナップショットの読み方
```
browser_snapshot の結果例：

- button "ログイン" [ref=btn-login]
- textbox "メールアドレス" [ref=email-input]
- textbox "パスワード" [ref=password-input]
- link "新規登録" [ref=register-link]

→ ref値を使って操作
```

### 3. クリック操作
```
browser_click:
  element: "ログインボタン"
  ref: "btn-login"
```

### 4. テキスト入力
```
browser_type:
  element: "メールアドレス入力欄"
  ref: "email-input"
  text: "user@example.com"
  submit: false  # Enterを押すか
```

### 5. フォーム一括入力
```
browser_fill_form:
  fields:
    - name: "メールアドレス"
      type: "textbox"
      ref: "email-input"
      value: "user@example.com"
    - name: "パスワード"
      type: "textbox"
      ref: "password-input"
      value: "password123"
```

## 実践パターン

### パターン1: ログインフロー
```
1. browser_navigate: https://example.com/login
2. browser_snapshot: ページ構造を確認
3. browser_fill_form:
   - email: "user@example.com"
   - password: "****"
4. browser_click: ログインボタン
5. browser_wait_for: "ダッシュボード" (ログイン成功の確認)
6. browser_snapshot: ログイン後のページ確認
```

### パターン2: 検索して結果をクリック
```
1. browser_navigate: https://example.com
2. browser_snapshot: 検索ボックスを特定
3. browser_type: 検索キーワード入力
4. browser_press_key: "Enter"
5. browser_wait_for: 検索結果の表示
6. browser_snapshot: 結果一覧を確認
7. browser_click: 目的の結果をクリック
```

### パターン3: フォーム入力と送信
```
1. browser_navigate: フォームページ
2. browser_snapshot: フォーム構造を確認
3. browser_fill_form: 全フィールド入力
4. browser_take_screenshot: 入力内容を確認
5. browser_click: 送信ボタン
6. browser_wait_for: 完了メッセージ
```

### パターン4: 動的コンテンツの待機
```
1. browser_click: ロードボタン
2. browser_wait_for:
   text: "データを読み込みました"
   # または
   time: 3  # 3秒待機
3. browser_snapshot: 新しいコンテンツを確認
```

### パターン5: マルチタブ操作
```
1. browser_tabs: action="list"  # 現在のタブ確認
2. browser_tabs: action="new"   # 新しいタブを開く
3. browser_navigate: 新しいURLへ
4. browser_tabs: action="select", index=0  # 元のタブに戻る
5. browser_tabs: action="close", index=1   # 2番目のタブを閉じる
```

### パターン6: JavaScript実行
```
browser_evaluate:
  function: "() => document.title"
  # → ページタイトルを取得

browser_evaluate:
  function: "() => window.scrollTo(0, document.body.scrollHeight)"
  # → ページ最下部へスクロール

browser_evaluate:
  element: "データ表示エリア"
  ref: "data-container"
  function: "(element) => element.innerText"
  # → 特定要素のテキストを取得
```

## Tips

### スナップショット vs スクリーンショット
- **snapshot**: 操作用。要素のref取得、構造把握に最適
- **screenshot**: 確認用。視覚的な状態確認、デバッグに最適

### 待機のベストプラクティス
```
# Good: テキストの出現を待つ
browser_wait_for:
  text: "読み込み完了"

# Good: テキストの消失を待つ
browser_wait_for:
  textGone: "読み込み中..."

# OK: 時間で待つ（最終手段）
browser_wait_for:
  time: 2
```

### エラーハンドリング
```
1. 操作前に必ずsnapshot
2. 要素が見つからない場合はスクロールしてみる
3. 動的コンテンツはwait_forで待機
4. エラー時はscreenshotで状態確認
```

### セレクタの優先順位
```
1. ref (snapshotから取得)
2. role + name (アクセシビリティ属性)
3. text content
```

## トラブルシューティング

### 要素が見つからない
```
1. browser_snapshot で現在の状態を確認
2. スクロールが必要かもしれない
3. iframeの中かもしれない
4. 動的に生成されるなら wait_for
```

### クリックが効かない
```
1. 要素が表示されているか確認（viewport内）
2. 他の要素に覆われていないか
3. disabled状態でないか
4. hover してから click
```

### 入力が反映されない
```
1. フォーカスが当たっているか
2. readonly/disabled でないか
3. JavaScript による入力制御がないか
4. slowly: true で1文字ずつ入力してみる
```
