# Git ブランチ名変更

現在のgitブランチ名を、作業内容に基づいて適切な名前に変更してください。

## 手順

1. `git branch --show-current` で現在のブランチ名を確認
2. 変更内容を確認：
   - デフォルトブランチ（`main` / `master`）の場合: `git log origin/HEAD..HEAD --oneline` と `git diff origin/HEAD --stat` で最後にpushされたところからの変更を確認
   - それ以外の場合: `git log --oneline -10` と `git diff --stat HEAD~5` で確認
3. 作業内容を分析し、適切なブランチ名を決定

## ブランチ命名規則

以下の形式に従ってください：

- `feature/<説明>` - 新機能の追加
- `fix/<説明>` - バグ修正
- `refactor/<説明>` - リファクタリング
- `docs/<説明>` - ドキュメントの更新
- `chore/<説明>` - 設定やビルド関連の変更
- `test/<説明>` - テストの追加・修正

説明部分は：
- 英語のケバブケース（kebab-case）を使用
- 簡潔で内容が分かる名前にする
- 例: `feature/add-user-authentication`, `fix/login-validation-error`

## 実行

1. 作業内容から適切なブランチ名を決定する
2. ブランチを作成・変更する：
   - デフォルトブランチ（`main` / `master`）の場合: `git checkout -b <新しいブランチ名>` で新しいブランチを作成
   - それ以外の場合: `git branch -m <新しいブランチ名>` で名前を変更
3. 変更後のブランチ名をユーザーに報告する
4. リモートブランチが存在する場合は、その旨を通知する（自動でpushはしない）
