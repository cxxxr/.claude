---
description: 現在のブランチのPRをマージし、ローカルを整理
---

# PRマージとブランチクリーンアップ

現在のブランチに対応するGitHub PRをマージし、ローカル環境を整理する。

## 手順

1. 現在の状態を確認：
   - `git branch --show-current` で現在のブランチ名を取得
   - デフォルトブランチ（main/master）でないことを確認
   - デフォルトブランチの場合はエラーを報告して終了

2. 対応するPRを確認：
   - `gh pr view --json number,state,title,mergeable` でPR情報を取得
   - PRが存在しない場合はエラーを報告
   - PRがすでにマージ済みの場合はステップ4へスキップ
   - マージ可能な状態か確認

3. PRをマージ：
   - `gh pr merge --merge` でPRをマージ（マージコミットを作成）
   - 必要に応じて `--squash` や `--rebase` オプションを使用
   - マージ失敗時はエラーを報告

4. デフォルトブランチに切り替え：
   - `gh repo view --json defaultBranchRef --jq '.defaultBranchRef.name'` でデフォルトブランチ名を取得
   - `git checkout <default-branch>` でブランチを切り替え

5. 最新の状態を取得：
   - `git pull` でリモートの最新を取得

6. PRブランチを削除：
   - `git branch -d <pr-branch>` でローカルブランチを削除
   - 削除できない場合は `-D` オプションで強制削除を提案

## 注意事項

- main/masterブランチではこのコマンドを実行しない
- 未コミットの変更がある場合は先に対処を促す
- マージ方法はプロジェクトの慣習に従う（デフォルトはmerge commit）
- 完了後、マージされたPRの情報を報告する
