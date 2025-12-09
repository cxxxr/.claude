---
description: リポジトリからファクトベースのARCHITECTURE.mdを生成
argument-hint: "[directory-path]"
---

# ARCHITECTURE.md 生成

対象: $ARGUMENTS（未指定の場合はカレントディレクトリ）

## 役割

シニアソフトウェアアーキテクトとして、コードベースを分析し、
**事実のみに基づいた**アーキテクチャドキュメントを生成する。

## 基本原則

1. **推測禁止**: 確認できない情報は「未確認事項」セクションへ
2. **エビデンス必須**: 主要な主張には `ファイル:行番号` を付記
3. **簡潔優先**: 箇条書きを基本とし、冗長な説明を避ける

## 分析フェーズ

### Phase 1: 構造把握
- ディレクトリ構成を確認（深度3まで）
- 主要言語・フレームワークを特定
- エントリーポイント（main/index/app.*）を発見

### Phase 2: 依存関係
- package.json / go.mod / requirements.txt 等を解析
- 内部モジュール間の依存を把握
- 循環依存があれば記録

### Phase 3: フロー分析
- 起動シーケンスを追跡
- 主要ユースケース1-2件のデータフローを特定
- 外部連携（API/DB/キュー）を列挙

### Phase 4: 設定・運用
- 環境変数・設定ファイルを収集
- ログ・メトリクス・トレースの仕組みを確認
- CI/CDパイプラインを確認

## 出力仕様

### フロントマター（必須）

```yaml
---
title: "ARCHITECTURE"
repo: "<リポジトリ名>"
version: "v1"
generated_at: "<ISO8601形式>"
commit: "<HEADハッシュ>"
confidence: "<High|Medium|Low>"
---
```

### セクション構成

| # | セクション | 必須 | 内容 |
|---|-----------|:---:|------|
| 1 | Overview | ✓ | 目的・ユースケース・非目標 |
| 2 | System Architecture | ✓ | 全体構成図（Mermaid flowchart） |
| 3 | Execution Flow | ✓ | 主要フロー（Mermaid sequenceDiagram） |
| 4 | Modules & Dependencies | ✓ | レイヤー構成・主要パッケージ |
| 5 | Data Model | | スキーマ・ストア・関係性 |
| 6 | External Integrations | | API/DB/キュー/認証 |
| 7 | Configuration | | 環境変数・設定ファイル一覧 |
| 8 | Agent Design | ※ | Planner/Executor/Tools/Prompts/Memory |
| 9 | Observability | | ログ/メトリクス/トレース |
| 10 | Build & Release | | CI/CD・テスト戦略 |
| 11 | Risks & Improvements | ✓ | リスク3件以上・改善案3件以上 |
| 12 | Open Questions | ✓ | 未確認事項・要調査項目 |
| 13 | References | ✓ | エビデンス一覧（ファイル:行） |

※ Agent Designはエージェント系プロジェクトの場合のみ

### ダイアグラム要件

最低限以下を含める：

**システム構成図**（flowchart形式）
**シーケンス図**（主要ユースケース1件）

## エビデンス記載ルール

主張には必ず根拠を示す：

```markdown
## Modules

- **認証モジュール**: JWT認証を使用
  - `src/auth/jwt.ts:15-42`
  - `config/auth.yaml:8`
```

## 信頼度の判定基準

| レベル | 条件 |
|--------|------|
| High | 全必須セクションにエビデンスあり、Open Questionsが3件以下 |
| Medium | 必須セクションの80%以上にエビデンスあり |
| Low | エビデンス不足、または推測を含む可能性あり |

## 禁止事項

- エビデンスなしの断定
- 「おそらく」「〜と思われる」等の推測表現（Open Questionsへ移動）
- 設定値の推測（コード/設定に明記されているもののみ記載）
- 未確認のモデル名・パラメータの記載

## 出力

- ARCHITECTURE.md として完全な形で出力
- プレースホルダーや追加質問なしで完結させる
- 不明点はすべてOpen Questionsに集約
