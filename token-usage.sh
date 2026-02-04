#!/usr/bin/env bash
# Claude Code トークン使用量確認スクリプト

set -euo pipefail

usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -s, --session ID    特定のセッションIDを指定"
    echo "  -p, --project PATH  プロジェクトパスを指定 (default: current dir)"
    echo "  -a, --all           全セッションの合計を表示"
    echo "  -l, --list          最近のセッション一覧を表示"
    echo "  --since DATETIME    指定日時以降のトークンのみ集計"
    echo "                      形式: 'YYYY-MM-DD' or 'YYYY-MM-DD HH:MM'"
    echo "  --since-login       最後のログイン以降のトークンのみ集計"
    echo "  -h, --help          このヘルプを表示"
    echo ""
    echo "Examples:"
    echo "  $0                  # 最新セッションの使用量"
    echo "  $0 -l               # セッション一覧"
    echo "  $0 -a               # 全セッション合計"
    echo "  $0 -a --since '2026-02-04'           # 2/4以降の全セッション"
    echo "  $0 -a --since '2026-02-04 10:00'     # 2/4 10:00以降"
    echo "  $0 -a --since-login                  # 最後のログイン以降"
}

# 最後のログイン時刻を取得（.credentials.jsonの更新時刻）
get_login_time() {
    local cred_file="$HOME/.claude/.credentials.json"
    if [ -f "$cred_file" ]; then
        # ファイルの更新時刻をISO8601 UTC形式で取得
        stat -c '%Y' "$cred_file" | xargs -I{} date -u -d @{} '+%Y-%m-%dT%H:%M:%S.000Z'
    else
        echo ""
    fi
}

get_project_dir() {
    local cwd="${1:-}"
    [ -z "$cwd" ] && cwd=$(pwd)
    local encoded=$(echo "$cwd" | sed 's|/|-|g')
    echo "$HOME/.claude/projects/$encoded"
}

# 全プロジェクトディレクトリを取得
get_all_project_dirs() {
    local base="$HOME/.claude/projects"
    [ -d "$base" ] && find "$base" -mindepth 1 -maxdepth 1 -type d 2>/dev/null
}

# 全プロジェクトから最新のセッションファイルを探す
find_latest_session_file() {
    local base="$HOME/.claude/projects"
    [ -d "$base" ] && find "$base" -name "*.jsonl" -type f 2>/dev/null | xargs ls -t 2>/dev/null | head -1
}

# 全プロジェクトから最新セッションがあるプロジェクトディレクトリを取得
find_active_project_dir() {
    local latest=$(find_latest_session_file)
    [ -n "$latest" ] && dirname "$latest"
}

calc_tokens() {
    local file="$1"
    local since="${2:-}"

    if [ -n "$since" ]; then
        jq -s --arg since "$since" '
          [.[] | select(.message.usage) | select(.timestamp >= $since)] |
          {
            input: (map(.message.usage.input_tokens // 0) | add),
            output: (map(.message.usage.output_tokens // 0) | add),
            cache_creation: (map(.message.usage.cache_creation_input_tokens // 0) | add),
            cache_read: (map(.message.usage.cache_read_input_tokens // 0) | add),
            messages: length,
            first_timestamp: (map(.timestamp) | min),
            last_timestamp: (map(.timestamp) | max)
          }
        ' "$file" 2>/dev/null
    else
        jq -s '
          [.[] | select(.message.usage)] |
          {
            input: (map(.message.usage.input_tokens // 0) | add),
            output: (map(.message.usage.output_tokens // 0) | add),
            cache_creation: (map(.message.usage.cache_creation_input_tokens // 0) | add),
            cache_read: (map(.message.usage.cache_read_input_tokens // 0) | add),
            messages: length,
            first_timestamp: (map(.timestamp) | min),
            last_timestamp: (map(.timestamp) | max)
          }
        ' "$file" 2>/dev/null
    fi
}

# ローカル時間をUTC ISO8601形式に変換
to_iso8601() {
    local input="$1"
    # 既にISO8601 UTC形式の場合はそのまま返す
    if [[ "$input" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}T.*Z$ ]]; then
        echo "$input"
        return
    fi
    # ローカルタイムゾーン名を取得
    local tz=$(date +%Z)
    # dateコマンドでローカル時間をUTCに変換
    # "YYYY-MM-DD HH:MM" 形式
    if [[ "$input" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}\ [0-9]{2}:[0-9]{2}$ ]]; then
        date -u -d "$input $tz" '+%Y-%m-%dT%H:%M:%S.000Z' 2>/dev/null || echo "$input"
        return
    fi
    # "YYYY-MM-DD" 形式
    if [[ "$input" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
        date -u -d "$input 00:00 $tz" '+%Y-%m-%dT%H:%M:%S.000Z' 2>/dev/null || echo "$input"
        return
    fi
    echo "$input"
}

format_number() {
    local num="$1"
    if [ "$num" -ge 1000000 ]; then
        # 1M以上: 小数点1桁まで表示（例: 1.5M）
        local int=$((num / 1000000))
        local dec=$(( (num % 1000000) / 100000 ))
        if [ "$dec" -eq 0 ]; then
            echo "${int}M"
        else
            echo "${int}.${dec}M"
        fi
    elif [ "$num" -ge 1000 ]; then
        # 1k以上: 小数点1桁まで表示（例: 1.5k）
        local int=$((num / 1000))
        local dec=$(( (num % 1000) / 100 ))
        if [ "$dec" -eq 0 ]; then
            echo "${int}k"
        else
            echo "${int}.${dec}k"
        fi
    else
        echo "$num"
    fi
}

# UTC ISO8601をローカル時間に変換して表示用フォーマット
utc_to_local() {
    local utc="$1"
    [ -z "$utc" ] || [ "$utc" = "null" ] || [ "$utc" = "N/A" ] && echo "N/A" && return
    date -d "$utc" '+%Y-%m-%d %H:%M:%S' 2>/dev/null || echo "$utc"
}

show_session() {
    local file="$1"
    local since="${2:-}"
    local result=$(calc_tokens "$file" "$since")

    if [ -z "$result" ] || [ "$result" = "null" ]; then
        echo "トークン情報が見つかりません"
        return 1
    fi

    local input=$(echo "$result" | jq -r '.input // 0')
    local output=$(echo "$result" | jq -r '.output // 0')
    local cache_create=$(echo "$result" | jq -r '.cache_creation // 0')
    local cache_read=$(echo "$result" | jq -r '.cache_read // 0')
    local messages=$(echo "$result" | jq -r '.messages // 0')
    local first_ts=$(echo "$result" | jq -r '.first_timestamp // "N/A"')
    local last_ts=$(echo "$result" | jq -r '.last_timestamp // "N/A"')

    echo "セッション: $(basename "$file" .jsonl)"
    [ -n "$since" ] && echo "フィルタ: $(utc_to_local "$since") 以降"
    echo "─────────────────────────────────"
    printf "入力トークン:     %12s\n" "$(format_number "$input")"
    printf "出力トークン:     %12s\n" "$(format_number "$output")"
    printf "キャッシュ作成:   %12s\n" "$(format_number "$cache_create")"
    printf "キャッシュ読取:   %12s\n" "$(format_number "$cache_read")"
    printf "メッセージ数:     %12s\n" "$(format_number "$messages")"
    echo "─────────────────────────────────"
    printf "合計トークン:     %12s\n" "$(format_number $((input + output + cache_create + cache_read)))"
    echo "─────────────────────────────────"
    echo "期間: $(utc_to_local "$first_ts") ~ $(utc_to_local "$last_ts")"
}

list_sessions() {
    local project_dir="$1"

    if [ ! -d "$project_dir" ]; then
        echo "プロジェクトディレクトリが見つかりません: $project_dir"
        return 1
    fi

    echo "最近のセッション:"
    echo "─────────────────────────────────────────────────────────"
    printf "%-38s %10s %10s\n" "セッションID" "更新日時" "サイズ"
    echo "─────────────────────────────────────────────────────────"

    ls -lt "$project_dir"/*.jsonl 2>/dev/null | head -10 | while read -r line; do
        local file=$(echo "$line" | awk '{print $NF}')
        local size=$(echo "$line" | awk '{print $5}')
        local date=$(echo "$line" | awk '{print $6, $7, $8}')
        local session=$(basename "$file" .jsonl)
        printf "%-38s %10s %10s\n" "$session" "$date" "${size}B"
    done
}

all_sessions() {
    local project_dir="$1"
    local since="${2:-}"
    local total_input=0
    local total_output=0
    local total_cache_create=0
    local total_cache_read=0
    local total_messages=0
    local session_count=0

    for file in "$project_dir"/*.jsonl; do
        [ -f "$file" ] || continue
        local result=$(calc_tokens "$file" "$since")
        [ -z "$result" ] || [ "$result" = "null" ] && continue

        local msg_count=$(echo "$result" | jq -r '.messages // 0')
        [ "$msg_count" -eq 0 ] && continue

        total_input=$((total_input + $(echo "$result" | jq -r '.input // 0')))
        total_output=$((total_output + $(echo "$result" | jq -r '.output // 0')))
        total_cache_create=$((total_cache_create + $(echo "$result" | jq -r '.cache_creation // 0')))
        total_cache_read=$((total_cache_read + $(echo "$result" | jq -r '.cache_read // 0')))
        total_messages=$((total_messages + msg_count))
        session_count=$((session_count + 1))
    done

    echo "全セッション合計 ($session_count セッション)"
    [ -n "$since" ] && echo "フィルタ: $(utc_to_local "$since") 以降"
    echo "─────────────────────────────────"
    printf "入力トークン:     %12s\n" "$(format_number "$total_input")"
    printf "出力トークン:     %12s\n" "$(format_number "$total_output")"
    printf "キャッシュ作成:   %12s\n" "$(format_number "$total_cache_create")"
    printf "キャッシュ読取:   %12s\n" "$(format_number "$total_cache_read")"
    printf "メッセージ数:     %12s\n" "$(format_number "$total_messages")"
    echo "─────────────────────────────────"
    printf "合計トークン:     %12s\n" "$(format_number $((total_input + total_output + total_cache_create + total_cache_read)))"
}

# Main
PROJECT_PATH=""
SESSION_ID=""
SHOW_ALL=false
SHOW_LIST=false
SINCE=""

while [[ $# -gt 0 ]]; do
    case $1 in
        -s|--session) SESSION_ID="$2"; shift 2 ;;
        -p|--project) PROJECT_PATH="$2"; shift 2 ;;
        -a|--all) SHOW_ALL=true; shift ;;
        -l|--list) SHOW_LIST=true; shift ;;
        --since) SINCE=$(to_iso8601 "$2"); shift 2 ;;
        --since-login) SINCE=$(get_login_time); shift ;;
        -h|--help) usage; exit 0 ;;
        *) echo "Unknown option: $1"; usage; exit 1 ;;
    esac
done

# プロジェクトディレクトリの決定
if [ -n "$PROJECT_PATH" ]; then
    # 明示的に指定された場合
    PROJECT_DIR=$(get_project_dir "$PROJECT_PATH")
else
    # 現在のディレクトリから探す
    PROJECT_DIR=$(get_project_dir)
    # 見つからなければ全プロジェクトから最新のものを探す
    if [ ! -d "$PROJECT_DIR" ]; then
        PROJECT_DIR=$(find_active_project_dir)
    fi
fi

if [ -z "$PROJECT_DIR" ] || [ ! -d "$PROJECT_DIR" ]; then
    echo "プロジェクトディレクトリが見つかりません"
    echo "~/.claude/projects/ にセッションデータがありません"
    exit 1
fi

if $SHOW_LIST; then
    list_sessions "$PROJECT_DIR"
elif $SHOW_ALL; then
    all_sessions "$PROJECT_DIR" "$SINCE"
elif [ -n "$SESSION_ID" ]; then
    SESSION_FILE="$PROJECT_DIR/$SESSION_ID.jsonl"
    if [ -f "$SESSION_FILE" ]; then
        show_session "$SESSION_FILE" "$SINCE"
    else
        echo "セッションファイルが見つかりません: $SESSION_FILE"
        exit 1
    fi
else
    # 最新のセッションを表示
    LATEST=$(ls -t "$PROJECT_DIR"/*.jsonl 2>/dev/null | head -1)
    if [ -n "$LATEST" ]; then
        show_session "$LATEST" "$SINCE"
    else
        echo "セッションファイルが見つかりません"
        exit 1
    fi
fi
