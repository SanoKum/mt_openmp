#!/bin/bash
# =============================================================
# HP Steam Turbine — AWS ベンチマーク
#
# Usage:
#   bash dev/scripts/run_hp_steam_aws.sh          # ベンチマーク開始
#   bash dev/scripts/run_hp_steam_aws.sh status    # 進捗確認
#   bash dev/scripts/run_hp_steam_aws.sh fetch      # 結果回収
# =============================================================
set -e

HOST=ec2-c7i
REMOTE_DIR=~/MULTALL-project/dev/test_cases_hp_steam
REMOTE_SCRIPT=~/MULTALL-project/dev/scripts/run_hp_steam_benchmark.sh
LOCAL_DIR=~/work/MULTALL-project/dev/test_cases_hp_steam/aws_results
SESSION=hp_bench

ACTION=${1:-run}

case "$ACTION" in

# -----------------------------------------------------------
run)
  echo "=== HP Steam AWS Benchmark — START ==="
  echo ""

  # 接続確認
  echo "[0] Checking SSH connection..."
  ssh -o ConnectTimeout=10 "$HOST" "echo 'Connected OK: \$(hostname)'"

  # screen 内でベンチマーク起動（バックグラウンド、切断耐性あり）
  echo ""
  echo "[1] Launching benchmark in screen session '$SESSION'..."
  ssh "$HOST" "screen -dmS $SESSION bash -c 'cd $REMOTE_DIR && bash $REMOTE_SCRIPT 2>&1 | tee benchmark.log; echo DONE >> benchmark.log'"

  echo ""
  echo "=== Benchmark started on AWS ==="
  echo ""
  echo "Next steps:"
  echo "  1. SSH して Y/N プロンプトに応答:"
  echo "       ssh -t ec2-c7i \"screen -r $SESSION\""
  echo "     (Y を入力して Enter。その後 Ctrl+A, D でデタッチ)"
  echo ""
  echo "  2. 進捗確認:"
  echo "       bash dev/scripts/run_hp_steam_aws.sh status"
  echo ""
  echo "  3. 完了後に結果回収:"
  echo "       bash dev/scripts/run_hp_steam_aws.sh fetch"
  ;;

# -----------------------------------------------------------
status)
  echo "=== HP Steam AWS Benchmark — STATUS ==="
  echo ""

  # screen セッション確認
  echo "--- Screen session ---"
  ssh "$HOST" "screen -ls 2>&1 | grep -E '$SESSION|No Sockets' || true"
  echo ""

  # benchmark.log の末尾
  echo "--- Last 20 lines of benchmark.log ---"
  ssh "$HOST" "tail -20 $REMOTE_DIR/benchmark.log 2>/dev/null || echo '(no log yet)'"
  echo ""

  # 既存 stage.log ファイル
  echo "--- Completed stage.log files ---"
  ssh "$HOST" "ls -lt $REMOTE_DIR/stage.log.hp_* 2>/dev/null || echo '(none yet)'"
  ;;

# -----------------------------------------------------------
fetch)
  echo "=== HP Steam AWS Benchmark — FETCH RESULTS ==="
  echo ""

  mkdir -p "$LOCAL_DIR"

  scp "$HOST:$REMOTE_DIR/stage.log.hp_*" "$LOCAL_DIR/" 2>/dev/null || echo "No stage.log.hp_* files"
  scp "$HOST:$REMOTE_DIR/run_*.out" "$LOCAL_DIR/" 2>/dev/null || echo "No run_*.out files"
  scp "$HOST:$REMOTE_DIR/benchmark.log" "$LOCAL_DIR/" 2>/dev/null || true

  echo ""
  echo "=== Results saved to: $LOCAL_DIR ==="
  ls -la "$LOCAL_DIR"/
  echo ""
  echo "--- LOOP: TOTAL summary ---"
  for f in "$LOCAL_DIR"/stage.log.hp_*; do
    [ -f "$f" ] || continue
    printf "  %-45s " "$(basename $f)"
    grep "LOOP: TOTAL" "$f" | tail -1 | awk '{print $NF}' || echo "(no timer)"
  done
  echo ""
  echo "Done."
  ;;

*)
  echo "Usage: $0 [run|status|fetch]"
  exit 1
  ;;

esac
