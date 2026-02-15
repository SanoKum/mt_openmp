#!/bin/bash
# compare_performance.sh — stdout ログファイルから最終の全体性能を抽出・比較する
#
# 使い方:
#   bash compare_performance.sh file1.out [file2.out ...]
#   bash compare_performance.sh file1.out file2.out   # 2ファイルなら差分も表示
#
# 抽出項目:
#   - 最終ステップの EMAX, EAVG, ECONT, FLOW (収束指標)
#   - 全体の圧力比 (PR)
#   - 全体の eta_TT, eta_TS (等エントロピー効率)
#   - 出力 (POWER, kW)
#   - 入口/出口流量

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 logfile1.out [logfile2.out ...]"
    exit 1
fi

extract_perf() {
    local file="$1"
    local label
    label=$(basename "$file")

    # --- 最終ステップ収束値 ---
    # stdout format: " 1000  0.3993  33 806  23  0.0183  0.2146 ..."
    # Match lines starting with whitespace + number + whitespace + number (step + EMAX)
    local step_line
    step_line=$(grep -E '^\s+[0-9]+\s+[-]?[0-9]+\.[0-9]+\s+[0-9]+\s+[0-9]+\s+[0-9]+' "$file" | tail -1)

    local nstep="" emax="" eavg="" econt="" flow=""
    if [ -n "$step_line" ]; then
        nstep=$(echo "$step_line" | awk '{print $1}')
        emax=$(echo "$step_line" | awk '{print $2}')
        eavg=$(echo "$step_line" | awk '{print $6}')
        econt=$(echo "$step_line" | awk '{print $7}')
        flow=$(echo "$step_line" | awk '{print $(NF-2)}')
    fi

    # --- 全体性能 (最後の "WHOLE MACHINE" ブロック) ---
    # 最後の "THE FOLLOWING RESULTS ARE FOR THE WHOLE MACHINE" 以降を抜き出す
    local whole_block
    whole_block=$(awk '/THE FOLLOWING RESULTS ARE FOR THE WHOLE MACHINE/{found=NR} found{block=block $0 "\n"} END{print block}' "$file" | \
        awk '/THE FOLLOWING RESULTS ARE FOR THE WHOLE MACHINE/{buf=""} {buf=buf $0 "\n"} END{printf "%s", buf}')

    local pr="" eta_tt="" eta_ts="" power="" flow_in="" flow_out=""

    if [ -n "$whole_block" ]; then
        pr=$(echo "$whole_block" | grep "PRESSURE RATIO" | head -1 | grep -oP '=\s*\K[0-9.]+')
        eta_tt=$(echo "$whole_block" | grep "TOTAL TO TOTAL ISENTROPIC" | head -1 | grep -oP '=\s*\K[0-9.]+')
        eta_ts=$(echo "$whole_block" | grep "TOTAL TO STATIC ISENTROPIC" | head -1 | grep -oP '=\s*\K[0-9.]+')
        power=$(echo "$whole_block" | grep "OVERALL POWER OUTPUT" | head -1 | grep -oP '=\s*\K[0-9.]+')
        # Mass flow: "INLET AND OUTLET MASS FLOW RATES = 135.302 110.515 Kg/sec."
        local flow_line
        flow_line=$(echo "$whole_block" | grep "INLET AND OUTLET MASS FLOW RATES" | head -1)
        flow_in=$(echo "$flow_line" | grep -oP '[0-9]+\.[0-9]+' | head -1)
        flow_out=$(echo "$flow_line" | grep -oP '[0-9]+\.[0-9]+' | head -2 | tail -1)
    fi

    # Output as key=value for easy comparison
    echo "FILE=$label"
    echo "STEP=$nstep"
    echo "EMAX=$emax"
    echo "EAVG=$eavg"
    echo "ECONT=$econt"
    echo "FLOW=$flow"
    echo "PR=$pr"
    echo "ETA_TT=$eta_tt"
    echo "ETA_TS=$eta_ts"
    echo "POWER=$power"
    echo "FLOW_IN=$flow_in"
    echo "FLOW_OUT=$flow_out"
}

# --- メイン処理 ---
declare -a ALL_FILES=("$@")
declare -a ALL_DATA=()

for f in "${ALL_FILES[@]}"; do
    if [ ! -f "$f" ]; then
        echo "ERROR: File not found: $f" >&2
        exit 1
    fi
    data=$(extract_perf "$f")
    ALL_DATA+=("$data")
done

# 表示用関数
get_val() {
    echo "$1" | grep "^$2=" | cut -d= -f2
}

# --- 表形式で出力 ---
ITEMS=("STEP" "EMAX" "EAVG" "ECONT" "FLOW" "PR" "ETA_TT" "ETA_TS" "POWER" "FLOW_IN" "FLOW_OUT")
LABELS=("Step" "EMAX" "EAVG" "ECONT" "FLOW" "Pressure Ratio" "eta_TT" "eta_TS" "Power (kW)" "Flow In (kg/s)" "Flow Out (kg/s)")

# Header
printf "%-18s" ""
for d in "${ALL_DATA[@]}"; do
    fname=$(get_val "$d" "FILE")
    printf "  %-22s" "$fname"
done
echo ""
printf "%-18s" ""
for d in "${ALL_DATA[@]}"; do
    printf "  %-22s" "----------------------"
done
echo ""

# Data rows
for i in "${!ITEMS[@]}"; do
    key="${ITEMS[$i]}"
    label="${LABELS[$i]}"
    printf "%-18s" "$label"
    for d in "${ALL_DATA[@]}"; do
        val=$(get_val "$d" "$key")
        printf "  %-22s" "${val:--}"
    done
    echo ""
done

# --- 2ファイルの場合は差分を表示 ---
if [ ${#ALL_DATA[@]} -eq 2 ]; then
    echo ""
    echo "=== Difference (file2 - file1) ==="
    NUMERIC_ITEMS=("EMAX" "EAVG" "ECONT" "FLOW" "PR" "ETA_TT" "ETA_TS" "POWER" "FLOW_IN" "FLOW_OUT")
    NUMERIC_LABELS=("EMAX" "EAVG" "ECONT" "FLOW" "Pressure Ratio" "eta_TT" "eta_TS" "Power (kW)" "Flow In (kg/s)" "Flow Out (kg/s)")

    printf "%-18s  %-14s  %-14s  %-14s\n" "" "Value 1" "Value 2" "Rel.Diff (%)"
    printf "%-18s  %-14s  %-14s  %-14s\n" "" "--------------" "--------------" "--------------"

    for i in "${!NUMERIC_ITEMS[@]}"; do
        key="${NUMERIC_ITEMS[$i]}"
        label="${NUMERIC_LABELS[$i]}"
        v1=$(get_val "${ALL_DATA[0]}" "$key")
        v2=$(get_val "${ALL_DATA[1]}" "$key")
        if [ -n "$v1" ] && [ -n "$v2" ]; then
            reldiff=$(python3 -c "
v1, v2 = float('$v1'), float('$v2')
if abs(v1) > 1e-30:
    rd = (v2 - v1) / abs(v1) * 100.0
    print(f'{rd:+.6f}')
else:
    print('N/A')
" 2>/dev/null || echo "N/A")
            printf "%-18s  %-14s  %-14s  %-14s\n" "$label" "$v1" "$v2" "$reldiff"
        else
            printf "%-18s  %-14s  %-14s  %-14s\n" "$label" "${v1:--}" "${v2:--}" "-"
        fi
    done
fi
