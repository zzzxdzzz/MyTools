#!/usr/bin/env zsh
# ============================================================
#  copy_media.sh — 递归复制指定目录下所有照片/视频到目标路径
#  用法：./copy_media.sh <源路径A> <目标路径B>
#  例：  ./copy_media.sh /Volumes/MyDisk/Photos ~/Desktop/Media_Output
# ============================================================
 
set -euo pipefail
 
# ── 颜色输出 ────────────────────────────────────────────────
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
CYAN='\033[0;36m'; NC='\033[0m'
 
# ── 参数检查 ────────────────────────────────────────────────
if [[ $# -lt 2 ]]; then
  echo -e "${YELLOW}用法：$0 <源路径A> <目标路径B>${NC}"
  echo "  源路径A：要扫描的目录（含所有子目录）"
  echo "  目标路径B：复制到的目标目录（不存在会自动创建）"
  exit 1
fi
 
SRC="${1%/}"   # 去掉末尾斜杠
DST="${2%/}"
 
# ── 校验源路径 ──────────────────────────────────────────────
if [[ ! -d "$SRC" ]]; then
  echo -e "${RED}错误：源路径不存在 → $SRC${NC}"; exit 1
fi
 
# ── 创建目标路径 ────────────────────────────────────────────
mkdir -p "$DST"
 
# ── 支持的文件格式（全部小写，脚本自动匹配大写） ────────────
# 图片格式
PHOTO_EXT=(jpg jpeg png gif bmp tiff tif heic heif webp
           raw cr2 cr3 nef nrw arw orf rw2 dng pef srw
           psd ai svg eps ico)
# 视频格式
VIDEO_EXT=(mp4 mov avi mkv m4v wmv flv webm 3gp 3g2
           mts m2ts ts mxf vob ogv rm rmvb mpg mpeg
           divx xvid f4v asf)
 
# 拼接 find 的 -iname 条件
build_find_args() {
  local -a args=()
  local first=true
  for ext in "$@"; do
    if $first; then
      args+=(-iname "*.${ext}")
      first=false
    else
      args+=(-o -iname "*.${ext}")
    fi
  done
  echo "${args[@]}"
}
 
PHOTO_ARGS=$(build_find_args "${PHOTO_EXT[@]}")
VIDEO_ARGS=$(build_find_args "${VIDEO_EXT[@]}")
 
# ── 统计变量 ────────────────────────────────────────────────
total=0; copied=0; skipped=0; renamed=0
 
echo -e "${CYAN}══════════════════════════════════════════${NC}"
echo -e "${CYAN}  Media Copy Script${NC}"
echo -e "${CYAN}  源：$SRC${NC}"
echo -e "${CYAN}  目标：$DST${NC}"
echo -e "${CYAN}══════════════════════════════════════════${NC}"
echo ""
 
# ── 核心复制函数 ────────────────────────────────────────────
copy_file() {
  local src_file="$1"
  local filename="${src_file:t}"          # zsh 取文件名
  local dest_file="$DST/$filename"
 
  ((total++))
 
  # 处理文件名冲突：加 _1 _2 ... 后缀
  if [[ -e "$dest_file" ]]; then
    local base="${filename:r}"            # 不含扩展名
    local ext="${filename:e}"             # 扩展名
    local counter=1
    while [[ -e "$DST/${base}_${counter}.${ext}" ]]; do
      ((counter++))
    done
    dest_file="$DST/${base}_${counter}.${ext}"
    ((renamed++))
    echo -e "  ${YELLOW}⚠ 重名重命名${NC}: $filename → ${base}_${counter}.${ext}"
  fi
 
  cp "$src_file" "$dest_file" && ((copied++)) || {
    echo -e "  ${RED}✗ 复制失败${NC}: $src_file"
    ((skipped++))
  }
}
 
# ── 执行 find + 逐文件复制 ──────────────────────────────────
echo -e "${GREEN}▶ 开始扫描并复制...${NC}"
 
# 使用 eval + while read 安全处理含空格的路径
eval "find \"$SRC\" -type f \( $PHOTO_ARGS -o $VIDEO_ARGS \)" | sort | while IFS= read -r file; do
  echo -e "  → ${file##$SRC/}"
  copy_file "$file"
done
 
# ── 汇总报告 ────────────────────────────────────────────────
echo ""
echo -e "${CYAN}══════════════════════════════════════════${NC}"
echo -e "${GREEN}✔ 完成！${NC}"
echo -e "  扫描文件总数：${total}"
echo -e "  成功复制：    ${GREEN}${copied}${NC}"
echo -e "  重命名处理：  ${YELLOW}${renamed}${NC}"
echo -e "  失败跳过：    ${RED}${skipped}${NC}"
echo -e "  目标路径：    $DST"
echo -e "${CYAN}══════════════════════════════════════════${NC}"