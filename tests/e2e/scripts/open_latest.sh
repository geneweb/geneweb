# Usage: ./open_latest.sh
ARTDIR="$(dirname "$0")/../reports/artifacts"
if [ ! -d "$ARTDIR" ]; then
  echo "No artifacts directory: $ARTDIR"
  exit 1
fi
latest="$(ls -1t "$ARTDIR" | head -n 1)"
if [ -z "$latest" ]; then
  echo "No artifacts found in $ARTDIR"
  exit 0
fi
full="$ARTDIR/$latest"
echo "Opening: $full"
case "$(uname)" in
  Darwin) open "$full" ;;
  Linux) xdg-open "$full" ;;
  *) echo "Platform not supported for automatic open. File path: $full" ;;
esac
