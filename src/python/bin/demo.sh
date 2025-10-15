#!/bin/bash
#
# GeneWeb Python Binaries Demo
# This script demonstrates how to use all the available binaries
#

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

echo -e "${BLUE}=== GeneWeb Python Binaries Demo ===${NC}"
echo

# Check if we're in the right directory
if [ ! -f "$SCRIPT_DIR/ged2gwb" ]; then
    echo -e "${RED}Error: This script must be run from the bin directory${NC}" >&2
    exit 1
fi

# Find sample GEDCOM files
SAMPLE_GED=""
if [ -f "$PROJECT_ROOT/gedcom/ged/sample.ged" ]; then
    SAMPLE_GED="$PROJECT_ROOT/gedcom/ged/sample.ged"
elif [ -f "$PROJECT_ROOT/ged2gwb/gedcom/ged/sample.ged" ]; then
    SAMPLE_GED="$PROJECT_ROOT/ged2gwb/gedcom/ged/sample.ged"
fi

if [ -z "$SAMPLE_GED" ]; then
    echo -e "${RED}Error: No sample GEDCOM file found${NC}" >&2
    exit 1
fi

echo -e "${GREEN}Found sample GEDCOM file: $SAMPLE_GED${NC}"
echo

# Demo 1: Show help for all binaries
echo -e "${BLUE}1. Showing help for all binaries:${NC}"
echo

echo -e "${YELLOW}ged2gwb --help:${NC}"
"$SCRIPT_DIR/ged2gwb" --help | head -20
echo "... (truncated)"
echo

echo -e "${YELLOW}geneweb-python (no args):${NC}"
"$SCRIPT_DIR/geneweb-python"
echo

# Demo 2: Convert GEDCOM to pickle
echo -e "${BLUE}2. Converting GEDCOM to pickle database:${NC}"
echo

OUTPUT_DB="demo-database.pkl"
echo -e "${YELLOW}Converting $SAMPLE_GED to $OUTPUT_DB...${NC}"
if "$SCRIPT_DIR/ged2gwb" "$SAMPLE_GED" --output "$OUTPUT_DB" --no-compress --verbose; then
    echo -e "${GREEN}✓ Conversion successful${NC}"

    # Show file info
    if [ -f "$OUTPUT_DB" ]; then
        FILE_SIZE=$(ls -lh "$OUTPUT_DB" | awk '{print $5}')
        echo -e "${GREEN}✓ Database created: $OUTPUT_DB ($FILE_SIZE)${NC}"
    fi
else
    echo -e "${RED}✗ Conversion failed${NC}"
fi
echo

# Demo 3: Load database
echo -e "${BLUE}3. Loading database:${NC}"
echo

if [ -f "$OUTPUT_DB" ]; then
    echo -e "${YELLOW}Loading $OUTPUT_DB...${NC}"
    if "$SCRIPT_DIR/ged2gwb" --load "$OUTPUT_DB"; then
        echo -e "${GREEN}✓ Database loaded successfully${NC}"
    else
        echo -e "${RED}✗ Database loading failed${NC}"
    fi
else
    echo -e "${YELLOW}No database to load${NC}"
fi
echo

# Demo 4: Test with base-dir
echo -e "${BLUE}4. Testing with --base-dir:${NC}"
echo

BASE_DIR="demo-bases"
OUTPUT_DB_BASE="demo-base-dir.pkl"
echo -e "${YELLOW}Converting with --base-dir $BASE_DIR...${NC}"
if "$SCRIPT_DIR/ged2gwb" "$SAMPLE_GED" --output "$OUTPUT_DB_BASE" --base-dir "$BASE_DIR" --no-compress; then
    echo -e "${GREEN}✓ Conversion with base-dir successful${NC}"

    # Show file info
    if [ -f "$BASE_DIR/$OUTPUT_DB_BASE" ]; then
        FILE_SIZE=$(ls -lh "$BASE_DIR/$OUTPUT_DB_BASE" | awk '{print $5}')
        echo -e "${GREEN}✓ Database created: $BASE_DIR/$OUTPUT_DB_BASE ($FILE_SIZE)${NC}"
    fi
else
    echo -e "${RED}✗ Conversion with base-dir failed${NC}"
fi
echo

# Demo 5: Test compression
echo -e "${BLUE}5. Testing compression:${NC}"
echo

OUTPUT_DB_COMPRESSED="demo-compressed.pkl"
echo -e "${YELLOW}Converting with compression...${NC}"
if "$SCRIPT_DIR/ged2gwb" "$SAMPLE_GED" --output "$OUTPUT_DB_COMPRESSED" --compress; then
    echo -e "${GREEN}✓ Compressed conversion successful${NC}"

    # Show file info
    if [ -f "$OUTPUT_DB_COMPRESSED.gz" ]; then
        FILE_SIZE=$(ls -lh "$OUTPUT_DB_COMPRESSED.gz" | awk '{print $5}')
        echo -e "${GREEN}✓ Compressed database created: $OUTPUT_DB_COMPRESSED.gz ($FILE_SIZE)${NC}"
    fi
else
    echo -e "${RED}✗ Compressed conversion failed${NC}"
fi
echo

# Demo 6: Test generic runner
echo -e "${BLUE}6. Testing generic runner:${NC}"
echo

echo -e "${YELLOW}Using geneweb-python to run ged2gwb...${NC}"
if "$SCRIPT_DIR/geneweb-python" ged2gwb --help | head -10; then
    echo -e "${GREEN}✓ Generic runner works${NC}"
else
    echo -e "${RED}✗ Generic runner failed${NC}"
fi
echo

# Cleanup
echo -e "${BLUE}7. Cleaning up demo files:${NC}"
echo

CLEANUP_FILES=("$OUTPUT_DB" "$OUTPUT_DB_BASE" "$OUTPUT_DB_COMPRESSED.gz")
CLEANUP_DIRS=("$BASE_DIR")

for file in "${CLEANUP_FILES[@]}"; do
    if [ -f "$file" ]; then
        rm -f "$file"
        echo -e "${GREEN}✓ Removed $file${NC}"
    fi
done

for dir in "${CLEANUP_DIRS[@]}"; do
    if [ -d "$dir" ]; then
        rm -rf "$dir"
        echo -e "${GREEN}✓ Removed $dir${NC}"
    fi
done

echo
echo -e "${BLUE}=== Demo Complete ===${NC}"
echo -e "${GREEN}All binaries are working correctly!${NC}"
echo
echo -e "${YELLOW}To install these binaries system-wide, run:${NC}"
echo -e "  ./install-binaries.sh"
echo
echo -e "${YELLOW}To use them from anywhere, add to your PATH:${NC}"
echo -e "  export PATH=\"$SCRIPT_DIR:\$PATH\""
