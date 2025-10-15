#!/bin/bash
#
# Install GeneWeb Python Binaries
# This script adds the bin directory to PATH and creates symlinks
#

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BIN_DIR="$SCRIPT_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}Installing GeneWeb Python binaries...${NC}"

# Check if we're in the right directory
if [ ! -f "$BIN_DIR/ged2gwb" ]; then
    echo -e "${RED}Error: ged2gwb script not found in $BIN_DIR${NC}" >&2
    exit 1
fi

# Function to add to PATH in shell config
add_to_path() {
    local shell_config="$1"
    local path_line="export PATH=\"$BIN_DIR:\$PATH\""

    if [ -f "$shell_config" ]; then
        if ! grep -q "$BIN_DIR" "$shell_config"; then
            echo "" >> "$shell_config"
            echo "# GeneWeb Python binaries" >> "$shell_config"
            echo "$path_line" >> "$shell_config"
            echo -e "${GREEN}Added to $shell_config${NC}"
        else
            echo -e "${YELLOW}Already in $shell_config${NC}"
        fi
    fi
}

# Detect shell and add to appropriate config file
if [ -n "$ZSH_VERSION" ]; then
    # Zsh
    add_to_path "$HOME/.zshrc"
elif [ -n "$BASH_VERSION" ]; then
    # Bash
    add_to_path "$HOME/.bashrc"
    add_to_path "$HOME/.bash_profile"
fi

# Create symlinks in /usr/local/bin (requires sudo)
echo -e "${BLUE}Creating symlinks in /usr/local/bin...${NC}"
if command -v sudo >/dev/null 2>&1; then
    sudo ln -sf "$BIN_DIR/ged2gwb" /usr/local/bin/ged2gwb
    sudo ln -sf "$BIN_DIR/gedcom" /usr/local/bin/gedcom
    sudo ln -sf "$BIN_DIR/db-pickle" /usr/local/bin/db-pickle
    sudo ln -sf "$BIN_DIR/geneweb-python" /usr/local/bin/geneweb-python
    echo -e "${GREEN}Symlinks created successfully${NC}"
else
    echo -e "${YELLOW}sudo not available, skipping symlink creation${NC}"
fi

# Test the installation
echo -e "${BLUE}Testing installation...${NC}"
if "$BIN_DIR/ged2gwb" --help >/dev/null 2>&1; then
    echo -e "${GREEN}✓ ged2gwb is working${NC}"
else
    echo -e "${RED}✗ ged2gwb test failed${NC}"
fi

if "$BIN_DIR/gedcom" --help >/dev/null 2>&1; then
    echo -e "${GREEN}✓ gedcom is working${NC}"
else
    echo -e "${YELLOW}⚠ gedcom test failed (may not have CLI)${NC}"
fi

echo -e "${BLUE}Installation complete!${NC}"
echo -e "${YELLOW}Note: You may need to restart your terminal or run 'source ~/.bashrc' to use the binaries${NC}"
