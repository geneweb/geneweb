#!/bin/bash
#
# Uninstall GeneWeb Python Binaries
# This script removes the binaries from PATH and removes symlinks
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

echo -e "${BLUE}Uninstalling GeneWeb Python binaries...${NC}"

# Function to remove from PATH in shell config
remove_from_path() {
    local shell_config="$1"

    if [ -f "$shell_config" ]; then
        # Create a backup
        cp "$shell_config" "$shell_config.backup.$(date +%Y%m%d_%H%M%S)"

        # Remove lines containing the bin directory
        grep -v "$BIN_DIR" "$shell_config" > "$shell_config.tmp"
        mv "$shell_config.tmp" "$shell_config"

        echo -e "${GREEN}Removed from $shell_config${NC}"
    fi
}

# Detect shell and remove from appropriate config file
if [ -n "$ZSH_VERSION" ]; then
    # Zsh
    remove_from_path "$HOME/.zshrc"
elif [ -n "$BASH_VERSION" ]; then
    # Bash
    remove_from_path "$HOME/.bashrc"
    remove_from_path "$HOME/.bash_profile"
fi

# Remove symlinks from /usr/local/bin
echo -e "${BLUE}Removing symlinks from /usr/local/bin...${NC}"
if command -v sudo >/dev/null 2>&1; then
    sudo rm -f /usr/local/bin/ged2gwb
    sudo rm -f /usr/local/bin/gedcom
    sudo rm -f /usr/local/bin/db-pickle
    sudo rm -f /usr/local/bin/geneweb-python
    echo -e "${GREEN}Symlinks removed successfully${NC}"
else
    echo -e "${YELLOW}sudo not available, skipping symlink removal${NC}"
fi

echo -e "${BLUE}Uninstallation complete!${NC}"
echo -e "${YELLOW}Note: You may need to restart your terminal for changes to take effect${NC}"
