# GeneWeb Python Binaries

This directory contains bash wrapper scripts for all Python modules in the GeneWeb project.

## Available Binaries

### `ged2gwb`

GEDCOM to GeneWeb converter - the main binary for converting GEDCOM files to pickle databases.

```bash
# Convert a GEDCOM file
./bin/ged2gwb input.ged --output database.pkl

# Load a database
./bin/ged2gwb --load database.pkl

# Show help
./bin/ged2gwb --help
```

### `gedcom`

GEDCOM parser and utilities - for parsing and validating GEDCOM files.

```bash
# Parse a GEDCOM file
./bin/gedcom input.ged

# Show help
./bin/gedcom --help
```

### `db-pickle`

Pickle database manager - for managing pickle databases.

```bash
# Load a pickle database
./bin/db-pickle database.pkl

# Show help
./bin/db-pickle --help
```

### `geneweb-python`

Generic runner for any Python module in the project.

```bash
# Run any module
./bin/geneweb-python ged2gwb --help
./bin/geneweb-python gedcom --version
./bin/geneweb-python lib.db_pickle
```

## Installation

### Automatic Installation

Run the installation script to add all binaries to your PATH:

```bash
cd src/python
./bin/install-binaries.sh
```

This will:

- Add the `bin` directory to your shell's PATH
- Create symlinks in `/usr/local/bin` (requires sudo)
- Test the installation

### Manual Installation

Add the bin directory to your PATH manually:

```bash
# Add to your shell config file (~/.bashrc, ~/.zshrc, etc.)
export PATH="/path/to/geneweb/src/python/bin:$PATH"
```

## Usage

After installation, you can use the binaries from anywhere:

```bash
# Convert GEDCOM to pickle
ged2gwb sample.ged --output sample.pkl

# Load database
ged2gwb --load sample.pkl

# Use other modules
gedcom --help
db-pickle --help
```

## Uninstallation

To remove the binaries from your system:

```bash
cd src/python
./bin/uninstall-binaries.sh
```

This will:

- Remove the bin directory from your shell's PATH
- Remove symlinks from `/usr/local/bin`
- Create backups of your shell config files

## Features

- **Automatic virtual environment activation**: Scripts automatically activate the project's virtual environment
- **Cross-platform compatibility**: Works on Linux, macOS, and Windows (with bash)
- **Error handling**: Graceful fallback to system Python if virtual environment is not found
- **Easy installation**: One-command setup for all binaries
- **Clean uninstallation**: Complete removal of all traces

## Troubleshooting

### Virtual Environment Not Found

If you see "Virtual environment not found" warnings, make sure to create the virtual environment first:

```bash
cd src/python
make venv
make install-dev
```

### Permission Denied

If you get permission denied errors, make sure the scripts are executable:

```bash
chmod +x bin/*
```

### Command Not Found

If commands are not found after installation, restart your terminal or run:

```bash
source ~/.bashrc  # or ~/.zshrc
```

## Development

These scripts are designed to work with the project's development workflow:

- They automatically set up the Python path
- They activate the virtual environment if available
- They pass all arguments to the underlying Python modules
- They maintain compatibility with the project's Makefile targets
