#!/usr/bin/env python3
"""Main entry point for consang command-line tool."""

import sys
from .cli import ConsangCLI


def main():
    """Main entry point."""
    cli = ConsangCLI()
    cli.main()


if __name__ == "__main__":
    main()
