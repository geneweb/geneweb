#!/usr/bin/env python3
# -*- coding: utf-8 -*-


# python3 ../gw/ged2gwb_dir $@

# import ged2gwb_dir.cli.main as main_cli
import sys
from ged2gwb_dir.cli.main import main

if __name__ == "__main__":
    main(sys.argv[1:] + ["--verbose"])
