#!/usr/bin/env python3
"""Run DAO analysis commands from any working directory."""
import sys
sys.dont_write_bytecode = True
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent / "src/python"))
from dao_bias.cli import main

if __name__ == "__main__":
    raise SystemExit(main())
