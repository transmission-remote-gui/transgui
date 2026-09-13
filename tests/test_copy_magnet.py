#!/usr/bin/env python3
"""Compile and run the real copy-magnet handler with isolated RPC/clipboard doubles."""

import argparse
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--source", type=Path,
        default=Path(__file__).resolve().parents[1] / "main.pas",
        help="main.pas to test (defaults to the working tree)",
    )
    parser.add_argument("--fpc", default="fpc", help="Free Pascal compiler executable")
    args = parser.parse_args()
    compiler = shutil.which(args.fpc)
    if compiler is None:
        parser.error("Free Pascal was not found; install FPC with its FCL units")

    source = args.source.read_text(encoding="utf-8")
    handlers = re.findall(
        r"^procedure TMainForm\.MenuItem101Click\(Sender: TObject\);\n"
        r".*?(?=^procedure TMainForm\.)",
        source, re.MULTILINE | re.DOTALL,
    )
    if len(handlers) != 1:
        parser.error("expected exactly one complete MenuItem101Click implementation")

    with tempfile.TemporaryDirectory(prefix="transgui-copy-magnet-") as directory:
        build = Path(directory)
        (build / "copy_magnet_handler.inc").write_text(handlers[0], encoding="utf-8")
        executable = build / ("copy_magnet_test.exe" if os.name == "nt" else "copy_magnet_test")
        subprocess.run(
            [compiler, "-B", "-Sa", "-Cr", "-Co", "-Ci", "-gl", "-gh",
             "-Fi" + str(build), "-FU" + str(build), "-o" + str(executable),
             str(Path(__file__).resolve().with_name("copy_magnet_test.pas"))],
            cwd=build, check=True,
        )
        result = subprocess.run(
            [str(executable)], cwd=build, capture_output=True, text=True, check=False,
        )
        print(result.stdout, end="")
        print(result.stderr, end="", file=sys.stderr)
        if result.returncode:
            return result.returncode
        leaks = re.findall(r"(\d+) unfreed memory blocks", result.stdout + result.stderr)
        if any(int(count) for count in leaks):
            print("copy-magnet test leaked memory", file=sys.stderr)
            return 1
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, subprocess.CalledProcessError) as error:
        print(error, file=sys.stderr)
        sys.exit(1)
