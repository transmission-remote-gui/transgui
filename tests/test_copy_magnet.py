#!/usr/bin/env python3
"""Compile the production copy-magnet and clipboard-monitoring routines in isolation."""

import argparse
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile


# Preserve offsets/newlines while hiding declarations inside comments or strings.
_PASCAL_LITERALS = re.compile(r"'[^']*(?:''[^']*)*'|//[^\n]*|\{.*?\}|\(\*.*?\*\)", re.DOTALL)
_BOUNDARY = re.compile(
    r"^(?:(?:class[ \t]+)?(?:procedure|function|constructor|destructor|operator)\b"
    r"|initialization\b|finalization\b|end[ \t]*\.)", re.MULTILINE | re.IGNORECASE,
)


def extract_routine(source: str, name: str) -> str:
    """Extract one column-one Pascal implementation, rejecting ambiguous boundaries."""
    masked = _PASCAL_LITERALS.sub(
        lambda match: re.sub(r"[^\n]", " ", match.group()), source,
    )
    declaration = re.compile(
        r"^(?:class[ \t]+)?(?:procedure|function|constructor|destructor|operator)"
        r"[ \t]+" + re.escape(name) + r"(?=[ \t]*(?:[(:;]|$))",
        re.MULTILINE | re.IGNORECASE,
    )
    matches = list(declaration.finditer(masked))
    if len(matches) != 1:
        raise ValueError(f"expected exactly one implementation of {name}")
    start = matches[0]
    following = _BOUNDARY.search(masked, start.end())
    if following is None:
        raise ValueError(f"missing top-level boundary after {name}")
    if not re.search(r"\bend\s*;\s*\Z", masked[start.start():following.start()], re.IGNORECASE):
        raise ValueError(f"incomplete or unsupported implementation of {name}")
    return source[start.start():following.start()]


def main() -> int:
    """Build extracted routines, run regression checks, and report failures or leaks."""
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
    names = (
        "IsProtocolSupported", "HasIPCRecordDelimiter", "TryReadClipboardText",
        "TryNormalizeClipboardTorrentLink", "TMainForm.MenuItem101Click",
        "TMainForm.CheckClipboardLink",
    )
    try:
        routines = "\n".join(extract_routine(source, name) for name in names)
    except ValueError as error:
        parser.error(str(error))

    with tempfile.TemporaryDirectory(prefix="transgui-copy-magnet-") as directory:
        build = Path(directory)
        (build / "copy_magnet_handler.inc").write_text(routines, encoding="utf-8")
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
