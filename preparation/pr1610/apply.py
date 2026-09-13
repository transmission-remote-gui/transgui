#!/usr/bin/env python3
"""Apply the reviewed PR 1610 follow-up to either the old PR head or master."""

import argparse
from pathlib import Path
import shutil
import subprocess


def patch_main(source: bytes, mode: str) -> bytes:
    """Patch only MenuItem101Click and reject unexpected source shapes."""
    start = source.index(b"procedure TMainForm.MenuItem101Click(Sender: TObject);")
    end = source.index(b"\nprocedure TMainForm.", start + 1)
    handler = source[start:end]
    if mode == "review":
        old_decl = b"  ClipboardText, PreviousClipboardLink: string;\n"
        new_decl = b"  ClipboardText, PreviousClipboardLink, ActualClipboardText: string;\n"
        old_body = b"""    except
      FLastClipboardLink:=PreviousClipboardLink;
      raise;
    end;
"""
    else:
        old_decl = b"  MagnetLink: TJSONString;\n"
        new_decl = old_decl + b"  ClipboardText, PreviousClipboardLink, ActualClipboardText: string;\n"
        old_body = b"""    FLastClipboardLink := Magnets.Text;   // To Avoid TransGUI detect again this existing links
    Clipboard.AsText := Magnets.Text;
"""
    new_body = b"""    ClipboardText:=Magnets.Text;
    PreviousClipboardLink:=FLastClipboardLink;
    // Suppress detection of our own links during the clipboard write.
    FLastClipboardLink:=ClipboardText;
    try
      Clipboard.AsText:=ClipboardText;
    except
      // A backend may commit the text before raising; keep suppressing our links.
      if not TryReadClipboardText(ActualClipboardText) or
         (ActualClipboardText <> ClipboardText) then
        FLastClipboardLink:=PreviousClipboardLink;
      raise;
    end;
"""
    if handler.count(old_decl) != 1 or handler.count(old_body) != 1:
        raise ValueError("MenuItem101Click differs from the reviewed source shape")
    handler = handler.replace(old_decl, new_decl).replace(old_body, new_body)
    return source[:start] + handler + source[end:]


def main() -> None:
    """Apply source and retained test/workflow files without committing them."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", type=Path, default=Path("."))
    parser.add_argument("--mode", choices=("review", "master"), required=True)
    parser.add_argument("--payload", type=Path)
    args = parser.parse_args()
    repo = args.repo.resolve()
    payload = (args.payload or Path(__file__).resolve().parent / "payload").resolve()
    main_path = repo / "main.pas"
    main_path.write_bytes(patch_main(main_path.read_bytes(), args.mode))
    for source in payload.rglob("*"):
        if source.is_file():
            target = repo / source.relative_to(payload)
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(source, target)
    subprocess.run(["git", "diff", "--check"], cwd=repo, check=True)


if __name__ == "__main__":
    main()
