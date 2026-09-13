# Focused Pascal regression checks

## Copy-magnet response and clipboard handling

From the repository root, with Python 3 and Free Pascal (including its FCL units) installed:

```sh
python3 -m unittest discover -s tests -p test_copy_magnet_runner.py -v
python3 tests/test_copy_magnet.py
```

The runner extracts `TMainForm.MenuItem101Click`, `CheckClipboardLink`, and the
production clipboard-reading and link-normalization routines directly from
`main.pas`. It compiles those implementations with real FPC `Classes`, `Variants`,
and `fpjson` units. Only external form, RPC, status, clipboard, and hash-detection
collaborators are replaced; the fixtures exercised here are already `magnet:` URIs,
so hash-to-magnet conversion is deliberately outside this focused test.

Extraction follows the repository's column-one top-level declaration style. It
recognizes procedures, functions, constructors, destructors, class methods,
operators, and unit-ending sections, while ignoring declarations inside comments
and strings. Missing, duplicate, unfinished, or unsupported boundaries fail
explicitly. The Python checks exercise these boundaries and indented local
routines without requiring a Pascal compiler; this is not a general Pascal parser.

The Pascal checks cover malformed responses, no selection, nil responses,
request shape, output order and line endings, empty arrays/strings, exception
identity, pre-write marker visibility, recovery reads, and retry on the same
objects. After a confirmed partial write they run the real clipboard monitor and
assert that the application's own magnet is not queued for import. A different
external magnet remains eligible for monitoring and is never overwritten.

Assertions, range/overflow/I/O checks, and heap tracing are enabled. A reported
memory leak fails the runner. Compiler output stays in a temporary directory and
is removed after the run.

To test another main-unit revision without changing the working tree:

```sh
git show HEAD~1:main.pas > /tmp/transgui-main-before.pas
python3 tests/test_copy_magnet.py --source /tmp/transgui-main-before.pas
```

These are isolated tests, not native GUI/backend fault-injection tests: they do
not contact a daemon or access the system clipboard. On a write exception, a
successful recovery read confirming the requested text keeps the new marker;
a different value, unavailable text format, or failed read restores the previous
marker. The original write exception is re-raised in every case, including when
the recovery read raises internally. No recovery writes are attempted.

When read-back is unavailable, the application cannot determine whether the OS
clipboard changed. In that case the fallback restores the old marker, and a
later activation may detect the copied text. Silent setter failures are also
outside this repair's guarantee. Native Windows, Linux, and macOS clipboard
behavior still requires platform testing.

The path-filtered `Copy-magnet regression` workflow runs both checks with read-only
repository permissions. Checkout is pinned to a full upstream commit SHA, and a
workflow/ref concurrency group cancels obsolete runs without cancelling unrelated
workflows. Packaging and release workflows are unchanged.
