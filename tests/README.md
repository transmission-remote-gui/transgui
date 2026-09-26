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
successful recovery read is used to determine whether the attempted write was
committed, resolve previously uncertain clipboard state, or preserve/restore the
appropriate existing marker. It does not unconditionally replace
`FLastClipboardLink` with the observed clipboard value. If read-back is
unavailable, the previous marker is restored only while the marker still equals
the attempted write; a marker updated by a reentrant callback is retained, and
clipboard state remains uncertain.

If write recovery cannot read the clipboard, the form records only that the
clipboard state is uncertain; it does not retain an unbounded or lossy window of
possible payloads. The next successful clipboard read synchronizes
`FLastClipboardLink` with the value actually observed and deliberately does not
queue that one value for automatic import. This guarantees that an application
payload which remained resident through arbitrarily many later failed writes is
not re-imported when clipboard reads eventually recover.

Clipboard callbacks can run before a setter has finished. While a clipboard
write is active, the monitor suppresses both the outgoing application payload and
the previously confirmed marker, preventing reentrant callbacks from re-importing
either the application's own new text or an unchanged pre-write magnet. Any other
clipboard value remains eligible for normal external-link processing.

If a callback changes the marker during a successful setter, the handler reads the
current clipboard before deciding which observation remains authoritative. If the
requested application text is still current, its marker is restored so it cannot
be self-imported. If a different value remains current, the callback marker is
preserved so an already-handled external link is not queued twice; a newer third
value remains eligible for the normal monitor path. If this reconciliation read
fails, the callback marker is retained and clipboard state remains uncertain until
the next successful read. Failed setters use the same observed-state principle,
always re-raise the original write exception, and never attempt a recovery write.

The bounded-state trade-off is explicit: while clipboard state is uncertain, an
external application may replace the clipboard before the next successful read.
That first successfully observed value is treated as the new baseline rather than
auto-imported, so one external clipboard event can be skipped in this exceptional
window. This favors avoiding duplicate/self-import after an indeterminate write
without allowing memory growth. Silent setter failures that return normally are
also outside this repair's guarantee. Native Windows, Linux, and macOS clipboard
behavior still requires platform testing.

The path-filtered `Copy-magnet regression` workflow runs both checks with read-only
repository permissions. Checkout is pinned to a full upstream commit SHA, and a
workflow/ref concurrency group cancels obsolete runs without cancelling unrelated
workflows. Packaging and release workflows are unchanged.
