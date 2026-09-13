# Focused Pascal regression checks

## Copy-magnet response and clipboard handling

From the repository root, with Python 3 and Free Pascal (including its FCL units) installed:

```sh
python3 tests/test_copy_magnet.py
```

The runner extracts `TMainForm.MenuItem101Click` directly from the working tree's
`main.pas`, compiles it with real `Classes`, `Variants`, and `fpjson` units, and
replaces only the surrounding form, RPC, status, and clipboard collaborators.
It does not maintain a second copy of the handler. A changed procedure boundary
fails extraction explicitly rather than silently running stale code.

Checks cover malformed responses, no selection, nil responses, request shape,
output order and line endings, empty arrays/strings, exception propagation,
clipboard-marker ordering, rollback after failed writes, and retry on the same
objects. Assertions, range/overflow/I/O checking, and heap tracing are enabled;
a reported memory leak fails the runner. Compiler output goes into a temporary
directory and is removed after the run.

To test another revision without changing the working tree:

```sh
git show HEAD~1:main.pas > /tmp/transgui-main-before.pas
python3 tests/test_copy_magnet.py --source /tmp/transgui-main-before.pas
```

This is an isolated handler test, not an end-to-end GUI test: it neither contacts
a daemon nor reads or writes the system clipboard. The clipboard double checks
that callbacks would see the new marker during a write, and can raise before or
after changing its text. The latter verifies that rollback touches only the
internal marker, not the operating system clipboard. Silent platform failures
that do not raise an exception are outside this handler's guarantee. Native
clipboard behavior still requires platform testing.

The `Copy-magnet regression` workflow runs these checks when the handler, tests,
or workflow changes. It does not modify packaging or release workflows.
