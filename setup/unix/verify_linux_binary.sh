#!/bin/bash

set -e

binary="${1:-./transgui}"
expected_widgetset="${2:-${LCL_WIDGETSET:-gtk2}}"

if [ ! -x "$binary" ]; then
  echo "Binary not found or not executable: $binary" >&2
  exit 2
fi

if ! command -v readelf > /dev/null 2>&1; then
  echo "readelf not found; install binutils to inspect shared libraries." >&2
  exit 3
fi

needed_libs="$(
  readelf -d "$binary" |
    awk '/\(NEEDED\)/ {
      sub(/^.*\[/, "")
      sub(/\].*$/, "")
      print
    }'
)"

if [ -z "$needed_libs" ]; then
  echo "Unable to inspect shared libraries for: $binary" >&2
  exit 4
fi

echo "Declared GTK/GDK linkage:"
grep -E 'gtk|gdk|pango|cairo|fontconfig|X11' <<< "$needed_libs" || true

if [ "${VERIFY_LINUX_RUNTIME_LIBS:-0}" = "1" ]; then
  if command -v ldd > /dev/null 2>&1; then
    ldd_status=0
    ldd_output="$(ldd "$binary" 2>&1)" || ldd_status=$?
    missing="$(awk '/=> not found/ {print $1}' <<< "$ldd_output")"

    if [ -n "$missing" ]; then
      echo "Missing shared libraries:" >&2
      echo "$missing" >&2

      case "$expected_widgetset" in
        gtk2)
          echo "For Debian/Ubuntu GTK2 runtime, try: sudo apt install libgtk2.0-0" >&2
          ;;
        gtk3)
          echo "For Debian/Ubuntu GTK3 runtime, try: sudo apt install libgtk-3-0" >&2
          ;;
      esac

      exit 4
    fi

    if [ "$ldd_status" -ne 0 ]; then
      echo "ldd could not inspect runtime libraries; continuing with ELF linkage data." >&2
      echo "$ldd_output" >&2
    fi
  else
    echo "ldd not found; skipping runtime missing-library check." >&2
  fi
else
  echo "Runtime missing-library check disabled; set VERIFY_LINUX_RUNTIME_LIBS=1 for trusted builds." >&2
fi

case "$expected_widgetset" in
  gtk2)
    if ! grep -Fq 'libgtk-x11-2.0.so.0' <<< "$needed_libs"; then
      echo "Expected GTK2 linkage, but libgtk-x11-2.0.so.0 was not found." >&2
      exit 5
    fi
    if grep -Fq 'libgtk-3.so.0' <<< "$needed_libs"; then
      echo "Expected GTK2 linkage, but GTK3 linkage was also found." >&2
      exit 5
    fi
    ;;
  gtk3)
    if ! grep -Fq 'libgtk-3.so.0' <<< "$needed_libs"; then
      echo "Expected GTK3 linkage, but libgtk-3.so.0 was not found." >&2
      exit 6
    fi
    if grep -Fq 'libgtk-x11-2.0.so.0' <<< "$needed_libs"; then
      echo "Expected GTK3 linkage, but GTK2 linkage was also found." >&2
      exit 6
    fi
    ;;
  none)
    echo "No expected widgetset enforcement requested; linkage check only."
    ;;
  *)
    echo "Unsupported expected widgetset: $expected_widgetset" >&2
    exit 7
    ;;
esac

if command -v objdump > /dev/null 2>&1; then
  echo "Highest GLIBC requirement:"
  objdump -T "$binary" |
    grep -o 'GLIBC_[0-9][0-9.]*' |
    awk '
      function version_gt(a, b,    av, bv, i, n) {
        n = split(a, av, ".")
        split(b, bv, ".")
        for (i = 1; i <= n || i in bv; i++) {
          if ((av[i] + 0) > (bv[i] + 0)) return 1
          if ((av[i] + 0) < (bv[i] + 0)) return 0
        }
        return 0
      }
      {
        version = substr($0, 7)
        if (max_version == "" || version_gt(version, max_version)) {
          max_version = version
        }
      }
      END {
        if (max_version != "") print "GLIBC_" max_version
      }
    ' || true
else
  echo "objdump not found; install binutils to inspect GLIBC requirements." >&2
fi

echo "Binary verification passed."
