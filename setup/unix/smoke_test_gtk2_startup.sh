#!/bin/bash

set -euo pipefail

ROOT="$(cd "$(dirname "$(realpath "${BASH_SOURCE[0]}")")/../../" && pwd)"

if [ "$#" -gt 1 ]; then
  echo "Usage: $0 [transgui-binary]" >&2
  exit 2
fi

transgui_binary="${1:-$ROOT/transgui}"
if [[ "$transgui_binary" != */* ]]; then
  transgui_binary="./$transgui_binary"
fi
if [ ! -f "$transgui_binary" ] || [ ! -x "$transgui_binary" ]; then
  echo "Transgui binary is not executable: $transgui_binary" >&2
  exit 1
fi
transgui_binary="$(realpath "$transgui_binary")"
transgui_ini="$(basename "$transgui_binary")"
transgui_ini="${transgui_ini%.*}.ini"

runtime_home="$(mktemp -d)"
startup_log="$runtime_home/startup.log"
wm_log="$runtime_home/wm.log"
xvfb_log="$runtime_home/xvfb.log"
clipboard_log="$runtime_home/clipboard.log"
clipboard_image="$runtime_home/clipboard.png"
export HOME="$runtime_home"

printf "%s\n" \
  "[MainForm]" \
  "FirstRun=0" \
  "" \
  "[Interface]" \
  "LinksFromClipboard=1" \
  "" \
  "[Connection]" \
  "Host=127.0.0.1" \
  "Port=1" \
  "Autoreconnect=1" \
  > "$runtime_home/$transgui_ini"
printf "%s\n" "RandomPlacement" > "$runtime_home/.twmrc"

xvfb_pid=""
wm_pid=""
clipboard_pid=""
app_pid=""

process_alive() {
  local pid="$1"
  local stat_line

  if ! kill -0 "$pid" 2> /dev/null; then
    return 1
  fi
  if ! IFS= read -r stat_line < "/proc/$pid/stat"; then
    return 1
  fi

  stat_line="${stat_line##*) }"
  [ "${stat_line%% *}" != "Z" ]
}

stop_process() {
  local pid="$1"
  local name="$2"
  local attempt

  if [ -z "$pid" ]; then
    return
  fi
  if ! process_alive "$pid"; then
    wait "$pid" 2> /dev/null || true
    return
  fi

  kill "$pid" 2> /dev/null || true
  for ((attempt = 0; attempt < 20; attempt++)); do
    if ! process_alive "$pid"; then
      wait "$pid" 2> /dev/null || true
      return
    fi
    sleep 0.1
  done

  kill -KILL "$pid" 2> /dev/null || true
  for ((attempt = 0; attempt < 20; attempt++)); do
    if ! process_alive "$pid"; then
      wait "$pid" 2> /dev/null || true
      return
    fi
    sleep 0.1
  done

  echo "$name did not exit after SIGKILL" >&2
}

cleanup() {
  exit_status=$?
  trap - EXIT
  set +e
  stop_process "$app_pid" "transgui"
  stop_process "$clipboard_pid" "xclip"
  stop_process "$wm_pid" "twm"
  stop_process "$xvfb_pid" "Xvfb"
  rm -rf "$runtime_home"
  exit "$exit_status"
}
trap cleanup EXIT

Xvfb :99 -screen 0 1280x800x24 > "$xvfb_log" 2>&1 &
xvfb_pid=$!

display_ready=0
for _ in $(seq 1 100); do
  if ! process_alive "$xvfb_pid"; then
    echo "Xvfb exited during startup" >&2
    cat "$xvfb_log" >&2
    exit 1
  fi
  if DISPLAY=:99 xdotool getmouselocation > /dev/null 2>&1; then
    display_ready=1
    break
  fi
  sleep 0.1
done

if [ "$display_ready" -ne 1 ]; then
  echo "Xvfb did not become ready" >&2
  cat "$xvfb_log" >&2
  exit 1
fi

DISPLAY=:99 twm > "$wm_log" 2>&1 &
wm_pid=$!
sleep 0.5
if ! process_alive "$wm_pid"; then
  echo "twm exited during startup" >&2
  cat "$wm_log" >&2
  exit 1
fi

base64 --decode > "$clipboard_image" << 'PNG'
iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII=
PNG
DISPLAY=:99 xclip -selection clipboard -target image/png -loops 0 \
  -verbose "$clipboard_image" > "$clipboard_log" 2>&1 &
clipboard_pid=$!
sleep 0.5
if ! process_alive "$clipboard_pid"; then
  echo "xclip exited before transgui startup" >&2
  cat "$clipboard_log" >&2
  exit 1
fi

DISPLAY=:99 G_DEBUG=fatal-criticals LIBOVERLAY_SCROLLBAR=0 \
  "$transgui_binary" --home="$runtime_home" > "$startup_log" 2>&1 &
app_pid=$!

window_found=0
for _ in $(seq 1 20); do
  if ! process_alive "$wm_pid"; then
    echo "twm exited during startup" >&2
    cat "$wm_log" >&2
    exit 1
  fi
  if ! process_alive "$app_pid"; then
    echo "transgui exited during startup" >&2
    cat "$startup_log" >&2
    exit 1
  fi
  if DISPLAY=:99 timeout 1s xdotool search --onlyvisible \
    --name "^Transmission Remote GUI v[0-9]" > /dev/null 2>&1; then
    window_found=1
    break
  fi
  sleep 0.5
done

if [ "$window_found" -ne 1 ]; then
  echo "No visible transgui main window appeared" >&2
  cat "$startup_log" >&2
  cat "$wm_log" >&2
  exit 1
fi

sleep 4
if ! process_alive "$app_pid"; then
  echo "transgui exited during startup" >&2
  cat "$startup_log" >&2
  exit 1
fi
if ! process_alive "$clipboard_pid"; then
  echo "The image-only clipboard owner exited during startup" >&2
  cat "$clipboard_log" >&2
  cat "$startup_log" >&2
  exit 1
fi

if ! DISPLAY=:99 timeout 1s xdotool search --onlyvisible \
  --name "^Transmission Remote GUI v[0-9]" > /dev/null 2>&1; then
  echo "The transgui main window disappeared during startup" >&2
  cat "$startup_log" >&2
  cat "$wm_log" >&2
  exit 1
fi

if visible_windows="$(DISPLAY=:99 timeout 1s xdotool search \
  --onlyvisible --class Transgui)"; then
  visible_window_count="$(wc -w <<< "$visible_windows")"
else
  search_status=$?
  echo "Unable to inspect visible transgui windows (status $search_status)" >&2
  cat "$startup_log" >&2
  cat "$wm_log" >&2
  exit 1
fi
if [ "$visible_window_count" -ne 1 ]; then
  echo "Unexpected visible transgui window count: $visible_window_count" >&2
  for window_id in $visible_windows; do
    printf "%s: " "$window_id" >&2
    DISPLAY=:99 xdotool getwindowname "$window_id" >&2 || true
  done
  cat "$startup_log" >&2
  cat "$wm_log" >&2
  exit 1
fi

if [ -s "$startup_log" ]; then
  echo "Unexpected GTK2 startup output:" >&2
  cat "$startup_log" >&2
  exit 1
fi
