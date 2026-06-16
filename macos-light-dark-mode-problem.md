# Fix: live Light/Dark appearance switching on macOS

**Status:** deferred/on-hold, with a partial investigation recorded below so
the next attempt does not repeat dead ends. A new stopgap fix is committed that
freezes the palette so the app doesn't react to the system appearance change,
rather than the existing behaviour of a partial and unreadable halfway-house.

The launch-time appearance pin (`MacLocale.PinCurrentAppearance`, called from
`transgui.lpr`), which this work would eventually replace.

## What the pin does, and why it's a stopgap

Native macOS apps follow the system appearance live: flip System Settings →
Appearance and the UI recolours instantly. _transgui_ does not — flip the
appearance while it is running and the torrent/peer/file/tracker grids are left
half-light/half-dark until restart. Generally, stock widgets behave, but the
custom widgets don't.

`MacLocale.PinCurrentAppearance` (called from `transgui.lpr`) sidesteps this
by locking the app to its launch appearance so it is at least consistent.

The correct idiomatic behaviour for a Mac app would be to correctly and entirely
update when the system updates. However, this has proven harder than estimated.

## Investigation: the real root cause (read this first)

An initial, plausible-sounding theory was: "_transgui_ caches derived colours at
form-create time (e.g. `FAlterColor` in `main.pas`, the grid `AlternateColor`),
so just recompute them and force a repaint."

**This is wrong, and testing disproves it**:

- Scrolling, resizing, reconnecting, moving columns, toggling panes and going
  fullscreen all repaint the grids, yet the wrong colours persist. So a repaint
  is not the missing ingredient, and the stale value is not merely a cached
  transgui property.

- Recomputing `AlternateColor` live in `TVarGrid.PrepareCanvas` (tried) would
  derive it from the grid's `Color`, which resolves through the *same* stale
  path — so it does not help either.

The actual cause is one level down, in how the LCL Cocoa widgetset resolves
symbolic system colours to concrete RGB:

- _transgui_ uses `clWindow` / `clBtnFace` / `clHighlight` / `clWindowText` 
  for the grid backgrounds, the alternating-row shade, selection and text.

- On Cocoa these map to **dynamic `NSColor`s** — see
  `lcl/interfaces/cocoa/cocoautils.pas`, `TCocoaColorUtil.sysIndexToColor`:
  
  `COLOR_WINDOW → NSColor.textBackgroundColor`,
  `COLOR_BTNFACE → NSColor.controlBackgroundColor`,
  `COLOR_HIGHLIGHT → NSColor.selectedControlColor`, etc.

- A dynamic `NSColor` only yields the *current* appearance's RGB when it is
  evaluated inside a drawing context whose `NSAppearance` is current. The stock
  chrome (toolbar, headers, tabs, status bar) recolours correctly because AppKit
  draws it in that context. The custom grid converts the symbolic colour to an
  absolute `TColor`/RGB **outside** a current-appearance context, so it freezes
  at the launch appearance's values and no amount of repainting refreshes it.

In short: the problem is **frozen dynamic-NSColor resolution for the custom
grid**, not a _transgui_-level colour cache.

## What a proper implementation needs

1. **Resolve system colours live, in-context.** In the grid's paint path,
   obtain the background / alternate / text / selection colours by evaluating
   the corresponding dynamic `NSColor` (`textBackgroundColor`,
   `controlBackgroundColor`, `selectedControlColor`, `controlTextColor`, …)
   within the current `NSAppearance` drawing context, then convert to `TColor`
   for that draw — rather than reading a pre-resolved `clWindow` value. This is
   the crux and it couples `vargrid.pas` (or a small Cocoa helper it calls) to
   the Cocoa widgetset. Investigate whether wrapping the draw in
   `NSAppearance.performAsCurrentDrawingAppearance` (10.16+) or setting
   `NSAppearance.setCurrentAppearance` around the conversion is enough.

2. **Derive the alternate-row shade from that live base colour** (the existing
   `GetLikeColor(base, -$10)` rule), each paint, instead of the stored
   `AlternateColor`.

3. **Trigger a repaint on appearance change.** Register an `ICocoaThemeObserver`
   (`TCocoaThemeServices.addObserver`, fired from `doDarwinThemeChangedNotify`)
   or hook `AppleInterfaceThemeChangedNotification`, and `Invalidate` the main
   form and grids. Note the LCL already calls `UpdateThemes` /
   `Graphics.UpdateHandleObjects` / `IntfDoOnThemeChange` here — confirm what
   that does and does not refresh before adding more.

4. **Re-apply state-dependent backgrounds.** The grid `.Color` is set per
   connection state (`clWindow` when connected, `clBtnFace` when disconnected;
   see `DoDisconnect` and the connect path in `main.pas`). Any refresh must read
   the current state, not assume one.

5. **Disable the pin** (undo this commit) once live refresh works.

## Risks / unknowns

- The fix is concentrated in `vargrid.pas` custom drawing, which already carries
  an LCL-bug workaround in `PrepareCanvas` — delicate territory.
  
- The combinatorial test surface is large and hard to drive: **appearance change ×
  connection state × which of the 14 dialogs is open × transient UI state**. 
  
  A realistic plan tests the main window across connected/disconnected × light/dark
  and treats dialogs as recolouring on reopen. Any implementation must be
  safe-by-construction (no new crashes) even in untested corners, since it cannot
  be exhaustively verified.

- Saying that, fixing the most common cases would be better than the status
  quo, but the pin is a big improvement over a largely unreadable mixture,
  for the most part wholly acceptable, and if it's too annoying, resolving 
  just involves quickly restarting the app.

## Effort estimate

It's a larger effort than originally estimated; mainly due to the manual testing
that would be required and difficult to automate. The risk lives in the Cocoa
colour-resolution plumbing, not the LCL-level wiring, so it'd be fiddly and
add more platform-specific code. Because the launch-time pin already yields
a more usable app, the choice has been to just document the problem for
possible future effort.
