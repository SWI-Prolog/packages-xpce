#!/bin/bash
#
# Run a command against a private, headless display for making the
# screenshots of the XPCE User Guide.
#
# Usage: shoot.sh command [arg ...]
#
# If mutter is installed, we start a headless Wayland session with a
# virtual monitor at scale 2, so the screenshots have twice the pixel
# density of the logical coordinates.  Otherwise we fall back to Xvfb
# at scale 1.  The session uses its own D-Bus, configuration directory
# and in-memory GSettings, so it does not touch the user's desktop.

# The private runtime and configuration directories must be set before
# starting the D-Bus session, so services it activates (such as the
# document portal) do not use those of the user.

if [ -z "$SHOOT_TMP" ] && command -v mutter >/dev/null; then
  SHOOT_TMP=$(mktemp -d)
  trap 'rm -rf "$SHOOT_TMP"' EXIT
  mkdir -m 700 "$SHOOT_TMP/run" "$SHOOT_TMP/config"
  export SHOOT_TMP
  export XDG_RUNTIME_DIR="$SHOOT_TMP/run" XDG_CONFIG_HOME="$SHOOT_TMP/config"
  export GSETTINGS_BACKEND=memory NO_AT_BRIDGE=1
  unset DISPLAY WAYLAND_DISPLAY DBUS_SESSION_BUS_ADDRESS
  dbus-run-session -- "$0" "$@"
  exit $?
fi

if [ -n "$SHOOT_TMP" ]; then
  tmp="$SHOOT_TMP"
  trap 'kill $mutter 2>/dev/null' EXIT

  mutter --headless --wayland --no-x11 --wayland-display=shoot-0 \
	 --virtual-monitor 3200x2400 > "$tmp/mutter.log" 2>&1 &
  mutter=$!
  for i in $(seq 100); do
    [ -S "$XDG_RUNTIME_DIR/shoot-0" ] && break
    sleep 0.1
  done
  if [ ! -S "$XDG_RUNTIME_DIR/shoot-0" ]; then
    echo "shoot.sh: failed to start mutter:" >&2
    cat "$tmp/mutter.log" >&2
    exit 1
  fi

  dc="--session --dest org.gnome.Mutter.DisplayConfig
      --object-path /org/gnome/Mutter/DisplayConfig"
  serial=$(gdbus call $dc \
		 --method org.gnome.Mutter.DisplayConfig.GetCurrentState |
	   sed -n 's/^(uint32 \([0-9]*\),.*/\1/p')
  gdbus call $dc \
	--method org.gnome.Mutter.DisplayConfig.ApplyMonitorsConfig \
	"$serial" 1 \
	"[(0, 0, 2.0, 0, true, [('Meta-0', '3200x2400@60.000', @a{sv} {})])]" \
	"@a{sv} {}" > /dev/null || exit 1

  WAYLAND_DISPLAY=shoot-0 SDL_VIDEO_DRIVER=wayland "$@"
  exit $?
fi

# Without unsetting WAYLAND_DISPLAY, SDL would use the user's desktop
unset WAYLAND_DISPLAY
export SDL_VIDEO_DRIVER=x11
exec xvfb-run -a -s "-screen 0 1600x1200x24" "$@"
