# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import os
from libqtile.config import Key, Screen, Group, Drag, Click, Match, DropDown, ScratchPad
from libqtile.command import lazy
from libqtile import layout, bar, widget, qtile
from libqtile.log_utils import logger
from libqtile.backend.wayland import InputConfig

try:
    from typing import List  # noqa: F401
except ImportError:
    pass

mod = "mod4"

keys = [
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "c", lazy.group.next_window()),
    Key([mod, "shift"], "c", lazy.group.prev_window()),
    Key([mod, "shift"], "h", lazy.layout.swap_left()),
    Key([mod, "shift"], "l", lazy.layout.swap_right()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    Key([mod], "i", lazy.layout.grow()),
    Key([mod], "m", lazy.layout.shrink()),
    Key([mod], "n", lazy.layout.normalize()),
    Key([mod], "o", lazy.layout.maximize()),
    Key([mod, "shift"], "space", lazy.layout.flip()),
    # Move windows up or down in current stack
    Key([mod, "control"], "k", lazy.layout.shuffle_down()),
    Key([mod, "control"], "j", lazy.layout.shuffle_up()),
    # Switch window focus to other pane(s) of stack
    Key([mod], "space", lazy.layout.next()),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod, "shift"], "Return", lazy.layout.toggle_split()),
    Key([mod], "Return", lazy.spawn("kitty")),
    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout()),
    Key([mod, "shift"], "q", lazy.window.kill()),
    Key([mod], "t", lazy.window.toggle_floating()),
    Key([mod, "control"], "r", lazy.reload_config()),
    Key([mod, "control"], "q", lazy.shutdown()),
    Key([mod], "F4", lazy.spawn("rofi-powermenu")),
    Key([mod, "control"], "s", lazy.group["scratchpad"].dropdown_toggle("term")),
    Key([mod, "control"], "e", lazy.group["scratchpad"].dropdown_toggle("emacs")),
]

groups = [
    Group("1", matches=[Match(wm_class=["firefox", "Google-chrome"])]),
    Group("2"),
    Group(
        "3",
        matches=[
            Match(wm_class=["jetbrains-idea"]),
        ],
    ),
    Group("4", matches=[Match(wm_class=["rambox"])]),
    Group("5", matches=[Match(wm_class=["SmartGit", "steam"])]),
    Group("6", matches=[Match(wm_class=["libreoffice"])]),
    Group("7"),
    Group("8"),
    Group("9", matches=[Match(wm_class=["KeePassXC"])]),
    Group(
        "0", matches=[Match(wm_class=["Google Play Music Desktop Player", "Spotify"])]
    ),
    ScratchPad(
        "scratchpad",
        [
            DropDown(
                "emacs",
                "emacs --name scratchpad",
                x=0.0,
                y=0.0,
                height=1.0,
                width=1.0,
                opacity=1.0,
            ),
            DropDown(
                "term",
                "kitty --name scratchpad",
                x=0.0,
                y=0.0,
                height=1.0,
                width=1.0,
                opacity=1.0,
            ),
        ],
    ),
]

lt_keys = {
    "1": "aogonek",
    "2": "ccaron",
    "3": "eogonek",
    "4": "eabovedot",
    "5": "iogonek",
    "6": "scaron",
    "7": "uogonek",
    "8": "umacron",
    "9": "doublelowquotemark",
    "0": "leftdoublequotemark",
}

for group_name in "1234567890":
    lt_key = lt_keys[group_name]
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key([mod], group_name, lazy.group[group_name].toscreen()),
            Key([mod], lt_key, lazy.group[group_name].toscreen()),
            # mod1 + shift + letter of group
            # = switch to & move focused window to group
            Key([mod, "shift"], group_name, lazy.window.togroup(group_name)),
            Key([mod, "shift"], lt_key, lazy.window.togroup(group_name)),
        ]
    )

layout_settings = {
    "border_focus": "#8BE9FD",
    "margin": 5,
    "single_border_width": 0,
    "single_margin": 0,
}

layouts = [
    layout.xmonad.MonadTall(**layout_settings),
    layout.xmonad.MonadWide(**layout_settings),
    layout.Max(),
]

widget_defaults = dict(
    font="sans",
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()


if qtile.core.name == "x11":
    keys.extend(
        [
            Key([mod], "d", lazy.spawn("rofi -show combi -combi-modi window,drun,run")),
            Key([mod, "shift"], "d", lazy.spawn("rofi -show run -sidebar-mode")),
        ]
    )
elif qtile.core.name == "wayland":
    keys.extend(
        [
            Key([mod], "d", lazy.spawn("rofi -show combi -combi-modi drun,run")),
            Key([mod, "shift"], "d", lazy.spawn("wofi --show run")),
        ]
    )
    os.environ["MOZ_ENABLE_WAYLAND"] = "1"
    os.environ["_JAVA_AWT_WM_NONREPARENTING"] = "1"
    os.environ["KITTY_CONF_FONT"] = "font_size 9"


pulse_step = 5
screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(hide_unused=True),
                widget.Prompt(),
                widget.WindowName(),
                widget.CurrentLayoutIcon(),
                widget.KeyboardLayout(
                    configured_keyboards=["lt", "us"], option="grp:caps_toggle"
                ),
                widget.Clock(format="%Y-%m-%d %a %H:%M %p"),
                widget.StatusNotifier(),
                widget.PulseVolume(step=pulse_step),
            ],
            24,
        ),
    ),
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(hide_unused=True),
                widget.Prompt(),
                widget.WindowName(),
                widget.CurrentLayoutIcon(),
                widget.KeyboardLayout(
                    configured_keyboards=["lt", "us"], option="grp:caps_toggle"
                ),
                widget.Clock(format="%Y-%m-%d %a %H:%M %p"),
                widget.PulseVolume(step=pulse_step),
            ],
            24,
        ),
    ),
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(hide_unused=True),
                widget.Prompt(),
                widget.WindowName(),
                widget.CurrentLayoutIcon(),
                widget.KeyboardLayout(
                    configured_keyboards=["lt", "us"], option="grp:caps_toggle"
                ),
                widget.Clock(format="%Y-%m-%d %a %H:%M %p"),
                widget.PulseVolume(step=pulse_step),
            ],
            24,
        ),
    ),
]


# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"

wl_input_rules = {
    "type:keyboard": InputConfig(kb_layout="lt,us",kb_options="grp:caps_toggle")
}

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, github issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
