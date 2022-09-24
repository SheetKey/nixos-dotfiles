local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

local theme = {}

theme.font          = "Ubuntu Nerd Font 8"

local base03  = "#002b36"
local base02  = "#073642"
local base01  = "#586e75"
local base00  = "#657b83"
local base0   = "#839496"
local base1   = "#93a1a1"
local base2   = "#eee8d5"
local base3   = "#fdf6e3"
local yellow  = "#b58900"
local orange  = "#cb4b16"
local red     = "#dc322f"
local magenta = "#d33682"
local violet  = "#6c71c4"
local blue    = "#268bd2"
local cyan    = "#2aa198"
local green   = "#859900"

theme.bg_normal     = base02
theme.bg_focus      = blue
theme.bg_urgent     = red
theme.bg_minimize   = base03
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = base00
theme.fg_focus      = base02
theme.fg_urgent     = base02
theme.fg_minimize   = base00

theme.useless_gap   = dpi(0)
theme.border_width  = dpi(1)
theme.border_normal = base03
theme.border_focus  = blue
theme.border_marked = red

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

-- Generate taglist squares:
local taglist_square_size = dpi(4)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
    taglist_square_size, theme.fg_normal
)

-- Variables set for theming notifications:
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path.."default/submenu.png"
theme.menu_height = dpi(15)
theme.menu_width  = dpi(100)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = themes_path.."default/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = themes_path.."default/titlebar/close_focus.png"

theme.titlebar_minimize_button_normal = themes_path.."default/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus  = themes_path.."default/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive = themes_path.."default/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = themes_path.."default/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = themes_path.."default/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = themes_path.."default/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = themes_path.."default/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = themes_path.."default/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = themes_path.."default/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = themes_path.."default/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = themes_path.."default/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = themes_path.."default/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = themes_path.."default/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = themes_path.."default/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = themes_path.."default/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = themes_path.."default/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = themes_path.."default/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = themes_path.."default/titlebar/maximized_focus_active.png"

-- theme.wallpaper = themes_path.."default/background.png"

-- You can use your own layout icons like this:
theme.layout_fairh = themes_path.."default/layouts/fairhw.png"
theme.layout_fairv = themes_path.."default/layouts/fairvw.png"
theme.layout_floating  = themes_path.."default/layouts/floatingw.png"
theme.layout_magnifier = themes_path.."default/layouts/magnifierw.png"
theme.layout_max = themes_path.."default/layouts/maxw.png"
theme.layout_fullscreen = themes_path.."default/layouts/fullscreenw.png"
theme.layout_tilebottom = themes_path.."default/layouts/tilebottomw.png"
theme.layout_tileleft   = themes_path.."default/layouts/tileleftw.png"
theme.layout_tile = themes_path.."default/layouts/tilew.png"
theme.layout_tiletop = themes_path.."default/layouts/tiletopw.png"
theme.layout_spiral  = themes_path.."default/layouts/spiralw.png"
theme.layout_dwindle = themes_path.."default/layouts/dwindlew.png"
theme.layout_cornernw = themes_path.."default/layouts/cornernww.png"
theme.layout_cornerne = themes_path.."default/layouts/cornernew.png"
theme.layout_cornersw = themes_path.."default/layouts/cornersww.png"
theme.layout_cornerse = themes_path.."default/layouts/cornersew.png"

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil


--local mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon })
--mylauncher:connect_signal("button::press", function() awful.util.mymainmenu:toggle() end)
--
--theme.mylaucher = mylauncher
--theme.mykeyboardlayout = mykerboardlayout
--theme.mytextclock = mytextclock
--
---- Keyboard map indicator and switcher
--local mykeyboardlayout = awful.widget.keyboardlayout()
--
--
---- Create a textclock widget
--local mytextclock = wibox.widget.textclock('%a %b %d, %I:%M%P')
--
--
--function theme.at_screen_connect(s)
--    -- Each screen has its own tag table.
--    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])
--
--    -- Create a promptbox for each screen
--    s.mypromptbox = awful.widget.prompt()
--    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
--    -- We need one layoutbox per screen.
--    s.mylayoutbox = awful.widget.layoutbox(s)
--    s.mylayoutbox:buttons(gears.table.join(
--                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
--                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
--                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
--                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
--
--    -- Create a taglist widget
--    s.mytaglist = awful.widget.taglist {
--        screen  = s,
--        filter  = awful.widget.taglist.filter.all,
--        buttons = awful.util.taglist_buttons
--    }
--
--    -- Create a tasklist widget
--    s.mytasklist = awful.widget.tasklist {
--        screen  = s,
--        filter  = awful.widget.tasklist.filter.currenttags,
--        buttons = awful.util.tasklist_buttons
--    }
--
--    -- Create the wibox
--    s.mywibox = awful.wibar({ position = "top", screen = s })
--
--    -- Add widgets to the wibox
--    s.mywibox:setup {
--        layout = wibox.layout.align.horizontal,
--        { -- Left widgets
--            layout = wibox.layout.fixed.horizontal,
--            mylauncher,
--            s.mytaglist,
--            s.mypromptbox,
--        },
--        s.mytasklist, -- Middle widget
--        { -- Right widgets
--            layout = wibox.layout.fixed.horizontal,
--            mykeyboardlayout,
--            mytextclock,
--            wibox.widget.systray(),
--            s.mylayoutbox,
--        },
--    }
--end

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
