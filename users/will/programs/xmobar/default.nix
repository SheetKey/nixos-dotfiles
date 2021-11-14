# xmobar default.nix

{
  programs.xmobar = {
    enable = true;

    extraConfig = ''
      Config 
        { font = "xft:Ubuntu Nerd Font Mono:weight=bold:pixelsize=11:antialias=true:hinting=true"
	, additionalFonts = [ "xft:mononoki Nerd Font Mono:pixelsize=11:antialias=true:hinting=true"
	                    , "xft:Font Awesome 5 Free Solid:pixelsize=12"
			    , "xft:Font Awesome 5 Brands:pixelsize=12"
			    ]
	, bgColor = "#002b36"
	, fgColor = "#839496"
	, position = TopSize L 100 24
	, lowerOnStart = True
	, hideOnStart = False
	, persistent = True
	, iconRoot = "./xpm/" -- default: "."
	, commands =
	        [ 
		  Run Com "echo" ["<fn=3>\xf17c</fn>"] "penguin" 3600
		, Run Com ".local/bin/kernal" [] "kernel" 36000
		, Run Cpu ["-t", "<fn=2>\xf108</fn> cpu: (<total>%) ","-H","50","--high","red"] 20
		, Run Memory ["-t", "<fn=2>\xf233</fn> mem: <used>M (<usedratio>%)"] 20
		, Run DiskU [("/", "<fn=2>\xf0aa</fn> hdd: <free> free")] [] 60
		, Run Com "echo" ["<fn=2>\xf0aa</fn>"] "uparrow" 3600
		, Run Uptime ["-t", "uptime: <days>d <hours>h"] 360
		, Run Com "echo" ["<fn=2>\xf0f3</fn>"] "bell" 3600
		, Run Com "echo" ["<fn=2>\xf242</fn>"] "baticon" 3600
		, Run BatteryP ["BAT0"] ["-t", "<acstatus><watts> (<left<%)"] 360
		, Run Date "<fn=2>\xf017</fn> %b %d %Y - (%H:%M) " "date" 50
		, Run Com "./trayer-padding-icon.sh" [] "trayerpad" 20
		, Run UnsafeStdinReader
		]

	, sepChar = "%"
	, alignSep = "}{"
	, template = " <icon=haskell_20.xpm/>	<fc=#666666>|Hello</fc>"
        }
    '';
  };
}

#	, template = " <icon=haskell_20.xpm/>	<fc=#666666>|</fc> %UnsafeStdinReader% }{ <box type=Bottom width=2 mb=2 color=#dc322f><fc=#dc322f>%penguin%  <action=`alacritty -e htop`>%kernel%</action> </fc></box>    <box type=Bottom width=2 mb=2 color=#859900><fc=#859900><action=`alacritty -e htop`>%cpu%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#268bd2><fc=#268bd2><action=`alacritty -e htop`>%memory%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#b59800><fc=#b59800><action'`alacritty -e htop`>%disku%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#d33682><fc=#d33682>%uparrow%  <action=`alacritty -e htop`%uptime%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#dc322f><fc=#dc322f>%baticon%</fc></box>    <box type=Bottom width=2 mb=2 color=#286bd2><fc=#286bd2>%date%</fc></box> %trayerpad%"
