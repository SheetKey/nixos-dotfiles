# xmobar default.nix

let
  kernel = ./kernel;
  trayer-padding-icon = ./trayer-padding-icon.sh;

in
{
  programs.xmobar = {
    enable = true;

    extraConfig = ''
      Config 
        { font = "xft:Ubuntu Nerd Font:weight=bold:pixelsize=11:antialias=true:hinting=true"
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
	, commands =
	        [ 
		  Run Com "echo" ["<fn=3>\xf17c</fn>"] "penguin" 3600
	--	, Run Com ".config/xmobar/kernel" [] "kernel" 36000

		, Run Com "bash" ["~/.config/xmobar/kernel"] "kernel" 36000
	
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
	, template = " <icon=haskell_20.xpm/> %UnsafeStdinReader% }{ <box type=Bottom width=2 mb=2 color=#51afef><fc=#51afef>%penguin%  <action=`alacritty -e htop`>%kernel%</action> </fc></box>    <box type=Bottom width=2 mb=2 color=#ecbe7b><fc=#ecbe7b><action=`alacritty -e htop`>%cpu%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#ff6c6b><fc=#ff6c6b><action=`alacritty -e htop`>%memory%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#a9a1e1><fc=#a9a1e1><action=`alacritty -e htop`>%disku%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#98be65><fc=#98be65>%uparrow%  <action=`alacritty -e htop`>%uptime%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#46d9ff><fc=#46d9ff>%date%</fc></box> %trayerpad%"
	}
    '';


  };
  xdg.configFile."xmobar/kernel".text = ''
    #! /bin/bash

    kern="$(uname -r)"
    echo -e "$kern "
  '';
#  xdg.configFile."xmobar/trayer-padding-icon.sh".text = ''
#    #!/bin/sh
#    # Copied from https://github.com/jaor/xmobar/issues/239#issuecomment-233206552
#    # Detects the width of running trayer-srg window (xprop name 'panel')
#    # and creates an XPM icon of that width, 1px height, and transparent.
#    # Outputs an <icon>-tag for use in xmobar to display the generated
#    # XPM icon.
#    #
#    # Run script from xmobar:
#    # `Run Com "/where/ever/trayer-padding-icon.sh" [] "trayerpad" 10`
#    # and use `%trayerpad%` in your template.
#    
#    
#    # Function to create a transparent Wx1 px XPM icon
#    create_xpm_icon () {
#        timestamp=$(date)
#        pixels=$(for i in `seq $1`; do echo -n "."; done)
#    
#        cat << EOF > "$2"
#    /* XPM *
#    static char * trayer_pad_xpm[] = {
#    /* This XPM icon is used for padding in xmobar to */
#    /* leave room for trayer-srg. It is dynamically   */
#    /* updated by by trayer-padding-icon.sh which is run  */
#    /* by xmobar.                                     */
#    /* Created: ${timestamp} */
#    /* <w/cols>  <h/rows>  <colors>  <chars per pixel> */
#    "$1 1 1 1",
#    /* Colors (none: transparent) */
#    ". c none",
#    /* Pixels */
#    "$pixels"
#    };
#    EOF
#    }
#    
#    # Width of the trayer window
#    width=$(xprop -name panel | grep 'program specified minimum size' | cut -d ' ' -f 5)
#    
#    # Icon file name
#    iconfile="/tmp/trayer-padding-${width}px.xpm"
#    
#    # If the desired icon does not exist create it
#    if [ ! -f $iconfile ]; then
#        create_xpm_icon $width $iconfile
#    fi
#    
#    # Output the icon tag for xmobar
#    echo "<icon=${iconfile}/>"
#  '';
}

