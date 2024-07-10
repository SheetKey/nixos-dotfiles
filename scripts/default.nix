# Scripts defaul.nix

{ pkgs, lib, config, ... }:

let
  polybar-exwm-workspace = pkgs.writeShellScriptBin "polybar-exwm-workspace" ''
    emacsclient -e "(will/exwm-workspace)" | sed -e 's/^"//' -e 's/"$//'
  '';
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
  nvidia-set-offload-steam = pkgs.writeShellScriptBin "nvidia-set-offload-steam" ''
    sed 's/^Exec=/&nvidia-offload /' /run/current-system/sw/share/applications/steam.desktop > ~/.local/share/applications/steam.desktop
  '';

  getKernelScript = pkgs.writeShellScriptBin "getKernelScript" ''
    kern="$(uname -r)"
    echo -e "$kern "
  '';

  trayer-padding-icon = pkgs.writeShellScriptBin "trayer-padding-icon.sh" ''
    # Copied from https://github.com/jaor/xmobar/issues/239#issuecomment-233206552
    # Detects the width of running trayer-srg window (xprop name 'panel')
    # and creates an XPM icon of that width, 1px height, and transparent.
    # Outputs an <icon>-tag for use in xmobar to display the generated
    # XPM icon.
    #
    # Run script from xmobar:
    # `Run Com "/where/ever/trayer-padding-icon.sh" [] "trayerpad" 10`
    # and use `%trayerpad%` in your template.
    
    
    # Function to create a transparent Wx1 px XPM icon
    create_xpm_icon () {
        timestamp=$(date)
        pixels=$(for i in `seq $1`; do echo -n "."; done)
    
        cat << EOF > "$2"
    /* XPM *
    static char * trayer_pad_xpm[] = {
    /* This XPM icon is used for padding in xmobar to */
    /* leave room for trayer-srg. It is dynamically   */
    /* updated by by trayer-padding-icon.sh which is run  */
    /* by xmobar.                                     */
    /* Created: ''${timestamp} */
    /* <w/cols>  <h/rows>  <colors>  <chars per pixel> */
    "$1 1 1 1",
    /* Colors (none: transparent) */
    ". c none",
    /* Pixels */
    "$pixels"
    };
    EOF
    }
    
    # Width of the trayer window
    width=$(xprop -name panel | grep 'program specified minimum size' | cut -d ' ' -f 5)
    
    # Icon file name
    iconfile="/tmp/trayer-padding-''${width}px.xpm"
    
    # If the desired icon does not exist create it
    if [ ! -f ''$iconfile ]; then
        create_xpm_icon $width $iconfile
    fi
    
    # Output the icon tag for xmobar
    echo "<icon=''${iconfile}/>" 
  '';

  trayer-width = pkgs.writeShellScriptBin "trayer-width" ''
    xprop -name panel | grep 'program specified minimum size' | cut -d ' ' -f 5
  '';

  dzen2-nix-icon = pkgs.writeShellScriptBin "dzen2-nix-icon" ''
    echo '' | dzen2 -p -fn '-*-firacode nerd font mono-bold-*-*-*-*-250-*-*-*-*-iso8859-1' -h 22 -w 30 -fg '#2fafff' -bg '#000000' -e 'button2=;' 
  '';

  protonhax = src: pkgs.writeScriptBin "protonhax" src;

in {
  options.scripts.protonhax-src = lib.mkOption {
    type = lib.types.string;
  };

  config.environment.systemPackages = [ 
    polybar-exwm-workspace
    getKernelScript
    trayer-padding-icon
    nvidia-offload
    nvidia-set-offload-steam
    trayer-width
    dzen2-nix-icon
    (protonhax config.scripts.protonhax-src)
  ];
}
