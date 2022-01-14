# Scripts defaul.nix

{ pkgs, ... }:

let
  mashScript = pkgs.writeScriptBin "mashScript" ''
    #!/usr/bin/env perl
    use strict;
    use File::Temp 'tempfile';
    
    # MASH: Mathematica Scripting Hack (use Mathematica like Perl/Python/Ruby/etc)
    #  by Daniel Reeves, 1999; rewritten in Perl for Mathematica 6+ in 2008.
    # MASH allows you to have a self-contained Mathematica script that can be
    #  executed (with arguments) from the command line and used in a pipeline.
    # It functions as the interpreter for Mathematica scripts by serving as a proxy
    #   between the script and the kernel.  Namely, it does the following:
    #  * Takes a Mathematica source file as its first argument (or from stdin if no
    #    arguments).
    #  * Makes the command line arguments available to the Mathematica code as a
    #    list called ARGV (or 'args') and defines convenient functions for printing
    #    to stdout and stderr and reading from stdin.  This is all done by
    #    evaluating the code in $prescript below.
    #  * Evaluates the Mathematica script (printing to stdout and stderr only what
    #    it is explicitly told to).
    #  * Evaluates the code in $postfix below, including exiting Mathematica.
    #  * Additionally, because Mathematica, annoyingly, detaches itself from the
    #    parent process (this script), we have to pass along various termination
    #    signals explicitly (for example, hitting control-c needs to send SIGINT to
    #    Mathematica, which is like "interrupt evaluation" in the frontend).
    # To make a Mathematica script foo.m executable:
    #  * Make the first line of foo.m be:  #!/usr/bin/env /path/to/mash.pl
    #    (or just "#!/usr/bin/env mash" if mash is in $PATH).
    #  * Do chmod u+x foo.m
    
    # Possible paths to the Mathematica kernel.  Add yours.
    my @mathpath = (
      "/nix/store/37fliaf65il82awmr70hj6jcds75ggjy-mathematica-13.0.0/bin/MathKernel",
    );
    my $math;  # The first of the above that actually exists.
    for (@mathpath) { if(-e $_) { $math = $_;  last; } }
    die "Mathematica kernel not found.\n" unless defined($math);
    
    # Evaluate this in Mathematica before eval'ing the script.
    my $prescript = <<'EOF';  # Much of this stuff probably belongs in a package.
    ARGV = args = Drop[$CommandLine, 4];        (* Command line args.             *)
    pr = WriteString["stdout", ##]&;            (* More                           *)
    prn = pr[##, "\n"]&;                        (*  convenient                    *)
    perr = WriteString["stderr", ##]&;          (*   print                        *)
    perrn = perr[##, "\n"]&;                    (*    statements.                 *)
    re = RegularExpression;                     (* I wish mathematica             *)
    eval = ToExpression[cat[##]]&;              (*  weren't so damn               *)
    EOF = EndOfFile;                            (*   verbose!                     *)
    read[] := InputString[""];                  (* Grab a line from stdin.        *)
    doList[f_, test_] :=                        (* Accumulate list of what f[]    *)
      Most@NestWhileList[f[]&, f[], test];      (*  returns while test is true.   *)
    readList[] := doList[read, #=!=EOF&];       (* Slurp list'o'lines from stdin. *)
    cat = StringJoin@@(ToString/@{##})&;        (* Like sprintf/strout in C/C++.  *)
    system = Run@cat@##&;                       (* System call.                   *)
    backtick = Import[cat["!", ##], "Text"]&;   (* System call; returns stdout.   *)
                                                (* ABOVE: mma-scripting related.  *)
    keys[f_, i_:1] :=                           (* BELOW: general utilities.      *)
      DownValues[f, Sort->False][[All,1,1,i]];  (* Keys of a hash/dictionary.     *)
    SetAttributes[each, HoldAll];               (* each[pattern, list, body]      *)
    each[pat_, lst_, bod_] := ReleaseHold[      (*  converts pattern to body for  *)
      Hold[Cases[Evaluate@lst, pat:>bod];]];    (*   each element of list.        *)
    some[f_, l_List] := True ===                (* Whether f applied to some      *)
      Scan[If[f[#], Return[True]]&, l];         (*  element of list is True.      *)
    every[f_, l_List] := Null ===               (* Similarly, And @@ f/@l         *)
      Scan[If[!f[#], Return[False]]&, l];       (*  (but with lazy evaluation).   *)
    pout = pr;                                   (* [Backward compatibility.]     *)
    strout = cat;                                (* [Backward compatibility.]     *)
    EOF
    
    # Evaluate this in Mathematica after eval'ing the script.
    my $postscript = <<'EOF';
    Exit[0];
    EOF
    
    # Slurp up the script, either from file or from stdin.
    my $script = $ARGV[0];
    my @lines;
    if(defined($script)) {
      open(F, "<$script") or die qq{Can't open Mathematica script "$script": $!\n};
      @lines = <F>;
      close(F);
    } else { @lines = <STDIN>; }
    
    # Feed slurped script plus prescript and postscript to temp file.
    shift(@lines) if $lines[0] =~ /^\#\!/;  # exclude the shebang line.
    my($tmpfh, $tmpf) = tempfile("mash-XXXX", SUFFIX=>'.m', UNLINK=>1);
    print $tmpfh $prescript, "\n\n", @lines, "\n\n", $postscript;
    
    # Open a pipe to Mathematica.
    my $cmd = "'$math' -noprompt -run \"<<$tmpf\" " . join(' ', @ARGV);
    my $pid = open(F, "$cmd |") or die "Can't open pipe from $cmd: $!";
    
    # Handle interrupt and termination signals.
    $SIG{INT} =  sub { kill('INT', $pid); };  # pass along INT (2, eg, ctrl-C)
    $SIG{TERM} = sub { kill('TERM', $pid); }; # pass along TERM (15, kill's default)
    $SIG{QUIT} = sub { kill('QUIT', $pid); }; # pass along QUIT (3)
    $SIG{ABRT} = sub { kill('ABRT', $pid); }; # pass along ABRT (6)
    $SIG{HUP} =  sub { kill('HUP', $pid); };  # pass along HUP (1)
    
    # Actually run the Mathematica script, then close the pipe.
    print while(<F>);
    close(F);
    #print STDERR "DEBUG: exiting MASH\n";
    
    #system("cp $tmpf mash-DEBUG.m"); # keep copy of the script with pre/postscript.
  '';

  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
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

in {
  environment.systemPackages = [ 
    getKernelScript
    trayer-padding-icon
    nvidia-offload
    mashScript
  ];
}
