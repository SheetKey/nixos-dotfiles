# Alacritty config

{ pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    
    settings = {
      window = {
        padding.x = 10;
	      padding.y = 10;
      };

      background_opacity = 0.94;
      
      font = {
        size = 9.0;
        
	      normal.family = "mononoki Nerd Font Mono";
	      normal.style = "Regular";
	      bold.family = "monospace";
	      bold.style = "Bold";
	      italic.family = "monospace";
	      italic.style = "Italic";
	      bold_italic.family = "monospace";
	      bold_italic.style = "Bold Italic";
      };
      
      colors = {
        # Default colors
	      primary = {
	        background = "#002b36";
	        foreground = "#839496";
	      };
        
	      # Cursor colors
	      cursor = {
	        text = "#002b36";
	        cursor = "#839496";
	      };
        
	      # Normal colors
	      normal = {
	        black = "#073642";
	        red = "#dc322f";
	        green = "#859900";
	        yellow = "#b58900";
	        blue = "#268bd2";
	        magenta = "#d33682";
	        cyan = "#2aa198";
	        white = "#eee8d5";
	      };
        
	      # Bright colors
	      bright = {
	        black = "#586e75";
	        red = "#cb4b16";
	        green = "#586e75";
	        yellow = "#657b83";
	        blue = "#839496";
	        magenta = "#6c71c4";
	        cyan = "#93a1a1";
	        white = "#fdf6e3";
	      };
        
      };
    };
  };
  
}
