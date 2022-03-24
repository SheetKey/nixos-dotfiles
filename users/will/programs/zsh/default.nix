# zsh default.nix

{ pkgs, ... }:

{

  programs.zsh = {
    enable = true;

    enableAutosuggestions = true;
    enableCompletion = true;
    completionInit = ''
      autoload -U compinit
      zstyle ':completion:*' menu select
      zmodload zsh/complist
      compinit
      _comp_options+=(globdots)
    '';
    enableSyntaxHighlighting = true;

    history = {
      save = 1000000;
      size = 1000000;
    };

    initExtraBeforeCompInit = ''
      autoload -Uz vcs_info
      precmd() { vcs_info }
      zstyle ':vcs_info:git:*' formats 'on branch %b'
      setopt PROMPT_SUBST
      PS1="%B%F{red}[%F{blue}%n%F{yellow}@%F{green}%M %F{magenta}%~ %F{yellow}${vcs_info_msg_0_}%F{red}]%{$reset_color%}$%b "
    '';

    initExtra = ''
      bindkey -M menuselect 'h' vi-backward-char
      bindkey -M menuselect 'k' vi-up-line-or-history
      bindkey -M menuselect 'l' vi-forward-char
      bindkey -M menuselect 'j' vi-down-line-or-history

      f () {
        tmp="$(mktemp)"
	      lf -last-dir-path="$tmp" "$@"
	      if [ -f "$tmp" ]; then
	      dir="$(cat "$tmp")"
	      rm -f "$tmp" >/dev/null
	      [ -d "$dir" ] && [ "$dir" != "($pwd)" ] && cd "$dir"
	      fi
      }

      export XDG_DATA_HOME="$HOME/.local/share"

    '';

    plugins = [
      {
        name = "zsh-vi-mode";
	      src = pkgs.fetchFromGitHub {
          owner = "jeffreytse";
	        repo = "zsh-vi-mode";
	        rev = "0eb3c7b43b1f0a81af3676b150747b839e17c125";
	        sha256 = "PmfzWj0MynNvDaws8K0QsLcCyJULwxUyyNIjsA7oLgM=";
	      };
      }
    ];
    
  };


}

