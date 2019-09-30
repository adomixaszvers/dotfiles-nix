{ pkgs, config, lib, ... }:
let
  customPlugins = {
    lightline-ale = pkgs.vimUtils.buildVimPlugin {
      name = "lightline-ale";
      src = pkgs.fetchFromGitHub {
        owner = "maximbaz";
        repo = "lightline-ale";
        rev = "dd59077f9537b344f7ae80f713c1e4856ec1520c";
        sha256 = "1f9v6nsksy36s5i27nfx6vmyfyjk27p2w2g6x25cw56b0r3sgxmx";
      };
    };
    lightline-bufferline = pkgs.vimUtils.buildVimPlugin {
      name = "lightline-bufferline";
      src = pkgs.fetchFromGitHub {
        owner = "mengelbrecht";
        repo = "lightline-bufferline";
        rev = "87431565ccfcc4c9ac892ab271eae77920c191e5";
        sha256 = "0dmcabdp8768fnb0mzzcidz6bx1aj93fgm4l4z768ils3xycbyw8";
      };
    };
    lightline-neomake = pkgs.vimUtils.buildVimPlugin {
      name = "lightline-neomake";
      src = pkgs.fetchFromGitHub {
        owner = "sinetoami";
        repo = "lightline-neomake";
        rev = "08271edbdb8b6efb21123cd602471a806dff1913";
        sha256 = "0gr4kpci2w38xskh2y588amzpp5grnp0qyi7a06vcsq930l0yq41";
      };
    };
  };
in {
  programs.neovim = {
    enable = true;
    extraConfig = lib.readFile ./vimrc;
    plugins = with pkgs.vimPlugins // customPlugins; [
      ale
      commentary
      fugitive
      fzf-vim
      fzfWrapper
      lightline-ale
      lightline-bufferline
      lightline-vim
      rainbow
      repeat
      surround
      vim-colorschemes
      vim-easymotion
      vim-gitgutter
      vim-hardtime
      vim-polyglot
      vim-unimpaired
      vinegar
    ];
  };
}
