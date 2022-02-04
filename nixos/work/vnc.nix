{ pkgs, ... }: { environment.systemPackages = with pkgs; [ tigervnc ]; }
