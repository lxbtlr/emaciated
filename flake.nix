{
  description = "My Emacs Binary Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: 
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      # Just export the raw Emacs package (e.g., emacs-gtk or emacs-pgtk)
      packages.${system}.default = pkgs.emacs-pgtk; 
      
      # We export the path to this directory so Home Manager can find your init.el
      lib.configPath = ./.; 
    };
}


#
#{
#  description = "experimental emacs flake";
#
#  inputs = {
#    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
#  };
#
#outputs = { self, nixpkgs }:
#    let
#      system = "x86_64-linux"; 
#      pkgs = nixpkgs.legacyPackages.${system};
#    in
#    {
#      packages.${system}.default = pkgs.writeShellScriptBin "launch-emacs" ''
#        # --init-directory set location of conf files
#        ${pkgs.emacs}/bin/emacs -nw --init-directory $(pwd) "$@"
#      '';
#    };
#}


