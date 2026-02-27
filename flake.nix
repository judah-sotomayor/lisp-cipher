{
  description = "lisp-cipher";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = {self, nixpkgs} :
    let
      forAllSystems = function:
        nixpkgs.lib.genAttrs [
          "x86_64-linux"
          "aarch64-darwin"
        ] (system: function nixpkgs.legacyPackages.${system} system);
    in
      {
        devShells = forAllSystems (pkgs: system: {
          default = pkgs.mkShell {
            name = "lisp-cipher";
            packages = with pkgs; [
              sbcl
              sbclPackages.qlot-cli
            ];
          };
        });
      };
}
