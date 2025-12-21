{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv";
  };

  outputs =
    {
      self,
      nixpkgs,
      devenv,
      ...
    }@inputs:
    let
      forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
    in
    {
      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [
              {
                packages = with pkgs; [
                  # compilers
                  mlton
                  lunarml
                  lua

                  # tools
                  millet
                  smlfmt
                  smlpkg

                  # other
                  gnumake
                  gcc
                ];

                env.LUNARML_LIB = "${pkgs.lunarml}/lib/lunarml";
                env.MLTON_LIB = "${pkgs.mlton}/lib";
              }
            ];
          };
        }
      );
    };
}
