{
  description = "seabug-contracts";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    cardano-transaction-lib = {
      type = "github";
      owner = "Plutonomicon";
      repo = "cardano-transaction-lib";
      # should be same rev as in packages.dhall
      # To update, do `spago2nix generate`
      # `calum/823-staking-keys-plutip` branch
      rev = "d918af3e09a80e0c3325f0350a97e753d18495bb";
    };
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
  };

  outputs = { self, nixpkgs, cardano-transaction-lib, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];
      perSystem = nixpkgs.lib.genAttrs defaultSystems;
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          cardano-transaction-lib.overlay
          (_: _: {
            ctl-server =
                cardano-transaction-lib.packages.${system}."ctl-server:exe:ctl-server";
          })
        ];
      };
      psProjectFor = system:
        let
          pkgs = nixpkgsFor system;
          src = ./.;
        in
        pkgs.purescriptProject {
          inherit pkgs src;
          projectName = "seabug-contracts";
          shell = {
            packages = with pkgs; [
              easy-ps.purs-tidy
              fd
              plutip-server
              ctl-server
              ogmios
              ogmios-datum-cache
              postgresql
            ];
          };
        };
    in
    {
      defaultPackage = perSystem (system: self.packages.${system}.seabug-contracts-bundle-web);

      packages = perSystem (system:
        let
          project = psProjectFor system;
        in
        {
          seabug-contracts-bundle-web = project.bundlePursProject {
            sources = [ "exe" "src" ];
            main = "Main";
          };
        });

      checks = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          project = psProjectFor system;
        in
        {
          seabug-contracts-unit-test = project.runPursTest {
            sources = [ "exe" "test" "src" ];
          };
          seabug-contracts-plutip-test = project.runPlutipTest {
            name = "seabug-contracts-plutip-test";
            testMain = "Test.Plutip";
            env = {};
          };
          formatting-check = pkgs.runCommand "formatting-check"
            {
              nativeBuildInputs = [
                pkgs.easy-ps.purs-tidy
                pkgs.fd
              ];
            }
            ''
              cd ${self}
              purs-tidy check $(fd -epurs)
              touch $out
            '';
        });

      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system};
          }
          "touch $out"
      );

      devShell = perSystem (system: (psProjectFor system).devShell);
    };
}
