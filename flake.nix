{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, ... }@inputs:  (inputs.flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import inputs.nixpkgs { inherit system; };
    cl = self.packages."${system}".cl;
    quicklisp-pkg-overrides = ( import ./pkgs/ql-overrides.nix { inherit generic-builders pkgs cl;} );
    generic-builders = import ./generic-builders.nix { inherit pkgs quicklisp-pkg-overrides cl; };
    quicklisp-pkgs = ( import ./pkgs/ql.nix { inherit generic-builders pkgs cl;} );
    manual-pkgs = ( import ./pkgs/cl.nix { inherit generic-builders pkgs cl;} );
  in {
    # Packages
    #
    # Note the order: manual packages take priority over the automatic quicklisp packages.
    # As seen above, there's an intermediate set that overrides attributes in the quicklisp set.
    packages.cl = quicklisp-pkgs // manual-pkgs;

    # Generic builders (build-asdf-system, build-quicklisp-system)
    lib = generic-builders;
  })) // {
    # Module that allows libraries installed into environment.systemPackages to be used
    nixosModules.cl-system-env = {
      environment.variables.CL_SOURCE_REGISTRY = "/run/current-system/sw/asdf-system//";
      environment.variables.ASDF_OUTPUT_TRANSLATIONS = "${builtins.storeDir}:${builtins.storeDir}";
      environment.pathsToLink = [ "/asdf-system" ];
    };
  };
}
