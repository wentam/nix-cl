
*This repo is experimental. Not stable. Everything could completely change at any moment.*

## Dependency resolution

Each CL package is installed into `$out/asdf-system/[system-name]/system/`. Symlinks for all of the
top-level dependencies for that system are created in `$out/asdf-system/[system-name]/deps/`.

This effectively turns the usage of `CL_SOURCE_REGISTRY` into a `PATH` but for CL. as long as
`CL_SOURCE_REGISTRY` ends in a double-slash ('//') it will search recursively.

This means you can just point ASDF to the top level of a nix store path:
`CL_SOURCE_REGISTRY=/nix/store/[some-system]// sbcl --eval '(require :asdf)' --eval '(asdf-load-system :system-name)'`.

ASDF will follow the dependency tree through the symlinks. No need to try to resolve this ourselves.

We're just using symlinks to provide the information ASDF expects and letting it take care of the
rest.

## Building fasls

I've added `withFaslsFor` to the derivation. We can just do: `(cl-package.withFaslsFor "sbcl")`
to get a package that contains fasls for SBCL.

Currently will default to sbcl.

Building for nothing should be a supported case in the future, but right now needs to be built
for something.

## On NixOS: Installing a CL library in your system environment

Thanks to the directory structure used here, we can link /asdf-system in `environment.pathsToLink`.
Point `CL_SOURCE_REGISTRY` to the result. Now we can just install lisp packages to our system
environment and it will just work.

For fasl files to play along, we also need to point `ASDF_OUTPUT_TRANSLATIONS`.

All of this has been taken care of in the 'cl-system-env' module of this flake.

```nix
{
  imports = [ this-flake.nixosModules.cl-system-env ];
  environment.systemPackages = [ cl.system-name ];

  # Or with fasls for sbcl
  environment.systemPackages = [ (cl.system-name.withFaslsFor "sbcl") ];
}
```

And now it's available in your system environment:

`sbcl --eval '(require :asdf)' --eval '(asdf-load-system :system-name)'`

## Native libraries

Native library paths are patched to facilitate the flexibility discussed above. This is
semi-automatic though: you just need to specify what packages contain the needed libraries with
`native-deps`.

## Bundles

By default, all builds are bundles. This is done via monolithic-bundle-op, but thanks to it's
specific behavior in our environment it's not fully monolithic and will only contain dependencies
internal to a given project. This nicely supports package-inferred systems and the like.

Currently, non-bundled builds are probably broken - but should be supported in the future.

