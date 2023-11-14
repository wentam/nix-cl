{pkgs, cl, quicklisp-pkg-overrides, ...}: let
  mkFaslAble = f: origArgs:
    (f origArgs) // { withFaslsFor = impl: mkFaslAble f ( origArgs // { withFaslsFor = impl; } ); };

  # List of dependencies that shouldn't be considered when deciding what packages to pull in,
  # generally because they're provided by the implementation in use.
  cl-deps-blacklist = [ "sb-bsd-sockets" "sb-posix" "sb-rotate-byte" "sb-cltl2" "uiop"
  "sb-concurrency" "sb-rt" "sb-introspect" "sb-sprof" ];

  # ASDF system names don't always play along perfectly with nix attribute names. This specifies
  # replacements needed to convert ASDF system names into nix names.
  #
  # Items in the first list are replace with items in the second list at the same index.
  system-name-replacements-from = [ "+"      "/"       "." ];
  system-name-replacements-to =   [ "-plus-" "-slash-" "-dot-" ];

  # Converts an ASDF system name to a cl package.
  starts-with-num = str: ((builtins.match "^([0-9].*)" str) != null);
  prefix-if-needed = str: if (starts-with-num str) then "cl-" else "";
  system-name-to-pkg = system-name: cl."${system-name-to-pkg-name system-name}";
  system-name-to-pkg-name = system-name:
    "${prefix-if-needed system-name}${(builtins.replaceStrings system-name-replacements-from
                                                               system-name-replacements-to
                                                               system-name)}";

in rec {

  # TODO: be consistant, impl should be withFaslsFor or vice-versa (though they should actually both
  # be builtFor)

  build-quicklisp-system = {name, version, hash, url, ...}@args: (build-asdf-system ((args // {
    inherit name version;

    src = pkgs.fetchurl {
      inherit hash url; # nix hash to-sri hash --type type
    };
  }) // (if (builtins.hasAttr (system-name-to-pkg-name name) quicklisp-pkg-overrides) then quicklisp-pkg-overrides."${(system-name-to-pkg-name name)}" else {})));

  build-asdf-system = origArgs: mkFaslAble ({
      name,
      version,
      src,
      native-deps ? [],
      withFaslsFor ? "sbcl",
      include-asds ? [ ], # TODO: factor this out. Should no longer be needed now that
                          # we're starting to follow the one system = one package rule.
                          #
                          # ASDF specifies that the asd file name must equal the system name,
                          # so should be safe to just find the single correct asd and copy
                          # it in.
      prePatch ? "",
      postPatch ? "",
      monolithic-bundle ? true,
      bundle ? true,
      program-op ? false,
      cl-deps ? [],
      extra-cl-deps ? [],

      # If we should provide temporary mutable source to the system during build time.
      # This may prevent data files from being referenced at the source location,
      # and may prevent SLIME/VLIME etc from finding the source. A few systems needs this.
      mutable-src ? false,

      no-bundle-clean ? false,
      ...
    }@args:

    (pkgs.stdenv.mkDerivation (let
      patched-src = pkgs.stdenv.mkDerivation {
        name = "${name}-patched-source";
        inherit version src postPatch prePatch;
        dontBuild = true;
        doUnpack = false;

        patchPhase = ''
          runHook prePatch
          find . -iname "*.asd" -exec sed 's/:build-operation.*$//' -i {} \;
          find . -iname "*.asd" -exec sed 's/:build-pathname.*$//' -i {} \;

          ${if ((builtins.length native-deps) > 0) then (
            (builtins.toString (builtins.map (native-dep: ''
              for lib in $(find -L "${native-dep}" -iname "*.so*"); do
                find . -iname "*.lisp" -exec sed "s|\".*$(basename $lib)\"|\"''${lib}\"|g" -i {} \;
              done

              # Note the difference in iname pattern relative to above
              for lib in $(find -L "${native-dep}" -iname "*.so"); do
                libname="$(echo $(basename $lib) | sed 's/.so$//')"
                find . -iname "*.lisp" -exec sed "s|:default \"''${libname}\"|:default \"$(echo $lib | sed 's/.so$//')\"|g" -i {} \;
              done
            '') native-deps))) else ""}
          runHook postPatch
        '';

        installPhase = ''
          mkdir -p $out
          cp -r . $out/
        '';
      };
      asdf = (cl.asdf.withFaslsFor withFaslsFor);
      build-script = pkgs.writeText "build.lisp" ''
        ;;; Build script
        ;;;
        ;;; We can assume to have ASDF and uiop (uiop is included with asdf).

        ${if (withFaslsFor == "ecl" && name != "asdf") then ''
          ;; ECL has problems loading bundled ASDF, it'll blow up the stack
          (load "${asdf}/asdf-system/asdf/system/build/asdf.lisp")
        '' else ''
          (require "asdf")
        ''}

        (load "${./lib.lisp}")

        ;; Pull in nix stuff as globals
        (defvar *out* (uiop:getenv "out"))
        (defvar *src* (uiop:getenv "lisp_source"))
        (defvar *system-name* "${name}")
        (defvar *system-build-path*
          (concatenate 'string
                       *out*
                       "/asdf-system/"
                       (asdf:primary-system-name *system-name*)
                       "/system/"))
        (defvar *impl* "${withFaslsFor}")

        ;; Build fast-load files for the system.
        (asdf:operate :build-op *system-name*)

        (format t "SYSTEM NAME: ~s~%" *system-name*)
        (format t "ASDPATH: ~s~%" (lib:asd-path *system-name*))
        (format t "ASDF VERSION: ~s~%" (asdf:asdf-version))

        ${if (bundle) then ''
          ;; We don't use deliver-asd-op because:
          ;; * ASDF doesn't have the context information we do about our build environment to
          ;;   determine where "internal deps" ends and "external deps" begins. It doesn't and
          ;;   shouldn't make those assumptions. This results in incorrect bundling and asd
          ;;   generation for our use-case.
          ;; * inconsistant behavior across ASDF versions
          ;; * doesn't always generate the correct dependencies in the ASD
          ;; * non-monolithic deliver-asd-op won't chase internal dependencies at all for the
          ;; bundle
          ;; * monolithic-deliver-asd-op won't generate any dependencies in the asd at all
          ;;   (even though in our usage it should have dependencies)
          ;; * it doesn't work correctly with package-inferred systems
          ;; * doesn't handle corner-cases well
          ;;
          ;; Using the information we have about our build environment, it's pretty
          ;; straightforward to chase the dependency tree and build our own ASD:
          ;; we know that everything in our nix store path is "internal".
          ;;
          ;; Thus, we're just calling the bundle operation directly.
          ;;
          ;; This is only bundling stuff that hasn't been built yet, which
          ;; is exactly what we want. This means the bundle will contain
          ;; the entire internal dependency tree of the package, but no
          ;; external dependencies.
          ;;
          ;; It's possible this is a bug, but I'm not sure. If for some reason this behavior
          ;; changes in ASDF, it should be trivial to implement a custom bundle operation
          ;; that does what we need
          (asdf:operate :monolithic-compile-bundle-op *system-name*)

          ${if (withFaslsFor == "ecl") then ''
            (asdf:make-build *system-name*
                 :type :shared-library
                 :monolithic t)
          '' else ""}

          ;; Bundle generation location isn't 100% consistant. Move it into a constant location.
          ;; TODO: do the same for ecl .so files?
          (let ((bundle (lib:find-monolithic-bundle *system-build-path* *impl* *system-name*)))
            (when bundle
              (unless (equal (pathname (directory-namestring bundle))
                             (pathname *system-build-path*))
                (rename-file bundle *system-build-path*))))

          ;; NOTE: old versions of ASDF don't handle bundle name well in the case of slashy
          ;; systems. We've pulled in a newer ASDF, so shouldn't be an issue here.

          (let ((asd-path    (concatenate 'string
                                          *system-build-path* "/"
                                          (asdf:primary-system-name *system-name*)
                                          ".asd"))
                (bundle-path (lib:find-monolithic-bundle *system-build-path* *impl* *system-name*))
                (bundle-name (lib:monolithic-bundle-filename *impl* *system-name* :noext t))
                (components nil))

            ;; If we're building a secondary system of a slashy system, the asd might already
            ;; exist without write permission. Make sure we can write it.
            (when (uiop:directory* asd-path) (uiop:run-program (list "chmod" "600" asd-path)))

            (when (and bundle-path (uiop:directory* bundle-path))
              (setf components `((:compiled-file ,bundle-name))))


            (format t "BUNDLE PATH: ~s~%" bundle-path)
            (format t "system name: ~s~%" *system-name*)
            (format t "source path: ~s~%" *src*)
            (format t "PATH DEPS: ~s~%" (lib:path-deps *system-name* *src*))
            (format t "EXTERNAL DEPS: ~s~%" (lib:external-path-deps *system-name* *src*))

            ;; Write the asd. Append mode in case this is a secondary system.
            ;; TODO: do we need to anything special for ECL .so files to be useful?
            (with-open-file (asd asd-path
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)
                (print `(defsystem ,*system-name* :class ASDF/BUNDLE:PREBUILT-SYSTEM
                          :version ,(lib:system-version *system-name*)
                          :depends-on ,(lib:external-path-deps *system-name* *src*)
                          :components ,components) asd)))
        '' else ""}

        ${if (program-op) then ''
          ;; Use compression
          (defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
            (uiop:dump-image (asdf:output-file o c)
                             :executable t :compression t))

          (asdf:operate :program-op *system-name*)
        '' else ""}

        ${if (bundle && !no-bundle-clean) then ''
          ;; Clean up extra build files and empty directories
          (let ((fasls (uiop:directory* (concatenate 'string
                                                     *system-build-path* "/**/*."
                                                     (lib:fast-load-extension *impl*))))
                (keep (concatenate 'string "*--all-systems." (lib:bundle-fast-load-extension *impl*)))
                (dirs (uiop:directory* (concatenate 'string *system-build-path* "/**/"))))

           (map 'list #'(lambda (x) (unless (pathname-match-p x keep) (delete-file x))) fasls)
           (map 'list #'(lambda (x) (ignore-errors (uiop:delete-empty-directory x))) (reverse dirs)))

           ${if (withFaslsFor == "ecl") then ''
             (let ((o-files (uiop:directory* (concatenate 'string *system-build-path* "/**/*.o"))))
               (map 'list #'(lambda (x) (delete-file x)) o-files))
           '' else ""}
        '' else ""}

      '';

      all-cl-deps = cl-deps ++ extra-cl-deps;
    in args // {
      src = patched-src;

      /*

        ${if (name != "asdf") then ''
          ${builtins.trace "${name} dependencies: ${builtins.toJSON (builtins.map (dep: dep.name) all-cl-deps)}" "true"}
          '' else ""}
        */


      buildPhase = ''
        runHook preBuild

        filename_chop=$(echo ${pkgs.lib.strings.escapeShellArg name} | sed 's|/.*$||')

        mkdir -p $out/asdf-system/$filename_chop/system/ $out/asdf-system/$filename_chop/deps/

        # If this is a slashy system, and we're not the primary system, we need to
        # first have everything of the parent system to be compatible with how ASDF
        # loads systems.
        ${if (builtins.length (builtins.split "/" name) > 1) then ''
          ${pkgs.rsync}/bin/rsync --exclude deplist -a --no-perms --no-owner \
            ${(system-name-to-pkg (builtins.elemAt (builtins.split "/" name) 0)).withFaslsFor withFaslsFor}/ \
            $out/
        '' else ""}

        # Create dependency symlinks
        # These symlinks are here to allow ASDF to recurse through the directory structure to
        # find the entire dependency tree

        ${builtins.toString (builtins.map (i: let
          i-passthrough = i.withFaslsFor withFaslsFor;
        in ''
          ln -snf "${i-passthrough}" $out/asdf-system/$filename_chop/deps/$(basename "${i-passthrough}")
        '') all-cl-deps)}

        # ASDF 'remembers' where it's source was when built. This is sometimes used to reference
        # files (FXML does this), and is also how SLIME/VLIME finds source files.
        #
        # If we build in the normal build directory, the source will be destroyed and those
        # references will be invalid.
        #
        # We also need to be able to patch the source. This means it either needs to built in $out,
        # or we need to create a 'patched source' derivation and build from that.
        #
        # The nature of "deliver-asd-op" can make a really clean deployment if we build from
        # a patched source derivation, so we're doing that.

        # CL_SOURCE_REGISTRY tells ASDF where to look for .asds. Paths ending in // tells ASDF
        # to search recursively.
        #
        # ASDF_OUTPUT_TRANSLATIONS tells ASDF where to look for and build fasls. By mapping the
        # storeDir to itself 1:1, fasls will be built and searched for right next to the .lisp
        # files as long as they're in the nix store.

        # Source must be before deps, or a dep containing the same.asd could create problems
        #
        # ASDF must be after source, or when building something contained within ASDF - such
        # as inferior shell, things will break

        ${if mutable-src then ''
          export lisp_source="$(pwd)"
        '' else ''
          export lisp_source="${patched-src}"
        ''}

        ${if (name != "asdf") then ''
          export CL_SOURCE_REGISTRY="''${lisp_source}//:$out/asdf-system/$filename_chop/deps//:${asdf}//"
        '' else ''
          export CL_SOURCE_REGISTRY="''${lisp_source}//:$out/asdf-system/$filename_chop/deps//"
        ''}

        export ASDF_OUTPUT_TRANSLATIONS="''${lisp_source}:$out/asdf-system/$filename_chop/system/:${builtins.storeDir}:${builtins.storeDir}"

        # Some packages really want a mutable home (slime/swank as an example). Let's make a fake one.
        mkdir -p home/
        export HOME="$(pwd)/home/"

        # Build
        #
        # Build must occur after 'install' because we need to have the dependency symlinks
        # prepared.

        ${if (withFaslsFor == "sbcl") then "${pkgs.sbcl}/bin/sbcl --script ${build-script}" else ""}
        ${if (withFaslsFor == "ecl" ) then "${pkgs.ecl}/bin/ecl --shell  ${build-script}" else ""}
        ${if (withFaslsFor == "ccl" ) then "${pkgs.ccl}/bin/ccl -b -l ${build-script} -e '(quit)'" else ""}

        # If we're not doing a bundled install, we need to copy our source into the system
        # path, including only the asds the package specifies.
        #
        # Including only the specified asds ensures that those who depend on us don't try
        # to use asds unrelated to this system - and thus try to use the non-existant fast-load
        # files.
        ${if (!bundle) then ''
          cp -r ${patched-src}/. $out/asdf-system/$filename_chop/system/

          cd $out/asdf-system/$filename_chop/system/
          for asd_file in $(find . -iname "*.asd"); do
            delete=1

            ${builtins.toString (builtins.map (asd: ''
              if [[ ${asd} -ef  $asd_file ]]; then
                delete=0
              fi
            '') include-asds)}

            if [ $delete -eq 1 ]; then
              rm -f $asd_file || true;
            fi
          done;
        '' else ""}

        runHook postBuild
      '';
      installPhase = ''
        runHook preInstall
        runHook postInstall
      '';
    }))) origArgs;
}
