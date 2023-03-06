{ generic-builders, pkgs, cl, ... }: let
  glfw-pkgs = (builtins.filter (pkg: ((builtins.substring 0 14 pkg) == "cl-glfw-opengl")) (builtins.attrNames cl));
  glfw-overrides = (builtins.listToAttrs (builtins.map (pkg: { name = pkg; value = { native-deps = [ pkgs.libGL ]; }; }) glfw-pkgs));
in
(pkgs.lib.attrsets.recursiveUpdate

  # Native dependencies
  (with pkgs; {
    cl-opengl           = { native-deps = [ libGL          ]; };
    cl-glu              = { native-deps = [ libGL libGLU   ]; };
    cl-glut             = { native-deps = [ libGL freeglut ]; };
    cl-libyaml          = { native-deps = [ libyaml        ]; };
    also-alsa           = { native-deps = [ alsa-lib       ]; };
    cl-glfw             = { native-deps = [ glfw           ]; };
    sqlite              = { native-deps = [ sqlite.out     ]; };
    cl-plus-ssl         = { native-deps = [ openssl.out    ]; };
    cl-egl              = { native-deps = [ libglvnd       ]; };
    cl-glfw-opengl-core = { native-deps = [ libGL          ]; };
  } // glfw-overrides)

  # Misc
  {
    # osicat has a weird .asd
    osicat = {
      extra-cl-deps = [ cl.trivial-features cl.cffi-grovel cl.cffi ];
      buildInputs = [ cl.cffi-grovel ];
    };

    # like osicat
    gsll = {
      extra-cl-deps = [ cl.cffi-libffi ];
    };

    iolib = {
      extra-cl-deps = [ cl.cffi ];
      buildInputs = [ pkgs.libfixposix cl.cffi-grovel ];
      native-deps = [ pkgs.libfixposix ];
    };

    cffi-libffi = {
      # TODO: cffi-libffi is putting # in front of the deps, and this
      # is keeping us from seeing them?
      extra-cl-deps = with cl; [ trivial-features cffi-grovel cffi ];
      native-deps = [ pkgs.libffi ];
      buildInputs = [ pkgs.libffi ];
    };

    # Has some headers we need to install
    cffi-grovel = {
      postInstall = ''
        mkdir -p $out/include/grovel/
        cp grovel/common.h $out/include/grovel/
      '';
    };

    cl-unicode = {
      # TODO: I think the fact that flexi-streams is missed might represent a minor flaw in our dependency chasing
      extra-cl-deps = [ cl.flexi-streams ];
      mutable-src = true;
    };

    trivial-mimes = {
      postInstall = ''
        cp mime.types $out/asdf-system/trivial-mimes/system/
      '';
    };

    # qt-libs is trying to fetch data files from github,
    # but of course nix build doesn't allow that because it's an easy way to
    # introduce impurities.
    #
    # Thankfully, it will play along if we download the files for it. These will be extracted
    # to "standalone" in the src dir, so we need a mutable src. I have no idea where it looks
    # for them at runtime, so this probably doesn't actually work yet.
    qt-libs = let
      lin64 = (builtins.fetchurl {
        url = "https://github.com/Shinmera/qt4/releases/download/qt-libs2.0.1/lin64-qt4.zip";
        sha256 = "sha256:0fnpm7nz1qh8qx36zs6jqzvd338lf4dblnjvyih5j193hb6qvi8x";
      });
      lin64-smokegen = (builtins.fetchurl {
        url = "https://github.com/Shinmera/smokegen/releases/download/qt-libs2.0.1/lin64-smokegen.zip";
        sha256 = "sha256:0krik6h70y4j4dli24f4p69szzyxrr0qmw489dsf48x2bjfqhcdw";
      });
      lin64-smokeqt = (builtins.fetchurl {
        url = "https://github.com/Shinmera/smokeqt/releases/download/qt-libs2.0.1/lin64-smokeqt.zip";
        sha256 = "sha256:0wyxxiail40snq0dnpiy7ij6vb96jdyxhgvmyda7hhjn2krawpqg";
      });
      lin64-commonqt = (builtins.fetchurl {
        url = "https://github.com/Shinmera/libcommonqt/releases/download/qt-libs2.0.2/lin64-libcommonqt.zip";
        sha256 = "sha256:1mhmn5i0pa2gqqk86c5dvdzm9l8njd57xqb2hmzjq11fy4d9h2fv";
      });
    in {
      extra-cl-deps = [ cl.drakma cl.zip ];
      preBuild = ''
          mkdir -p $out/asdf-system/qt-libs/system/qt4/ $out/asdf-system/qt-libs/system/smokegen/
          mkdir -p $out/asdf-system/qt-libs/system/smokeqt/ $out/asdf-system/qt-libs/system/libcommonqt/
          cp ${lin64} $out/asdf-system/qt-libs/system/qt4/binaries.zip
          cp ${lin64-smokegen} $out/asdf-system/qt-libs/system/smokegen/binaries.zip
          cp ${lin64-smokeqt} $out/asdf-system/qt-libs/system/smokeqt/binaries.zip
          cp ${lin64-commonqt} $out/asdf-system/qt-libs/system/libcommonqt/binaries.zip
      '';

      # TODO: This is a guess for standalone dir. Where does it look for it?
      #postInstall = ''
      #  cp -r standalone $out/asdf-system/qt-libs/system/
      #'';

      mutable-src = true;
    };

    # TODO: still broken
    smokebase = let
      lin64 = (builtins.fetchurl {
        url = "https://github.com/Shinmera/qt4/releases/download/qt-libs2.0.1/lin64-qt4.zip";
        sha256 = "sha256:0fnpm7nz1qh8qx36zs6jqzvd338lf4dblnjvyih5j193hb6qvi8x";
      });
      lin64-smokegen = (builtins.fetchurl {
        url = "https://github.com/Shinmera/smokegen/releases/download/qt-libs2.0.1/lin64-smokegen.zip";
        sha256 = "sha256:0krik6h70y4j4dli24f4p69szzyxrr0qmw489dsf48x2bjfqhcdw";
      });
      lin64-smokeqt = (builtins.fetchurl {
        url = "https://github.com/Shinmera/smokeqt/releases/download/qt-libs2.0.1/lin64-smokeqt.zip";
        sha256 = "sha256:0wyxxiail40snq0dnpiy7ij6vb96jdyxhgvmyda7hhjn2krawpqg";
      });
      lin64-commonqt = (builtins.fetchurl {
        url = "https://github.com/Shinmera/libcommonqt/releases/download/qt-libs2.0.2/lin64-libcommonqt.zip";
        sha256 = "sha256:1mhmn5i0pa2gqqk86c5dvdzm9l8njd57xqb2hmzjq11fy4d9h2fv";
      });
    in
    {
      extra-cl-deps = [ cl.qt-plus-libs ];
      buildInputs = [ pkgs.qt4 ];

      #preBuild = ''
      #  mkdir -p $out/asdf-system/smokebase/system/qt4/ $out/asdf-system/smokebase/system/smokegen/
      #  mkdir -p $out/asdf-system/smokebase/system/smokeqt/ $out/asdf-system/smokebase/system/libcommonqt/

      #  cp ${lin64} $out/asdf-system/smokebase/system/qt4/binaries.zip
      #  cp ${lin64-smokegen} $out/asdf-system/smokebase/system/smokegen/binaries.zip
      #  cp ${lin64-smokeqt} $out/asdf-system/smokebase/system/smokeqt/binaries.zip
      #  cp ${lin64-commonqt} $out/asdf-system/smokebase/system/libcommonqt/binaries.zip
      #'';

      mutable-src = true;
    };

    uax-15 = {
      preInstall = ''
        cp -r unicode-15-data $out/asdf-system/uax-15/system/
      '';
    };

    jingoh-dot-org = {
      extra-cl-deps = [ cl.cl-3bmd cl.cl-3bmd-ext-code-blocks cl.read-as-string 
      cl.eclector cl.cl-ppcre cl.check-bnf];
    };
    jingoh-dot-tester = {
      extra-cl-deps = [ cl.cl-3bmd cl.cl-3bmd-ext-code-blocks cl.read-as-string 
      cl.eclector cl.cl-ppcre cl.check-bnf cl.bordeaux-threads cl.structure-ext
      cl.cl-ansi-text cl.vivid-diff cl.fuzzy-match];
    };

    # hu.dwim.* stuff has a lot of problems

    hu-dot-dwim-dot-util = {
      postPatch = ''
        find . -iname "*.lisp" -exec sed 's/(in-package :hu.dwim.util)/(in-package :hu.dwim.util)(enable-sharp-boolean-syntax)(enable-sharp-comment-syntax)(enable-readtime-wrapper-syntax)(enable-feature-cond-syntax)/' -i {} \;
      '';
    };

    hu-dot-dwim-dot-util-slash-threads = {
      extra-cl-deps = [
        cl.hu-dot-dwim-dot-def-slash-namespace
        cl.hu-dot-dwim-dot-defclass-star
      ];
      postPatch = ''
        find . -iname "*.lisp" -exec sed 's/(in-package :hu.dwim.util)/(in-package :hu.dwim.util)(enable-sharp-boolean-syntax)(enable-sharp-comment-syntax)(enable-readtime-wrapper-syntax)(enable-feature-cond-syntax)/' -i {} \;
      '';
    };

    hu-dot-dwim-dot-presentation = {
      extra-cl-deps = [ cl.hu-dot-dwim-dot-asdf ];
    };
  }
)

# Broken stuff in top 300 of sorted list
#
# hu-dot-dwim-dot-util - itself, broken macro - mess of custom syntax macros makes this difficult to debug
#   hu-dot-dwim-dot-logger                                                      - hu-dot-dwim-dot-util
#   hu-dot-dwim-dot-walker                                                      - hu-dot-dwim-dot-util
#   hu-dot-dwim-dot-delico                                                      - hu-dot-dwim-dot-util
#   hu-dot-dwim-dot-computed-class                                              - hu-dot-dwim-dot-util
#   hu-dot-dwim-dot-serializer                                                  - hu-dot-dwim-dot-util
#   hu-dot-dwim-dot-def-plus-hu-dot-dwim-dot-delico                             - hu-dot-dwim-dot-util
#   hu-dot-dwim-dot-quasi-quote-dot-xml                                         - hu-dot-dwim-dot-util
#   hu-dot-dwim-dot-quasi-quote-dot-xml-plus-hu-dot-dwim-dot-quasi-quote-dot-js - hu-dot-dwim-dot-util
#
# qt-libs - itself, trying to impurely fetch network stuff at build time w/ usocket
#   qt-plus-libs - qt-libs
#   smokebase    - qt-libs
#   commonqt     - qt-libs
#   qtcore       - qt-libs
#   qtgui        - qt-libs
#   qtools       - qt-libs
#
# mcclim-clx    - itself, something nil?
# mcclim-render - itself, something nil?
#
# cl-l10n-cldr                     - itself, directory nil bug
# cl-l10n                          - itself, directory nil bug
# asdf-package-system              - itself, directory nil bug
# hu-dot-dwim-dot-def-plus-cl-l10n - itself, directory nil bug
#
# hu-dot-dwim-dot-zlib       - we're missing cffi-c2ffi from the cffi repo
# hu-dot-dwim-dot-web-server - we're missing cffi-c2ffi from cffi repo
#
# hu-dot-dwim-dot-uri - iolib-slash-sockets
#
# hu-dot-dwim-dot-util-plus-iolib    - iolib-slash-os
# hu-dot-dwim-dot-quasi-quote-dot-js - iolib-slash-os
# hu-dot-dwim-dot-logger-plus-iolib  - iolib-slash-os


