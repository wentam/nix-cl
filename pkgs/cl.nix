# These are manual packages - see ql.nix for automated quicklisp packages

{ generic-builders, pkgs, cl, ... }: let
  build-asdf-system      = generic-builders.build-asdf-system;
  build-quicklisp-system = generic-builders.build-quicklisp-system;
  bqs = build-quicklisp-system;
in rec {

  parse-declarations = (build-asdf-system rec {
    name = "parse-declarations";
    version = "64bf656d33c78367f97bee851a8c9371ba459765";

    src = pkgs.fetchgit {
      url = "https://gitlab.common-lisp.net/parse-declarations/parse-declarations.git";
      rev = version;
      sha256 = "sha256-wVDfsnhDEVO84aTItt3Sagofiu6zv0SjN/QJ9II/flk=";
    };
  });

  # quri fails to build with automatic cl-utilities
  cl-utilities = (build-asdf-system rec {
    name = "cl-utilities";
    version = "6b4de39201d439330aec0b3589931aa3be570e66";

    src = pkgs.fetchFromGitHub {
      owner = "Publitechs"; repo = "cl-utilities";
      rev = version;
      sha256 = "sha256-VZygX86y2HecmpEvtaDWWX9rZdzupc4awdqHBCpBJ34=";
    };
  });

  cxml = (build-asdf-system rec {
    name = "cxml";
    version = "8701da08ba4aac30891b8d2005edb018c1d3d796";

    src = pkgs.fetchFromGitHub {
      owner = "sharplispers"; repo = "cxml";
      rev = version;
      sha256 = "sha256-oahCFKDm4a39BFn9ONZEzYRxu0MqK2yU67bu09fQ1KE=";
    };

    postInstall = ''
      mkdir -p $out/asdf-system/cxml/system/xml/
      cp catalog.dtd $out/asdf-system/cxml/system/
    '';

    cl-deps = with cl; [ closure-common puri trivial-gray-streams ];
  });

  cl-plus-ssl = (build-asdf-system rec {
    name = "cl+ssl";
    version = "1e2ffc9511df4b1c25c23e0313a642a610dae352";

    src = pkgs.fetchFromGitHub {
      owner = "cl-plus-ssl"; repo = "cl-plus-ssl";
      rev = version;
      sha256 = "sha256-3p2BEsD7pjvkYZcYwtOohwfhHc87gmheFPq/ZwKBjUc=";
    };

    cl-deps = with cl; [ alexandria bordeaux-threads cffi flexi-streams trivial-features trivial-garbage trivial-gray-streams uiop usocket ];
    native-deps = [ pkgs.openssl.out ];
  });

  # Swank 2.28 from quicklisp doesn't specify all of the components, and instead
  # is doing some weird trick with swank-loader.lisp
  #
  # Github HEAD is sane though, though - so we'll just manually package this.
  swank = (build-asdf-system rec {
    name = "swank";
    version = "dd179f4a0c6874fe0e49fb6c460e9e52a5f58833";

    src = pkgs.fetchFromGitHub {
      owner = "slime"; repo = "slime";
      rev = version;
      sha256 = "sha256-LNfkeU+C4dIPs61bBM3FBa/GB81zk0AH/2evzJhi97k=";
    };

    # Swank makes some environmental assumptions regarding paths and how the system
    # was installed.
    #
    # Don't clean, and patch the loader to be able to find modules
    no-bundle-clean = true;

    postPatch = ''
      sed "s|\*load-path\* '()|*load-path* (list \"$out/contrib/\")|" -i swank.lisp
      sed "30i (defvar *source-directory* \"$out/\")" -i swank-loader.lisp
      sed "30i (defvar *fasl-directory* (uiop:pathname-parent-directory-pathname *load-pathname*))" -i swank-loader.lisp
    '';
  });

  swank-slash-exts = (build-asdf-system rec {
    name = "swank/exts";
    version = "dd179f4a0c6874fe0e49fb6c460e9e52a5f58833";

    src = pkgs.fetchFromGitHub {
      owner = "slime"; repo = "slime";
      rev = version;
      sha256 = "sha256-LNfkeU+C4dIPs61bBM3FBa/GB81zk0AH/2evzJhi97k=";
    };

    no-bundle-clean = true;

    cl-deps = with cl; [ swank ];
  });


  # cl-xmpp breaks the naming convention, someone maintains a less broken version on GH
  cl-xmpp = (build-asdf-system rec {
    name = "cl-xmpp";
    version = "e0d5bac33473e8ffe10df2848a117a837d98c61b";

    src = pkgs.fetchFromGitHub {
      owner = "atlas-engineer"; repo = "cl-xmpp";
      rev = version;
      sha256 = "sha256-mKAGvirLOda7+FLu17NIQJ+fA1vSNFj2uqX1ZI8aZR4=";
    };

    cl-deps = with cl; [ cxml ironclad usocket fxml ];
  });

  cl-xmpp-slash-tls = (build-asdf-system rec {
    name = "cl-xmpp/tls";
    version = "e0d5bac33473e8ffe10df2848a117a837d98c61b";

    src = pkgs.fetchFromGitHub {
      owner = "atlas-engineer"; repo = "cl-xmpp";
      rev = version;
      sha256 = "sha256-mKAGvirLOda7+FLu17NIQJ+fA1vSNFj2uqX1ZI8aZR4=";
    };

    cl-deps = with cl; [ cxml ironclad usocket fxml cl-base64 cl-sasl cl-plus-ssl ];
  });


  nvlime = (build-asdf-system rec {
    name = "nvlime";
    version = "2cdbcc1d8be09c16674d4b1983fba1a2f15b431a";

    src = pkgs.fetchFromGitHub {
      owner = "monkoose"; repo = "nvlime";
      rev = version;
      sha256 = "sha256-OEcKlhxTxXoORtQFHrOYDNfKuj0A9VbacgZHAMYvmyE=";
    };

    # The in-repo asd has a depends-on of just 'swank', but this is
    # incorrect. Actually needs swank-slash-exts.
    postPatch = ''
      sed 's|:swank|:swank/exts|' -i lisp/nvlime.asd
    '';

    cl-deps = with cl; [ swank-slash-exts alexandria yason ];
  });

  vlime = (build-asdf-system rec {
    name = "vlime";
    version = "3205f02306314ab8cfc9034cf72097891c923e9d";

    src = pkgs.fetchFromGitHub {
      owner = "vlime"; repo = "vlime";
      rev = version;
      sha256 = "sha256-NTP9mwrFrYsZC++fpSTa2IKk/9HHFToOWzc75+7HJkg=";
    };

    # The in-repo asd has a depends-on of just 'swank', but this is
    # incorrect. Actually needs swank-slash-exts.
    postPatch = ''
      sed 's|:swank|:swank/exts|' -i lisp/vlime.asd
    '';

    cl-deps = with cl; [ swank-slash-exts alexandria yason ];
  });


  nvlime-usocket = (build-asdf-system rec {
    name = "nvlime-usocket";
    version = "2cdbcc1d8be09c16674d4b1983fba1a2f15b431a";

    src = pkgs.fetchFromGitHub {
      owner = "monkoose"; repo = "nvlime";
      rev = version;
      sha256 = "sha256-OEcKlhxTxXoORtQFHrOYDNfKuj0A9VbacgZHAMYvmyE=";
    };

    cl-deps = with cl; [ nvlime usocket vom ];
  });

  vlime-usocket = (build-asdf-system rec {
    name = "vlime-usocket";
    version = "3205f02306314ab8cfc9034cf72097891c923e9d";

    src = pkgs.fetchFromGitHub {
      owner = "vlime"; repo = "vlime";
      rev = version;
      sha256 = "sha256-NTP9mwrFrYsZC++fpSTa2IKk/9HHFToOWzc75+7HJkg=";
    };

    cl-deps = with cl; [ vlime usocket vom ];
  });

  nvlime-sbcl = (build-asdf-system rec {
    name = "nvlime-sbcl";
    version = "2cdbcc1d8be09c16674d4b1983fba1a2f15b431a";

    src = pkgs.fetchFromGitHub {
      owner = "monkoose"; repo = "nvlime";
      rev = version;
      sha256 = "sha256-OEcKlhxTxXoORtQFHrOYDNfKuj0A9VbacgZHAMYvmyE=";
    };

    cl-deps = with cl; [ nvlime vom ];
  });

  vlime-sbcl = (build-asdf-system rec {
    name = "vlime-sbcl";
    version = "3205f02306314ab8cfc9034cf72097891c923e9d";

    src = pkgs.fetchFromGitHub {
      owner = "vlime"; repo = "vlime";
      rev = version;
      sha256 = "sha256-NTP9mwrFrYsZC++fpSTa2IKk/9HHFToOWzc75+7HJkg=";
    };

    cl-deps = with cl; [ vlime vom ];
  });

  #metabang-bind = (build-asdf-system rec {
  #  name = "metabang-bind";
  #  version = "ee35be8042e2ba7f3f734f70458236b4f0547d62";

  #  src = pkgs.fetchFromGitHub {
  #    owner = "gwkkwg"; repo = "metabang-bind";
  #    rev = version;
  #    sha256 = "sha256-WJgN31jVi0yTF7TShpPDuG9mRTbbSIfa52oboSf+K8s=";
  #  };
  #});

 # hu-dot-dwim-dot-util = (build-asdf-system rec {
 #   name = "hu.dwim.util";
 #   version = "3def50fed925acbae83cf5ac7aa1e22facdf01b8";

 #   src = pkgs.fetchFromGitHub {
 #     owner = "hu-dwim"; repo = "hu.dwim.util";
 #     rev = version;
 #     sha256 = "sha256-xXuDOIyZbFQj30AiCQ77AMcUdHYIfrOTVI43dTTunq8=";
 #   };
 # });

 #hu-dot-dwim-dot-syntax-sugar = (build-asdf-system rec {
 #  name = "hu.dwim.syntax-sugar";
 #  version = "0927cf2db26eac114e8a61f478ccdd01b37a4639";

 #  src = pkgs.fetchFromGitHub {
 #    owner = "hu-dwim"; repo = "hu.dwim.syntax-sugar";
 #    rev = version;
 #    sha256 = "sha256-aXPyY3dLPSAzfA7MrHUmNg0uExd2HpuGJl5aFBs5xLM=";
 #  };
 #});



  asdf = (build-asdf-system rec {
    name = "asdf";
    version = "3.3.6";

    src = pkgs.fetchgit {
      url = "https://gitlab.common-lisp.net/asdf/asdf.git";
      rev = version;
      sha256 = "sha256-e3r0y2VZA/g4Vyp+gTFpbAitHxeLca9pZjoG3LzPYCk=";
    };

    # Must be patch phase due to the way we're patching the source
    prePatch = ''
      make build/asdf.lisp
    '';
    postInstall = ''
      cp version.lisp-expr $out/asdf-system/asdf/system/
      mkdir -p $out/asdf-system/asdf/system/build/
      cp build/asdf.lisp $out/asdf-system/asdf/system/build/
    '';
  });
}
