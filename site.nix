let
  _nixpkgs = import <nixpkgs> {};

  nixpkgsSrc = _nixpkgs.fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs-channels";
    rev    = "2f6440eb09b7e6e3322720ac91ce7e2cdeb413f9";
    sha256 = "0vb7ikjscrp2rw0dfw6pilxqpjm50l5qg2x2mn1vfh93dkl2aan7";
  };

  nixpkgs = import nixpkgsSrc {};

  ghc = nixpkgs.haskell.packages.ghc822.ghcWithPackages (ps: with ps;
        [ ps.hakyll
        ]);

  generator =
    nixpkgs.stdenv.mkDerivation {

      name = "blog-0.1";

      src = ./site.hs;
      LANG = "en_US.UTF-8";
      LOCALE_ARCHIVE = "${nixpkgs.glibcLocales}/lib/locale/locale-archive";

      preUnpack = ''mkdir generator'';

      unpackCmd = ''
        cp $curSrc ./generator/site.hs
        sourceRoot=generator
      '';

      buildInputs = [ ghc ];

      buildPhase = ''
        ghc -dynamic site.hs -o site
      '';

      installPhase = ''
        mkdir -p $out/bin
        cp site $out/bin/generate-site
        '';


};
in

nixpkgs.stdenv.mkDerivation {

  name = "blog-0.1";

  src = nixpkgs.lib.cleanSource ./.;
  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = "${nixpkgs.glibcLocales}/lib/locale/locale-archive";

  buildInputs = [ generator ];

  preConfigure = ''
    export LANG="en_US.UTF-8";
    '';

  buildPhase = ''
    generate-site build
  '';

  installPhase = ''
    cp -r _site $out
  '';

}


