{ pkgs, lib, ... }:
let
  configs = pkgs.runCommand "emacs-configs" { src = ./.; } ''
    mkdir -p $out

    ls -la
    cp $src/init.el $out/init.el
    cp $src/preamble.sty $out/preamble.sty
    cp $src/script.el $out/script.el
  '';

  emacsPkgs = pkgs.emacsWithPackages
    (epkgs: with epkgs; [ haskell-mode ]);

  emacsUnwrapped = pkgs.writeShellScriptBin "emacs" ''
    export PREAMBLE_PATH="${configs}/preamble.sty"
    ${emacsPkgs}/bin/emacs --batch -l ${configs}/init.el "$@"
  '';

  fontconfig = pkgs.makeFontsConf {
    fontDirectories = with pkgs; [ freefont_ttf ];
  };

  emacs = let binDeps = with pkgs; [ texliveFull ];
  in pkgs.runCommand "emacs" { buildInputs = with pkgs; [ makeWrapper ]; } ''
    makeWrapper ${emacsUnwrapped}/bin/emacs $out/bin/emacs \
        --prefix PATH : "${lib.makeBinPath binDeps}" \
  '';
in pkgs.writeShellScriptBin "org-to-pdf" ''
  ${emacs}/bin/emacs -l ${configs}/script.el -- "$@"
''
