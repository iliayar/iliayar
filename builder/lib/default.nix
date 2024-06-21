{pkgs, lib, ... }:
pkgs.haskellPackages.mkDerivation {
    pname = "site-lib";
    version = "1.0.0";
    src = ./.;

    isExecutable = false;

    buildDepends = with pkgs.haskellPackages; [
        hakyll
        hakyll-sass
        pathwalk
    ];

    license = lib.licenses.wtfpl;
}
