{
  description = "My site builder";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    denv = {
      url = "github:iliayar/env.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    iosevka = {
      url =
        "file+https://github.com/be5invis/Iosevka/releases/download/v30.1.2/PkgWebFont-Iosevka-30.1.2.zip";
      flake = false;
    };

    katex = {
      url =
        "file+https://github.com/KaTeX/KaTeX/releases/download/v0.16.10/katex.tar.gz";
      flake = false;
    };

    mermaid = {
      url =
        "file+https://cdn.jsdelivr.net/npm/mermaid@10.9.1/dist/mermaid.min.js";
      flake = false;
    };

  };

  outputs = inputs@{ flake-parts, denv, iosevka, katex, mermaid, ... }:
    flake-parts.lib.mkFlake { inputs = denv.inputs; } {
      imports = [ denv.flakeModules.default ];
      systems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, lib, ... }:
        let site-lib = import ./lib { inherit pkgs lib; };
        in {
          denvs.default = {
            langs.haskell = {
              enable = true;
              extraPackages = hpkgs:
                with hpkgs; [
                  hakyll
                  hakyll-sass
                  pandoc
                  pathwalk
                  site-lib
                ];
            };
            denv.packages = with pkgs; [
              haskellPackages.hakyll
              (python3.withPackages (pypkgs: with pypkgs; [ pygments ]))
              texliveFull
            ];
          };

          packages = let
            thirdparty = pkgs.runCommand "thirdparty" {
              buildInputs = with pkgs; [ unzip ];
            } ''
              mkdir -p $out

              # Iosevka
              mkdir $out/iosevka && cd $out/iosevka && unzip ${iosevka}

              # Katex
              cd $out && tar -xvf ${katex}

              # Mermaid
              cp ${mermaid} $out/mermaid.min.js
            '';

            makeBuilderUnwrapped = srcPath:
              pkgs.haskellPackages.mkDerivation {
                pname = "site";
                version = "1.0.0";
                src = srcPath;

                isExecutable = true;

                buildDepends = with pkgs.haskellPackages; [
                  hakyll
                  hakyll-sass
                  site-lib
                ];

                license = lib.licenses.wtfpl;
              };

            org-to-pdf = import ./org-to-pdf { inherit pkgs lib; }; 

            makeBuilder = path:
              let siteUnwrapped = makeBuilderUnwrapped path;
                  binDeps = [ org-to-pdf pkgs.typst ];
              in pkgs.runCommand "site" {
                buildInputs = with pkgs; [ makeWrapper ];
              } ''
                makeWrapper ${siteUnwrapped}/bin/site $out/bin/site \
                    --set THIRDPARTY_PATH "${thirdparty}" \
                    --set LOCALE_ARCHIVE "${pkgs.glibcLocales}/lib/locale/locale-archive" \
                    --prefix PATH : "${lib.makeBinPath binDeps}"
              '';
          in { 
             org-to-pdf = org-to-pdf;
             main-site = makeBuilder ./main; 
             conspects = makeBuilder ./conspects; 
          };
        };
      flake = { };
    };
}
