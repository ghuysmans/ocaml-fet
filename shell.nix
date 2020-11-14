with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "x";
  XDG_DATA_DIRS = ["${gtk3}/share/gsettings-schemas/${gtk3.name}"];
  buildInputs = with ocamlPackages; [
    ocaml
    dune_2
    findlib
    lablgtk3
    ptime
    cmdliner
    vim
  ];
  #for grep!
  gtk3 = with ocamlPackages.lablgtk3;
    "${out}/lib/ocaml/${ocaml.version}/site-lib/${pname}";
}
