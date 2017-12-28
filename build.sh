# ocamlbuild -cflag -annot -cflag -g -lflag -g -use-ocamlfind -pkgs lwt,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml.ppx window.byte sigotonin.byte
#ocamlbuild -cflag -annot -use-ocamlfind -pkgs lwt,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml.ppx sigotonin.byte
# js_of_ocaml window.byte
# js_of_ocaml sigotonin.byte

ocamlbuild -cflags -annot,-g -lflag -g -use-ocamlfind -pkgs lwt,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml.ppx window0.byte sigotonin0.byte
errno=$?
js_of_ocaml --pretty window0.byte 
js_of_ocaml --opt 3 sigotonin0.byte 
exit $errno
# ocamlbuild -use-ocamlfind -cflags -annot,-g -pkgs lwt,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml.ppx -plugin-tag "package(js_of_ocaml.ocamlbuild)" window0.js sigotonin0.js
