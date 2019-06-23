# ocamlbuild -cflag -annot -cflag -g -lflag -g -use-ocamlfind -pkgs lwt,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml.ppx window.byte sigotonin.byte
#ocamlbuild -cflag -annot -use-ocamlfind -pkgs lwt,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml.ppx sigotonin.byte
# js_of_ocaml window.byte
# js_of_ocaml sigotonin.byte

ocamlbuild -cflags -annot,-g -lflag -g -use-ocamlfind -pkgs lwt,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml.ppx sigotoninEasy.byte sigotoninStd.byte canvas.byte
#ocamlbuild -cflags '-annot,-g,-w @10' -lflag -g -use-ocamlfind -pkgs base,lwt,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml.ppx,ppx_deriving,ppx_typerep_conv,typerep ayameKatugi.byte
#ocamlbuild -cflags -annot,-g -lflag -g -use-ocamlfind -pkgs netcgi2,unmagic,ppx_deriving,ppx_typerep_conv,typerep remoteKekoro.native
errno=$?


#js_of_ocaml --pretty window0.byte
#js_of_ocaml --pretty canvas.byte
#js_of_ocaml --pretty ken.byte 
#js_of_ocaml --pretty cai.byte
#js_of_ocaml --pretty deori.byte
#js_of_ocaml --opt 3 sigotoninEasy.byte
#js_of_ocaml --opt 3 sigotoninStd.byte
#js_of_ocaml --opt 3 kaga.byte
js_of_ocaml --opt 3 +base/runtime.js ayameKatugi.byte 
#js_of_ocaml --opt 3 determinizeWorker.byte
exit $errno
# ocamlbuild -use-ocamlfind -cflags -annot,-g -pkgs lwt,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml.ppx -plugin-tag "package(js_of_ocaml.ocamlbuild)" window0.js sigotonin0.js
