FROM ocaml/opam:alpine-ocaml-4.12@sha256:97d4da55048befbda12adc18765b049a9eb53a878bcce58dbfecb06f363273e8

USER root
RUN sed -i 's-/home/opam:/sbin/nologin-/home/opam:/bin/bash-' /etc/passwd
RUN apk add git inotify-tools openssh openssl-dev libffi-dev zlib-dev pcre neovim gmp-dev pcre-dev

USER opam

RUN opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
RUN opam repo add janestreet-bleeding-external https://github.com/janestreet/opam-repository.git#external-packages
RUN opam update


RUN opam install -y dune base merlin ocamlformat ocaml-lsp-server utop

RUN opam pin add faraday https://github.com/dakotamurphyucf/faraday.git -y
RUN opam pin add faraday-async https://github.com/dakotamurphyucf/faraday.git -y
RUN opam pin add gluten https://github.com/anmonteiro/gluten.git -y
RUN opam pin add gluten-async https://github.com/anmonteiro/gluten.git -y
RUN opam pin add httpaf https://github.com/dakotamurphyucf/httpaf.git -y
RUN opam pin add httpaf-async https://github.com/dakotamurphyucf/httpaf.git -y
RUN opam pin add websocketaf https://github.com/dakotamurphyucf/websocketaf.git -y
RUN opam pin add websocketaf-async https://github.com/dakotamurphyucf/websocketaf.git -y
RUN opam pin https://github.com/dakotamurphyucf/ocaml-caqti.git -y -n
RUN opam install -y core async ppx_jane ppx_log ppx_expect pythonlib ppx_bin_prot ppx_csv_conv ppx_python ppx_sexp_value ppx_sexp_message ppx_yojson_conv uri httpaf-async websocketaf-async ppx_rapper_async caqti-driver-postgresql
RUN eval $(opam env)