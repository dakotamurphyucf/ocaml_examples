FROM gitpod/workspace-full

# Install custom tools, runtimes, etc.
# For example "bastet", a command-line tetris clone:
# RUN brew install bastet
#
# More information: https://www.gitpod.io/docs/config-docker/
RUN sudo add-apt-repository -y ppa:avsm/ppa && sudo apt-get update -y && sudo apt-get install -y opam rsync darcs aspcud openssh openssl-dev libffi-dev zlib-dev pcre gmp-dev pcre-dev

USER gitpod

RUN echo '. /home/gitpod/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true' >> /home/gitpod/.bashrc
RUN opam init --disable-sandboxing
RUN eval $(opam env) && opam switch create 4.12.0
RUN eval $(opam env) && opam update
RUN eval $(opam env) && ocaml -version

RUN opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
RUN opam repo add janestreet-bleeding-external https://github.com/janestreet/opam-repository.git#external-packages
RUN opam update

RUN opam pin add httpaf https://github.com/anmonteiro/httpaf/httpaf.opam
RUN opam pin add httpaf-async https://github.com/anmonteiro/httpaf/httpaf-async.opam
RUN opam pin add websocketaf https://github.com/anmonteiro/websocketaf/websocketaf.opam
RUN opam pin add websocketaf-async https://github.com/anmonteiro/websocketaf/websocketaf-async.opam

RUN opam install -y dune base core async merlin ocamlformat \ 
ppx_jane ppx_log ppx_expect pythonlib ppx_bin_prot ppx_csv_conv \
ppx_python ppx_sexp_value ppx_sexp_message ppx_yojson_conv ppx_import ocaml-lsp-server utop uri httpaf websocketaf websocketaf-async digestif -y
RUN eval $(opam env)