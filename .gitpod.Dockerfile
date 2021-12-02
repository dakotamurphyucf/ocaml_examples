FROM gitpod/workspace-full

# Install custom tools, runtimes, etc.
# For example "bastet", a command-line tetris clone:
# RUN brew install bastet
#
# More information: https://www.gitpod.io/docs/config-docker/
RUN sudo add-apt-repository -y ppa:avsm/ppa && sudo apt-get update -y && sudo apt-get install -y opam rsync darcs aspcud

USER gitpod

RUN echo '. /home/gitpod/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true' >> /home/gitpod/.bashrc
RUN opam init --disable-sandboxing
RUN eval $(opam env) && opam switch create 4.12.0
RUN eval $(opam env) && opam update
RUN eval $(opam env) && ocaml -version

RUN opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
RUN opam repo add janestreet-bleeding-external https://github.com/janestreet/opam-repository.git#external-packages
RUN opam update

RUN opam install dune base merlin ocamlformat ppx_jane ocaml-lsp-server -y