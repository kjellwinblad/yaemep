FROM erlang:22
RUN apt-get update
RUN apt-get install -y\
        emacs25-nox\
        elpa-company\
        tmux\
        man\
        git
COPY . /yaemep/
CMD /yaemep/example/emacs_with_erlang_and_yaemep.sh -bg black  /yaemep/emacs_erlang_yaemep_support.erl
