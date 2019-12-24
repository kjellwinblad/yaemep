FROM erlang:22
COPY . /yaemep/
RUN apt-get update
RUN apt-get install -y\
        emacs25-nox\
        elpa-company\
        tmux\
        man\
        git
CMD /yaemep/example/emacs_with_erlang_and_yaemep.sh /yaemep/emacs_erlang_yaemep_support.erl
