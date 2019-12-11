FROM erlang:22
RUN apt-get update
RUN apt-get install -y\
        emacs-nox\
        elpa-company\
        tmux\
        man\
        git
ENV user yaemep
RUN useradd -ms /bin/bash ${user} && chown -R ${user} /home/${user}
COPY ./example/emacs.d /home/yaemep/.emacs.d
COPY . /the/path/to/your/yaemep/
USER yaemep
CMD emacs /the/path/to/your/yaemep/emacs_erlang_yaemep_support.erl
