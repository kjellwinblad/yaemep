#!/bin/bash


# From https://stackoverflow.com/questions/59895/get-the-source-directory-of-a-bash-script-from-within-the-script-itself
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"


ERLANG_VERSION=20191023.843
ERLANG_SHA512="4cde89a13eea7b831f74aeee834389eaaeb752c60065e64bf827e259e06018a39513c0c084e3029e784abc6a667b64f7d0579bd498dca885a531db10ebfbe790  erlang-20191023.843.tar"


if ! test -d "$DIR/emacs.d/erlang-$ERLANG_VERSION"
then
    (cd "$DIR/emacs.d" && wget https://melpa.org/packages/erlang-$ERLANG_VERSION.tar)
    if ! (cd "$DIR/emacs.d" && echo $ERLANG_SHA512 | sha512sum --check)
    then
        echo "Error: Checksum does not match (Check the script)"
        exit
    fi
    (cd "$DIR/emacs.d" && tar -xf "erlang-$ERLANG_VERSION.tar")
    (cd "$DIR/emacs.d" && rm -r "erlang" >> /dev/null 2>&1)
    (cd "$DIR/emacs.d" && cp -r "erlang-$ERLANG_VERSION" erlang)
fi


emacs -q --load "$DIR/emacs.d/init.el" "$@"
