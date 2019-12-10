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
ERLANG_SHA512="796b3e66db1477614d3fdfc900fc1df288f32e8092d8cafc7d9c7950c40af255612d2b18b79b45d85ef317a67cd12251390b14e776ea0316344e76b7f8d778fc"


ACTUAL_HASH=`(cd "$DIR/emacs.d/erlang" && ls | LC_ALL=C sort  | xargs -n 1 cat | sha512sum | head -n1 | awk '{print $1;}')`

if ! (test "$ACTUAL_HASH" = "$ERLANG_SHA512")
then
    if test -d "$DIR/emacs.d/erlang"
    then
        rm -rf "$DIR/emacs.d/erlang"
    fi
fi

if ! test -d "$DIR/emacs.d/erlang"
then
    (cd "$DIR/emacs.d" && wget https://melpa.org/packages/erlang-$ERLANG_VERSION.tar)
    (cd "$DIR/emacs.d" && tar -xf "erlang-$ERLANG_VERSION.tar")
    (cd "$DIR/emacs.d" && rm -r "erlang" >> /dev/null 2>&1)
    (cd "$DIR/emacs.d" && cp -r "erlang-$ERLANG_VERSION" erlang)
fi

ACTUAL_HASH=`(cd "$DIR/emacs.d/erlang" && ls | LC_ALL=C sort  | xargs -n 1 cat | sha512sum | head -n1 | awk '{print $1;}')`

if ! (test "$ACTUAL_HASH" = "$ERLANG_SHA512")
then
    echo "Error: Checksum does not match (Check the script)"
    exit
fi


emacs -q --load "$DIR/emacs.d/init.el" "$@"
