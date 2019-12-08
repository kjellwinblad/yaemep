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
ERLANG_SHA512="74298ff9e9175fc4972354268773915bb6206ce442aa49f93e01c72c77cfa8d7052599c94ae7c32980e06d1351aac42541fbd411daa2e8b05836b2826b5b4f21"


if ! test -d "$DIR/emacs.d/erlang"
then
    (cd "$DIR/emacs.d" && wget https://melpa.org/packages/erlang-$ERLANG_VERSION.tar)
    (cd "$DIR/emacs.d" && tar -xf "erlang-$ERLANG_VERSION.tar")
    (cd "$DIR/emacs.d" && rm -r "erlang" >> /dev/null 2>&1)
    (cd "$DIR/emacs.d" && cp -r "erlang-$ERLANG_VERSION" erlang)
fi

ACTUAL_HASH=`find "$DIR/emacs.d/erlang" -type f -print0 | sort -z | xargs -0 cat | sha512sum | head -n1 | awk '{print $1;}'`

echo "$ACTUAL_HASH"
echo "$ERLANG_SHA512"

if ! (test "$ACTUAL_HASH" = "$ERLANG_SHA512")
then
    echo "Error: Checksum does not match (Check the script)"
    exit
fi


emacs -q --load "$DIR/emacs.d/init.el" "$@"
