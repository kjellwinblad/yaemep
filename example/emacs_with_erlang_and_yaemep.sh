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
ERLANG_SHA512="df6c55e23c3e239f8226aa0901b6c50424b5dc33dc0a027eaa79c29097ecb8623ad3cf4c4d49f4cfbac01b8caa355a2a2fdd5dc47795a5327486186e2fac8e6c"


if ! test -d "$DIR/emacs.d/erlang"
then
    (cd "$DIR/emacs.d" && wget https://melpa.org/packages/erlang-$ERLANG_VERSION.tar)
    (cd "$DIR/emacs.d" && tar -xf "erlang-$ERLANG_VERSION.tar")
    (cd "$DIR/emacs.d" && rm -r "erlang" >> /dev/null 2>&1)
    (cd "$DIR/emacs.d" && cp -r "erlang-$ERLANG_VERSION" erlang)
fi

ACTUAL_HASH=`find "$DIR/emacs.d/erlang" -type f -print0 | sort -z | xargs -0 sha512sum | sha512sum | head -n1 | awk '{print $1;}'`

#echo "$ACTUAL_HASH"
#echo "$ERLANG_SHA512"

if ! (test "$ACTUAL_HASH" = "$ERLANG_SHA512")
then
    echo "Error: Checksum does not match (Check the script)"
    exit
fi


emacs -q --load "$DIR/emacs.d/init.el" "$@"
