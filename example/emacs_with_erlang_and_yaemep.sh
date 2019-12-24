#!/bin/bash


# From https://stackoverflow.com/questions/59895/get-the-source-directory-of-a-bash-script-from-within-the-script-itself
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"


ERLANG_VERSION=20191219.1238
ERLANG_SHA512="aa31bd35dfa12f6c8d97042e00806dd3803cce24e6bebbc98c9e122cfa47019665f3bc35f6312ceb3eb627f8b29426d497daeb02fd0ba59daa7f443ebfb0e935"


ACTUAL_HASH=`(cd "$DIR/emacs.d/erlang" && ls | LC_ALL=C sort  | xargs -n 1 cat | sha512sum | head -n1 | awk '{print $1;}')`

echo hej1 $ERLANG_SHA512
echo hej2 $ACTUAL_HASH

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
