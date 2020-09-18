#!/usr/bin/bash
set -e

USAGE="$0 PATH_TO_DIR"

DIR=${1?$USAGE}
FORMATTER=brittany

[ -a $DIR ] || (echo "$DIR does not exist" && exit 1)

function check_for() {
    which $1 > /dev/null 2> /dev/null || (echo "Please install $1" && exit 1)
}

check_for $FORMATTER
check_for git
check_for xargs
check_for grep

git diff --quiet $DIR || (echo "Will not format with unstaged modifications" && exit 1)

[ -z $(git ls-files --others --exclude-standard $DIR | grep '\.hs') ] || (echo "Will not format with untracked *.hs files")

(git ls-files --others --exclude-standard $DIR && git ls-files $DIR) | grep '\.hs' | xargs $FORMATTER --write-mode=inplace
