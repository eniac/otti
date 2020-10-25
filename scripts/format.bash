#!/usr/bin/env bash
set -e

USAGE="
$0 SINCE_REF DIR:

Format all Haskell files changed since SINCE_REF under DIR.
If SINCE_REF is 'all', then formats all Haskell files under DIR.
"

REF=${1?"$USAGE"}
DIR=${2?"$USAGE"}
FORMATTER=brittany

[ -a $DIR ] || (echo "$DIR does not exist" && exit 1)

function check_for() {
    which $1 > /dev/null 2> /dev/null || (echo "Please install $1" && exit 1)
}

check_for $FORMATTER
check_for git
check_for xargs
check_for grep

if [ "all" == $REF ];
then
    echo "Finding all Haskell files in $DIR"
    [ -z $(git ls-files --others --exclude-standard $DIR | grep '\.hs') ] || (echo "Will not format with untracked *.hs files" && exit 1)
    files=$(git ls-files $DIR | grep '\.hs')
else
    mb=$(git merge-base HEAD $REF)
    echo "The common ancestor of HEAD and $REF is $mb"
    echo "Finding all Haskell files in $DIR that have changed since $mb"
    files=$(git diff --name-only $mb -- $DIR | (grep '\.hs' || true))
fi

for f in $files
do
    git diff --quiet -- $f || (echo "Unstaged changes in $f" && exit 1)
done

echo Formatting:
for f in $files
do
    echo "  * $f"
    $FORMATTER --write-mode=inplace $f || echo "Skipping"
done
