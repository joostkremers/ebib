#!/bin/bash

#  Purpose   : pre-commit hook
#  Project   : ebib
#  Commenced : 04-Sep-2012

#  NOTES:
#
#    name file : 'pre-commit.sh'
#    symlink   : ln --symbolic pre-commit.sh .git/hooks/pre-commit
#    then      : place under revision control
#
#    the 'pre-commit' hook is the first of four 'git'
#    hooks that are run prior to each commit
#
#    this script regenerates the info file if the source file for the
#    manual (ebib.text) is newer than ebib.info.

# PREAMBLE

SOURCE="manual/ebib.text"               # markdown source
SOURCE_BBODY="manual/before-body.texi"  # texinfo headers
TEXINFO="manual/ebib.texi"              # texinfo output
INFO="ebib.info"                        # GNU info output
MARKDOWN_GFM="docs/ebib-manual.md"      # Github-flavoured Markdown output
RAW_FILTER="manual/raw.lua"             # Filter for handling {{raw}} directives

SCRIPT=$(basename "$0")

E_SUCCESS=0
E_FAILURE=1

# FUNCTIONS

function confirm_file
{
    local file="$1"
    test -f "$file" && return 0
    echo "$SCRIPT: regular file not found: $file"
    let "errors++"
    return 1
}

function create_texi
{
    local source="$1"
    echo "$SCRIPT: running pandoc to create texinfo"
    pandoc --read=markdown \
           --write=texinfo \
           --output="$TEXINFO" \
           --include-before-body="$SOURCE_BBODY" \
           --lua-filter="$RAW_FILTER" \
           --standalone \
           --table-of-contents \
           "$1" && return 0
    echo "$SCRIPT: pandoc -w texinfo failed"
    let "errors++"
    return 1
}

function run_makeinfo
{
    local source="$1"
    echo "$SCRIPT: runnig makeinfo"
    makeinfo "$1" && return 0 # makeinfo puts the output file in the current dir
    echo "$SCRIPT: makeinfo run failed"
    let "errors++"
    return 1
}


function create_gfm
{
    local source="$1"
    echo "$SCRIPT: running pandoc to create gfm"
    pandoc --read=markdown \
           --write=gfm \
           --lua-filter="$RAW_FILTER" \
           --output="$MARKDOWN_GFM" \
           "$1" && return 0
    echo "$SCRIPT: pandoc -w gfm failed"
    let "errors++"
    return 1
}

function check_exit
{
    local errors="$1"
    test "$errors" -eq 0 && return 0
    echo "$SCRIPT: fatal: exit status $E_FAILURE"
    exit $E_FAILURE
}

# ACTIVE code

errors=0

confirm_file "$SOURCE"
confirm_file "$INFO"
check_exit   "$errors"
if [ "$(git status --porcelain manual/ebib.text)" == 'M  manual/ebib.text' ] ; then
    echo "$SCRIPT: regenerating documentation files"
    git stash -q --keep-index
    create_texi "$SOURCE" && run_makeinfo "$TEXINFO" && git add "$INFO"
    create_gfm "$SOURCE" && git add "$MARKDOWN_GFM"
    git stash pop -q
fi
check_exit   "$errors"

echo "$SCRIPT: documentation is current"
exit $E_SUCCESS

# end of file
