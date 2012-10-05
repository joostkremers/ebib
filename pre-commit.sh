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
#    this script currently blocks a commit if the
#    documentation has not been regenerated correctly,
#    as determined by the file modification times
#
#    a more sophisticated approach would invoke
#    pandoc to create the documentation, perhaps with
#    the aid of a makefile

# PREAMBLE

PANDOC="manual/ebib.text"               # markdown source
PANDOC_BBODY="manual/texi-before-body"  # texinfo headers
TEXINFO="manual/texi/ebib.texi"         # texinfo output
INFO="ebib.info"                        # GNU info output

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

function run_pandoc
{
    local source="$1"
    pandoc --read=markdown \
           --write=texinfo \
           --output="$TEXINFO" \
           --include-before-body="$PANDOC_BBODY" \
           --standalone \
           --table-of-contents \
           "$1" && return 0
    echo "$SCRIPT: pandoc run failed"
    let "errors++"
    return 1
}

function run_makeinfo
{
    local source="$1"
    makeinfo "$1" && return 0 # makeinfo puts the output file in the current dir
    echo "$SCRIPT: makeinfo run failed"
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

confirm_file "$PANDOC"
confirm_file "$INFO"
check_exit   "$errors"
if [  "$INFO" -ot "$PANDOC" ] ; then
    echo "$SCRIPT: regenerating info file"
    git stash -q --keep-index
    run_pandoc "$PANDOC" && run_makeinfo "$TEXINFO" && git add "$INFO"
    git stash pop -q
fi
check_exit   "$errors"

echo "$SCRIPT: documentation is current"
exit $E_SUCCESS

# end of file
