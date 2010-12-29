#!/bin/bash

BASE=$(pwd)

if [ "$1" = "" ] ; then
    echo "Usage: make-ebib-release.sh <version_number>"
    exit 1
fi

version="$1"
full_name=ebib-"$version"

mkdir "$BASE/$full_name"
mkdir -p "$BASE/$full_name/doc/html"
mkdir "$BASE/$full_name/info"
echo "Created $BASE/$full_name and subdirectories"
echo

(cd $BASE/src
    cp ebib.el ChangeLog ../"$full_name"
    echo "Copied ebib.el and ChangeLog to $full_name"
    echo
)

(cd $BASE/manual
    if [ -f html/ebib-manual.html ] ; then
	mv html/ebib-manual.html ../"$full_name"/doc/html/
	cp ebib-manual.css ../"$full_name"/doc/html/
	echo "Moved html manual to $full_name/doc/html/"
    else
	echo Warning: ebib-manual.html not found!
    fi
    if [ -f pdf/ebib-manual.pdf ] ; then
	mv pdf/ebib-manual.pdf ../"$full_name"/doc/
	echo "Moved pdf manual to $full_name/doc/"
    else
	echo Warning: ebib-manual.pdf not found!
    fi
    if [ -f texi/ebib-manual.info ] ; then
	mv texi/ebib-manual.info ../"$full_name"/info
	echo "Moved info manual to $full_name/info"
    else
	echo Warning: ebib-manual.info not found!
    fi
)

(cd $BASE
    cp INSTALL README ../"$full_name"
    echo "Copied release files to $full_name"
    echo
)

(cd $BASE/"$full_name"
    echo "$version" > VERSION
    mv ebib.el ebib.el.source
    sed "s/==VERSION==/$version/" ebib.el.source > ebib.el
    rm ebib.el.source
    echo created VERSION
    echo
)

(cd $BASE
    tar czf "$full_name".tar.gz "$full_name"
    echo "Created $full_name.tar.gz"
    echo
)

echo "Ebib release $version created."
echo
