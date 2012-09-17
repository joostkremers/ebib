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

cp ebib.el "$full_name"
echo "Copied ebib.el to $full_name"
echo

(cd $BASE/manual
    if [ -f html/ebib-manual.html ] ; then
	cp html/ebib-manual.html ../"$full_name"/doc/html/
	cp ebib-manual.css ../"$full_name"/doc/html/
	echo "Copied html manual to $full_name/doc/html/"
    else
	echo Warning: ebib-manual.html not found!
    fi
    if [ -f pdf/ebib-manual.pdf ] ; then
	cp pdf/ebib-manual.pdf ../"$full_name"/doc/
	echo "Copied pdf manual to $full_name/doc/"
    else
	echo Warning: ebib-manual.pdf not found!
    fi
    if [ -f texi/ebib.info ] ; then
	cp texi/ebib.info ../"$full_name"/info
	echo "Copied info manual to $full_name/info"
    else
	echo Warning: ebib.info not found!
    fi
)

(cd $BASE
    cp INSTALL README.md "$full_name"
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
