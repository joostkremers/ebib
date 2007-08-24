#!/bin/bash

BASE=$(pwd)

if [ "$1" = "" ] ; then
    echo "Usage: make-ebib-release.sh <version_number>"
    exit 1
fi

version="$1"
full_name=ebib-"$version"

mkdir "$BASE/$full_name"
echo "Created $BASE/$full_name"
echo

(cd $BASE/src
    cp ebib.el ChangeLog ../"$full_name"
    echo "Copied ebib.el and ChangeLog to $full_name"
    echo
)

(cd $BASE/manual
    mv ebib-manual.texinfo ebib-manual.texinfo.source
    sed -e "s/==VERSION==/$version/" -e "s/==DATE==/$(date +'%d %B %Y')/" \
	ebib-manual.texinfo.source > ebib-manual.texinfo
    rm ebib-manual.texinfo.source
    echo "Creating ebib.info"
    makeinfo ebib-manual.texinfo
    echo
    echo "Creating html manual"
    rm -rf ebib/
    makeinfo --html ebib-manual.texinfo
    echo
    echo "Creating pdf manual"
    texi2dvi ebib-manual.texinfo
    dvips -Ppdf -t a4 ebib-manual.dvi
    ps2pdf ebib-manual.ps
    cp -a ebib.info ebib-manual.pdf ebib/ ../"$full_name"
    echo "Copied manual to $full_name"
    echo
)

(cd $BASE/release-files
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
