#!/bin/bash

# virtual mem: 1 GB
ulimit -v 1048576
# cpu-time: 10 min
ulimit -t 600

ROOT_DIR=/home/design.perl6.org
DEST_DIR=/var/www/design.perl6.org
MU_DIR=$ROOT_DIR/mu
TEST_DIR=$ROOT_DIR/roast
POD_DIR="$ROOT_DIR/specs"

for i in $MU_DIR $TEST_DIR $POD_DIR
do
    if [ ! -d $i ]; then
        git clone https://github.com/perl6/$(basename $i).git $i
    fi
    cd $i
    git fetch
    git reset --hard origin/master
    git log --pretty=%h -1 > .revision
done

cd $MU_DIR

# index file
cp docs/feather/syn_index.html    $DEST_DIR/index.html
# Copy the icons for smartlinks highlighting
cp -ufp docs/feather/hilite-*.png $DEST_DIR/
# And the css file
cp -ufp docs/feather/perl.css     $DEST_DIR/


perl util/smartlinks.pl --out-dir $DEST_DIR --dir $TEST_DIR --css /perl.css --line-anchor --pod-dir $POD_DIR
perl util/podhtm.pl --css /perl.css --url-prefix http://design.perl6.org/ --url-postfix .html --index --charset=UTF-8 --out $DEST_DIR/Differences.html docs/Perl6/Perl5/Differences.pod
