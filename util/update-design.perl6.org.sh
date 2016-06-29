#!/bin/bash

# virtual mem: 1 GB
ulimit -v 1048576
# cpu-time: 10 min
ulimit -t 600

ROOT_DIR=$HOME
MU_DIR=$ROOT_DIR/mu
TEST_DIR=$ROOT_DIR/roast
POD_DIR="$ROOT_DIR/specs"
UPDATED=''

for i in $MU_DIR $TEST_DIR $POD_DIR
do
    if [ ! -d $i ]; then
        git clone https://github.com/perl6/$(basename $i).git $i
        UPDATED=1
    fi
    cd $i
    git fetch
    before=$(git rev-parse HEAD)
    git reset --hard origin/master
    after=$(git rev-parse HEAD)
    if [ "$before" != "$after" ]
    then UPDATED=1
    fi
    git log --pretty=%h -1 > .revision
done

[ "$UPDATED" ] || exit

DEST_DIR=$(mktemp -d)

cd $MU_DIR

perl util/smartlinks.pl --out-dir $DEST_DIR --dir $TEST_DIR --css /perl-with-historical-message.css --line-anchor --pod-dir $POD_DIR || exit 1
perl util/podhtm.pl --css /perl-with-historical-message.css --url-prefix http://design.perl6.org/ --url-postfix .html --index --charset=UTF-8 --out $DEST_DIR/Differences.html docs/Perl6/Perl5/Differences.pod

source /home/rakudobrew/rakudobrew-bash

cd $POD_DIR
cp html/* $DEST_DIR/
grep -vE '^(#.*|\s*)$' pod6-files | while read LINE
do
    INPUT=$(echo $LINE | cut -d ' ' -f 1)
    OUTPUT=$(echo $LINE | cut -d ' ' -f 2)
    TEMPFILE=$(tempfile --mode=0644)
    perl6-m  -MMONKEY-SEE-NO-EVAL -MPod::To::HTML -e 'put pod2html EVAL slurp(@*ARGS.shift) ~ "\n; \$=pod", :css-url</perl-with-historical-message.css>' "$INPUT" > "$TEMPFILE" && mv "$TEMPFILE" "$DEST_DIR/$OUTPUT" \
        || rm -f "$TEMPFILE"
done

if [ -z "$NOSSH" ]
then
    chmod 755 "$DEST_DIR"
    rsync -az --delete $DEST_DIR/ design.perl6.org@www:/var/www/design.perl6.org/
fi
if [ -z "$KEEPTMP" ]
then
    rm -rf "$DEST_DIR"
fi
