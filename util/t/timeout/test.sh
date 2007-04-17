#!/bin/sh

timeout='perl ../../timeout.pl'

./full.pl 10 &

echo 'full: 00:00:01 <=> 0'
$timeout -r './full\.pl' -t '00:00:01'

sleep 1
echo 'full: 00:00:01 <=> 1'
$timeout -r './full\.pl' -t '00:00:01'

echo 'full: 00:00:00'
$timeout -r './full\.pl' -t '00:00:00'

echo 'indeed killed'
$timeout -r './full\.pl' -t '00:00:00'

echo '==='

./half.pl 10 &

sleep 1
echo 'half: 00:00:01 <=> 0'
$timeout -r './half\.pl' -t '00:00:01'

sleep 2
echo 'half: 00:00:01 <=> 3'
$timeout -r './half\.pl' -t '00:00:01'

echo 'half: 00:00:00'
$timeout -r './half\.pl' -t '00:00:00'

echo 'indeed killed'
$timeout -r './half\.pl' -t '00:00:00'

