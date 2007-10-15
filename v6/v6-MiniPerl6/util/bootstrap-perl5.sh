rm -rf lib5-new
echo 'Compiling with old version'
. util\build-perl5.sh
rm -rf lib5
mv lib5-new lib5
echo 'Compiling with new version'
. util\build-perl5.sh
rm -rf lib5
mv lib5-new lib5
echo 'Finished'
