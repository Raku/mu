# Run this script, then create buildmaster according to directions @
# http://buildbot.sourceforge.net/manual-0.7.5.html#Creating-a-buildmaster
# Look at master.cfg for port and slave configuration
if [[ ! $PREFIX ]]
then
  PREFIX=/usr/local
fi
echo "PREFIX is $PREFIX"

LOG=/tmp/buildbot.log
cd /tmp
rm -rf zope.interface-3.3.0
wget http://www.zope.org/Products/ZopeInterface/3.3.0/zope.interface-3.3.0.tar.gz -O - | tar -xz
cd zope.interface-3.3.0
  echo "zope.interface install" | tee $LOG && \
  python setup.py install >> $LOG
cd ..

rm -rf Twisted-2.5.0
wget http://tmrc.mit.edu/mirror/twisted/Twisted/2.5/Twisted-2.5.0.tar.bz2 -O - | tar -xj

cd Twisted-2.5.0
  echo "Twisted-2.5.0 install (sudo)" | tee -a $LOG && \
  sudo python setup.py install >> $LOG # had problems installing w/o su perms
cd ..

rm -rf buildbot-0.7.5
wget http://downloads.sourceforge.net/buildbot/buildbot-0.7.5.tar.gz -O - | tar -xz
cd buildbot-0.7.5
  echo "buildbot-0.7.5 install (sudo)" | tee -a $LOG && \
  sudo python setup.py install >> $LOG # had problems installing w/o su perms
  echo 'import sys, os.path' > find_buildbot.py
  echo 'if os.path.exists(os.path.join( sys.prefix, "bin", "buildbot" ) ):' >> find_buildbot.py
  echo '    print os.path.join( sys.prefix, "bin", "buildbot" )' >> find_buildbot.py

  echo "Creating symlink in $PREFIX" | tee -a $LOG && \
  echo ln -s `python find_buildbot.py` $PREFIX/bin/buildbot

cd ..
