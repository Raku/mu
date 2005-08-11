# This scripts is a loop
# it should be run as
#
#	$ cd /path/to/pugs
#	$ nohup sh util/smoke_loop.sh
#
# and then be restarted if it dies

# This is what it does:
#	- svn up every 300 seconds, and if there's any change
# 		- svn up parrot, if any change
#			- rebuild parrot
#		- regenerate docs
#			- p6bible
#			- haddock
#		- make smoke
#			- if make fails or no ./pugs is produced
#				- make clean, try again
#		- eval $AFTER_CMD

# it tries to parallelize non-competing processes to reduce the smoke loop latency



#### CONFIG VARS ####

# this is where you did 'svn co' for parrot
export PARROT_PATH=/usr/local/src/parrot

# this is run after each smoke
AFTER_CMD='DEST=pasta:.htdocs/pugs_test_status; rsync -avz html/ $DEST/; rsync -avz docs/haddock/ $DEST/haddock/'

# this is the -j param to make and the PUGS_TEST_CONCURRENT value
PARALLEL_PROCS=3

#### CONFIG ENDS ####




export PUGS_EMBED=parrot

while true; do
	while [ `svn up | wc -l` -le 1 ]; do
		sleep 300;
	done

	rm -rf html
	mkdir html

	wait; # finish cleanup
	
	(
		# build and test pugs & parrot
		(
			# build parrot if we need to
			cd $PARROT_PATH
			if [ `svn up | wc -l` -gt 1 ]; then
				perl Configure.pl
				make -s # parrot's make doesn't like -j3
				# make test &
			fi
		)
	
		# try incremental build first
		# if it fails clean and try again
		for clean in 0 1; do
			# create the smoke report
			[ $clean = "1" ] && make clean
			perl Makefile.PL
			nice env PUGS_TESTS_CONCURRENT=$PARALLEL_PROCS make smoke -j$PARALLEL_PROCS -s || continue
			[ -x ./pugs ] || continue # in case make lied
			mv smoke.html html/index.html
			mv tests.yml html
			break
		done
	) &

	(
		# generate docs
		(
			# update documentation
			(cd ../perl6_doc; svn up) &
			(cd ../Perl6-Bible; svn up) &
			wait; # wait for doc updates to finish
		) >/dev/null
		
		# create the doc index
		perl util/catalog_tests.pl > /dev/null
		mv t_index/* html
		rmdir t_index
	) &

	make haddock -s &

	wait # finish everthing

	eval $AFTER_CMD;
done

