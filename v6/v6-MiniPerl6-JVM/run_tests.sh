for f in `ls t/*.t` ; do echo $f ; perl mp6-jvm.pl < $f > x ; ~/projetos/groovy/groovy-1.0/bin/groovy x ; done
