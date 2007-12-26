die "Please set the %PARROT_PERL6 environment variable to the languages/perl6/ directory in Parrot"
    unless $ENV{PARROT_PERL6};

print <<"EOT";
# please run the following commands to merge back to "perl6"
diff -u -r --unidirectional-new-file --exclude=merge.pl --exclude=perl6.diff $ENV{PARROT_PERL6} . > $ENV{PARROT_PERL6}/kp6.diff

# in the $ENV{PARROT_PERL6} directory:
patch -p0 < kp6.diff
# you may need to re-run Parrot's Makefile.PL first 
make
EOT
