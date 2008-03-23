#!/usr/bin/perl
# A script to help create and run elf_a .
use strict;
use warnings;

my $files = join(" ",map{"elf_b_src/$_"}qw( main.p6 ));
system("./elf_a_create.pl --create-only") == 0 or die $!;
system("./elf_a -x -o ./elf_b $files") == 0 or die $!;

exec("./elf_b",@ARGV);
