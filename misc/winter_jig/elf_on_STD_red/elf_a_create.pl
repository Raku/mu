#!/usr/bin/perl
# A script to help create and run elf_a .
use strict;
use warnings;

system("(cd elf_a_src/; cat head.pl main.pl) > ./elf_a") == 0 or die $!;

exec("./elf_a",@ARGV);
