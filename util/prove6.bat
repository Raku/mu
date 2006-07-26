@echo off
setlocal
set PERL5LIB=blib6/pugs/perl5/lib;blib6/pugs/perl5/arch
set PERL6LIB=blib6/lib
prove %*
