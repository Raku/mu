package v6;
use Filter::Simple sub {
    my($this_class,$first_arg,@rest)=@_;
    my ($caller, $file, $line) = caller(1);
    $line++;
    my $location = qq{\n\#line $line "$file"\n};

    my $pkg = (do{ no warnings; $v6::implementation }
	       || $ENV{PERL5_V6_IMPLEMENTATION}
	       || 'Pugs::Compiler::Perl6::v6');

    my $cmd = ("use ".$pkg.$first_arg
	       .(@rest ? ",qw(".join(" ",@rest).")" : '')
	       .";");
    s/^(use v6-alpha;)/\# $1/
;#	or die "Couldn't find the leading 'use v6-alpha;' to delete it.";
    $_ = $cmd.$location.$_;
#    print STDERR $_,"\n"; $_;
};
1;
__END__
