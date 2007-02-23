package v6;
use Filter::Simple sub {
    my($this_class,$first_arg,@rest)=@_;
    my ($caller, $file, $line) = caller(1);
    $line++;
    my $location = qq{\n\#line $line "$file"\n};

    my $pkg = ($ENV{PERL5_V6_IMPLEMENTATION}
	       || (defined(%v6::implementation_version)
		   && $v6::implementation_version{$_[0]})
	       || do{ no warnings; $v6::implementation }
	       || 'Pugs::Compiler::Perl6::v6');

    my $cmd = ("use ".$pkg.$first_arg
	       .(@rest ? ",qw(".join(" ",@rest).")" : '')
	       .";\n");
    $_ = $cmd.$location.$_;
};
1;
__END__
