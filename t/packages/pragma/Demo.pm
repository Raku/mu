module pragma::Demo-0.01;

sub import (Str $pkg, Int ?$arg = 0 is copy) returns Void {
    my $newarg = [~] $arg.split("").grep:{ $_ ~~ rx:Perl5/\d/ };
#  warn "pragma::Demo::import($arg)\n";
    Pugs::Internals::install_pragma_value($pkg, $newarg);
}

