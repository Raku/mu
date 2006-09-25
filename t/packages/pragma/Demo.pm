use v6-alpha;

module pragma::Demo-0.01;

sub import (Str $pkg, Int $arg is copy = 0) returns Void {
    my $newarg = [~] $arg.split("").grep:{ $_ ~~ rx:Perl5/\d/ };
#  warn "pragma::Demo::import($arg)\n";
    Pugs::Internals::install_pragma_value($pkg, $newarg);
}

