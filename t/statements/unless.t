use v6-alpha;

use Test;

=kwid

Basic "unless" tests

=cut

plan 14;

# L<S04/Conditional statements/unless statements 
#   work as in Perl 5>

my $x = 'test';
unless $x ne $x { pass('unless $x ne $x {} works');  }
else            { flunk('unless $x ne $x {} failed'); }

unless $x eq $x { flunk('unless $x eq $x {} failed'); }
else              { pass('unless $x eq $x {} works');  }

unless 0 { pass("unless 1 {} works");  }
else     { flunk("unless 1 {} failed"); }

unless 1 { flunk("unless 0 {} failed"); }
else     { pass("unless 0 {} works");  }

unless undef { pass("unless undef {} works");  }
else         { flunk("unless undef {} failed"); }

# with paratheses
unless ($x ne $x) { pass("unless ($x ne $x) {} works");  }
else              { flunk("unless ($x ne $x) {} failed"); }

unless (5+2) { flunk("unless (5+2) {} failed"); }
else         { pass("unless (5+2) {} works");  }

# die called in the condition part of an if statement should die immediately
# rather than being evaluated as a boolean
my $foo = 1;
try { unless (die "should die") { $foo = 3 } else { $foo = 2; } };
#say '# $foo = ' ~ $foo;
is $foo, 1, "die should stop execution immediately.";

# unless...elsif

{
    my $foo = 1;
    unless 1 { $foo = 2 } elsif 1 { $foo = 3 };
    is $foo, 3, 'unless 1 {} elsif 1 {}';
}

{
    my $foo = 1;
    unless 1 { $foo = 2 } elsif 0 { $foo = 3 };
    is $foo, 1, 'unless 1 {} elsif 0 {}';
}

{
    my $foo = 1;
    unless 0 { $foo = 2 } elsif 1 { $foo = 3 };
    is $foo, 2, 'unless 0 {} elsif 1 {}';
}

{
    my $foo = 1;
    unless 0 { $foo = 2 } elsif 0 { $foo = 3 };
    is $foo, 2, 'unless 0 {} elsif 0 {}';
}

# unless...elsif...else

{
    my $foo = 1;
    unless 0 { $foo = 2 } elsif 0 { $foo = 3 } else { $foo = 4 };
    is $foo, 2;
}

{
    my $foo = 1;
    unless 1 { $foo = 2 } elsif 0 { $foo = 3 } else { $foo = 4 };
    is $foo, 4;
}

{
    my $foo = 1;
    unless 1 { $foo = 2 } elsif 1 { $foo = 3 } else { $foo = 4 };
    is $foo, 3;
}

{
    my $foo = 1;
    unless 0 { $foo = 2 } elsif 1 { $foo = 3 } else { $foo = 4 };
    is $foo, 2;
}
