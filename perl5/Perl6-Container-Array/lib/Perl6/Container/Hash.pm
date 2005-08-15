
# This is a Perl5 file

# ChangeLog
#
# 2005-08-14
# - added functions clone(), elems(), buckets()

# TODO - 

package Perl6::Container::Hash;

use strict;
# use Perl6::Value;
# use Perl6::Value::List;
# use constant Inf => Perl6::Value::Num::Inf;

sub clone { 
    my $tmp = { %{ $_[0] } };
    $tmp;
}

sub elems {
    my @tmp = %{ $_[0] };
    @tmp / 2
}

sub buckets { scalar %{ $_[0] } }

1;
__END__

=head1 NAME

Perl6::Container::Hash - Perl extension for Perl6 "Hash" class

=head1 SYNOPSIS

  use Perl6::Container::Hash;

  ...

=head1 DESCRIPTION

...


=head1 SEE ALSO

Pugs

=head1 AUTHOR

Flavio S. Glock, E<lt>fglock@Egmail.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Flavio S. Glock

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.


=cut
