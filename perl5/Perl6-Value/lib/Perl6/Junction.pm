# Junction.pm

# ChangeLog
#
# 2005-09-27
# * created this file
#

# TODO
# 

use strict;

use Perl6::Value;

my $class_description = '-0.0.1-cpan:FGLOCK';

class1 'Junction'.$class_description => {
    is => [ $::Object ],
    instance => {
        attrs => [ '$.type', '$.values' ],
                # type - unboxed str
                # values - ptr to native ARRAY of objects
        methods => {
            'num' =>  sub { warn "Junction.num() not implemented" },
            'int' =>  sub { warn "Junction.int() not implemented" },
            'str' =>  sub { Str->new( '$.unboxed' => Perl6::Value::Num::to_str( 
                    _('$.type') . "(" . join(", ", @{_('$.values')} ) . ")"
                ) ) 
            },
            'bit' =>  sub { warn "Junction.bit() not implemented" },
            'perl' => sub { $::SELF->str },
            'ref' =>  sub { $::CLASS }, 
        },
    }
};



1;
__END__

=head1 NAME

Perl6::Value::Junction - Perl6 junctions

=head1 SYNOPSIS

  use Perl6::Value::Junction;
  
  ...

=head1 DESCRIPTION


=head1 SEE ALSO

Pugs

=head1 AUTHOR

Flavio S. Glock, E<lt>fglock@gmail.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Flavio S. Glock

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut
