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
        attrs => [ '$.type', '$.things' ],
                # type - unboxed str
                # things - ptr to native ARRAY of objects
        methods => {
            'junction_simplify' => sub {
                my @ary = @{_('$.things')};
                my $type = _('$.type');
                my @res;
                for ( @ary ) {
                    if ( $_->isa('Junction') && $_->type eq $type ) {
                        push @res, @{ $_->things };
                    }
                    else {
                        push @res, $_
                    }
                }
                #warn "SORT @res";
                # remove duplicates
                for ( @res ) {
                    $_ = $_->fetch if $_->isa('Scalar');
                }
                @res = sort { Perl6::Value::identify($a) cmp Perl6::Value::identify($b) } @res;
                my $last_id = rand();
                #warn "SORTED @res";
                @res = grep { 
                        my $id = Perl6::Value::identify( $_ );
                        #warn "ID $id";
                        $id eq $last_id ? 0 : ( $last_id = $id, 1 )
                    } @res;
                @{_('$.things')} = @res;
            },
            'values' => sub { 
                my $ary = Array->new;
                $ary->push( @{_('$.things')} );
                return $ary;
            },
            'num' =>  sub { warn "Junction.num() not implemented" },
            'int' =>  sub { warn "Junction.int() not implemented" },
            'str' =>  sub { 
                my $sep = "!";  # none?
                $sep = "|" if _('$.type') eq 'any';
                $sep = "&" if _('$.type') eq 'all';
                $sep = "^" if _('$.type') eq 'one';             
                Str->new( '$.unboxed' => 
                    "(" . 
                    join(" $sep ", 
                        map { $_->str->unboxed } @{_('$.things')}
                    ) . 
                    ")"
                ) 
            },
            'bit' =>  sub { 
                #warn "Junction.bit() not implemented";
                my @ary = @{_('$.things')};
                my $type = _('$.type');
                @ary = map {
                        ref($_) ?
                        $_->bit->unboxed :
                        $_ != 0
                    } @ary;
                #warn "BIT: $type [ @ary ]";
                if ( $type eq 'all' ) {
                    for ( @ary ) { return Bit->new( '$.unboxed' => 0 ) unless $_ }
                    return Bit->new( '$.unboxed' => 1 );
                }
                if ( $type eq 'any' ) {
                    for ( @ary ) { return Bit->new( '$.unboxed' => 1 ) if $_ }
                    return Bit->new( '$.unboxed' => 0 );
                }
                if ( $type eq 'one' ) {
                    my $count = 0;
                    for ( @ary ) { $count++ if $_ }
                    return Bit->new( '$.unboxed' => ( $count == 1 ) );
                }
                if ( $type eq 'none' ) {
                    for ( @ary ) { return Bit->new( '$.unboxed' => 0 ) if $_ }
                    return Bit->new( '$.unboxed' => 1 );
                }
                die "Unknown Junction $type";
            },
            'perl' => sub {
                Str->new( '$.unboxed' =>  
                    _('$.type') . "(" . 
                    join(", ", 
                        map { $_->str->unboxed } @{_('$.things')}
                    ) . ")"
                ) 
            },
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
