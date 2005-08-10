package Perl6::Value::List;

# Perl6::Value::List - implementation of Perl6 'List' class in Perl5

# TODO - add methods grep(), map(), ...

use strict;
our $VERSION = '0.01';
use constant Inf => 100**100**100;

sub new () {
    my $class = shift;
    my %param = @_;

    my $elems = delete $param{elems}; 
    unless ( defined $elems ) {
        $elems = ( defined $param{start} || defined $param{end} ) ? sub { Inf } : sub{ 0 };
    }

    my $start = delete $param{start}; 
    $start = sub{} unless defined $start;

    my $end = delete $param{end};   
    $end = sub{} unless defined $end;

    my $is_infinite = delete $param{is_infinite}; 
    $is_infinite = sub{ $_[0]->elems == Inf } unless defined $is_infinite;

    die 'invalid parameters' if keys( %param );
    return bless { start =>  $start, 
                   end =>    $end,
                   elems =>  $elems,
                   is_infinite => $is_infinite }, $class;
}

sub clone () { $_[0] }

sub elems () { $_[0]->{elems}() }

sub is_infinite () { $_[0]->{is_infinite}() }

sub to_ref () { $_[0] }

sub to_bit () { $_[0]->elems > 0 }

sub to_num () { $_[0]->elems }

sub to_str () {
    my $self = shift;
    die "infinite list stringification" 
        if $self.is_infinite;
    return '' if $self.elems <= 0;
    my $s = $self->shift;
    return ''.$s if $self.elems <= 0;
    while(1) {
        $s .= ' ' . $self->shift;
        return $s if $self.elems <= 0;
    }
}

sub reverse () {
    my $self = shift;
    my $class = ref($self);
    return $class->new( start => $self->{end},
                        end =>   $self->{start},
                        elems => $self->{elems},
                        is_infinite => $self->{is_infinite} );
}

# --- extra methods ---

sub concat_list () {
    my $self = shift;
    my $list = shift;
    my $class = ref($self);
    return $class->new( start => sub{ return $self->shift if $self->elems;
                                      return $list->shift; },
                        end =>   sub{ return $list->pop if $list->elems;
                                      return $self->pop; },
                        elems => sub{ $self->elems + $list->elems },
                        is_infinite => sub{ $self->is_infinite || $list->is_infinite } );
}

sub from_single () {
    my $class = shift;
    my @list = @_;
    return $class->new( start => sub{ shift @list },
                        end =>   sub{ pop @list },
                        elems => sub{ scalar @list } );
}

sub from_range () {
    my $class = shift;
    my %param = @_;
    my $start = delete $param{start};
    my $end =   delete $param{end};
    my $step =  delete $param{step};
    die 'invalid parameters' if keys( %param );
    return bless {  start => sub{ 
                                my $r = $start; 
                                if ( defined $step ) { $start += $step } else { $start++ };
                                return $r;
                            },
                    end =>    sub{ 
                                my $r = $end; 
                                if ( defined $step ) { 
                                    # XXX - this should use modulus, etc.
                                    $end -= $step 
                                } 
                                else { 
                                    $end-- 
                                };
                                return $r;
                            },
                    elems =>  sub{ 
                                return $end - $start + 1 unless defined $step;
                                return int(( $end - $start + 1 ) / $step);
                            },
                    is_infinite => { return $start == -Inf || $end == Inf },
            }, $class;
}

# ---- these methods should be declared last, because they interfere with CORE::* things

sub pop () {
    my $self = shift;
    return if $self->elems <= 0;
    return $self->{end}();
}

sub shift () {
    my $self = shift;
    return if $self->elems <= 0;
    return $self->{start}();
}

1;
__END__

=head1 NAME

Perl6::Value::List - Perl extension for Perl6 "List" class

=head1 SYNOPSIS

  use Perl6::Value::List;
  
  my $list = Perl6::Value::List.from_range( start => 10, end => 20 );
  
  my $list = Perl6::Value::List.new( ... );

=head1 DESCRIPTION

This module implements a "List" object.

new() without parameters is an empty list.

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
