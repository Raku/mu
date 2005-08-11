package Perl6::Value::List;

# Perl6::Value::List - implementation of Perl6 'List' class in Perl5

# ChangeLog
#
# 2005-08-11
# * grep(), map() don't depend on coroutines
#
# 2005-08-10
# * Removed method concat_list(), added "TODO" methods

# TODO - add methods grep(), map(), ...
# TODO - add tests
# TODO - update MANIFEST
# TODO - is_contiguous() should test if $step == 1
# TODO - fix elems() in from_range(), when start/end are Str - 'a'..'z'
# TODO - rewrite ops using map()

use strict;
our $VERSION = '0.01';
use constant Inf => 100**100**100;
use base qw(Exporter);
use vars qw(@EXPORT_OK);
@EXPORT_OK = qw(Inf);

sub TODO { die "not implemented" };

sub new {
    my $class = shift;
    my %param = @_;

    $param{cis_infinite}   = sub { $_[0]->{celems}() == Inf } 
        unless defined $param{cis_infinite};
    $param{cis_contiguous} = sub { 0 } 
        unless defined $param{cis_contiguous};
    $param{cstringify}     = sub { $_[0]->{cstart}() . '....' . $_[0]->{cend}() } 
        unless defined $param{is_lazy}; 
    $param{is_lazy}        = 1 unless defined $param{is_lazy};
    unless ( defined $param{celems} ) {
        $param{celems} =
            ( defined $param{cstart} || defined $param{cend} ) ? sub { Inf } : sub { 0 }
    }
    $param{cstart} = sub {} unless defined $param{cstart};
    $param{cend}   = sub {} unless defined $param{cend}; 
    return bless \%param, $class;
}

sub clone         { bless { %{ $_[0] } }, ref $_[0] }
sub elems         { $_[0]->{celems}() }
sub is_infinite   { $_[0]->{cis_infinite}() }
sub is_contiguous { $_[0]->{cis_contiguous}() }
sub to_str        { $_[0]->{cstringify}() }
sub to_ref        { $_[0] }
sub to_bit        { $_[0]->elems > 0 }
sub to_num        { $_[0]->elems }
sub to_list       { $_[0] }
sub is_lazy       { $_[0]->{is_lazy} }

sub flatten       { 
    my $ret = shift;
    my $class = ref($ret);

    # TODO - add tests for this error message
    # fail "can't instantiate an infinite list"
    #     if $ret->is_infinite;

    my @list;
    while ( $ret->elems ) { push @list, $ret->shift }
    $class->from_single( @list ); 
}

sub from_range {
    my $class = shift;
    my %param = @_;
    my $start = $param{start};
    my $end =   $param{end};
    my $step =  $param{step};
    $class->new(
                cstart =>  sub {
                            my $r = $start;
                            if ( defined $step ) { $start += $step } else { $start++ };
                            return $r;
                        },
                cend =>    sub {
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
                celems =>  sub {
                            return $end - $start + 1 unless defined $step;
                            return int(( $end - $start + 1 ) / $step);
                        },
                cis_infinite => sub { return $start == -&Inf || $end == Inf },
                cis_contiguous => sub { $step == -1 || $step ==  1 || $step == undef },
    );
}

sub from_single {
    my $class = shift;
    my @list = @_;
    $class->new( cstart => sub{ shift  @list },
                 cend =>   sub{ pop    @list },
                 celems => sub{ scalar @list },
                 is_lazy => 0 );
}

sub from_coro {
    my $class = shift;
    my $start = shift;
    my $size = Inf;
    $class->new(
                cstart =>  sub {
                            my $r = $start->();
                            # print "coro\n";
                            $size = 0 unless defined $r;
                            return $r;
                        },
                cend =>    sub {},
                celems =>  sub { $size },
                cis_infinite => sub { $size == Inf },
                cis_contiguous => sub { 0 },
    );
}

# --- list operations ---

sub reverse { 
    my $ret = shift;
    Perl6::Value::List->new( 
            cstart =>         $ret->{cend},
            cend =>           $ret->{cstart},
            celems =>         $ret->{celems},
            cis_infinite =>   $ret->{cis_infinite},
            cis_contiguous => $ret->{cis_contiguous},
            cstringify =>     $ret->{cstringify},
    );
}

sub grep { 
    my $array = shift;
    my $code = shift;
    return $array->map( 
        sub {
            return $_[0] if $code->($_[0]);
            while( $array->elems ) {
                my $x = $array->shift; 
                # print "x ", $_," elems ", $ret->elems ,"\n";
                return $x if $code->($x);
            }
        } ); 
}

sub map { 
    my $array = shift;
    my $code = shift;
    my $ret = $array->clone; 
    my @shifts;
    my @pops;
    Perl6::Value::List->new(
            cstart => sub {
                    #print "entering map, elems = ", $ret->elems, "\n";
                    while( $ret->elems ) {
                        # TODO - invert the order a bit
                        my $x = $ret->shift; 
                        #print "map $x\n";
                        # print " got x ", $_," elems ", $ret->elems ,"\n";
                        push @shifts, $code->($x);
                        #print " mapped to [", @shifts, "] ", scalar @shifts, "\n";
                        return shift @shifts if @shifts;
                        # print " skipped ";
                    }
                    #print " left [", @shifts, @pops, "] ", scalar @shifts, "+", scalar @pops, "\n";
                    return shift @shifts if @shifts;
                    return shift @pops if @pops;
            },
            cend => sub { 
                    while( $ret->elems ) {
                        local $_ = $ret->pop; 
                        # print "x ", $_," elems ", $ret->elems ,"\n";
                        unshift @pops, $code->($_);
                        return pop @pops if @pops;
                    }
                    return pop @pops if @pops;
                    return pop @shifts if @shifts;
            },
            celems => sub { 
                    $ret->elems ? Inf : $#shifts + $#pops + 2 
            },
    );
}

sub uniq { 
    my $array = shift;
    my $ret = $array->clone;
    my %seen = ();
    return $array->map( 
        sub {
            return if $seen{$_[0]};
            $seen{$_[0]}++;
            $_[0];
        } ); 
}

sub kv { 
    my $array = shift;
    my $ret = $array; 
    my $count = 0;
    return $array->map( 
        sub {
            return ( $count++, $_[0] )
        } ); 
}

sub pairs { 
    my $array = shift;
    my $ret = $array; 
    my $count = 0;
    return $array->map( 
        sub {
            warn "TODO: pairs";
            return ( $count++, $_[0] )
        } ); 
}

sub keys { 
    my $array = shift;
    my $ret = $array; 
    my $count = 0;
    return $array->map( 
        sub {
            return $count++
        } ); 
}

sub values { 
    @_
}

sub zip { 
    my $array = shift;
    my @lists = @_;
    my $ret = $array->clone; 
    my @shifts;
    my @pops;
    Perl6::Value::List->new(
            cstart => sub {
                return shift @shifts if @shifts;
                my $any = 0;
                for ( $ret, @lists ) { $any++ if $_->elems }
                push @shifts, ( $ret->shift, 
                            map { my $x = $_->shift; defined $x ? $x : 'x' } 
                                @lists ) if $any;
                return shift @shifts if @shifts;
                return shift @pops if @pops;
            },
            cend => sub { 
                return pop @pops if @pops;
                my $any = 0;
                for ( $ret, @lists ) { $any++ if $_->elems }
                unshift @pops, ( $ret->pop, 
                            map { my $x = $_->pop; defined $x ? $x : 'x' } 
                                @lists ) if $any;
                return pop @pops if @pops;
                return pop @shifts if @shifts;
            },
            celems => sub { 
                my $any = 0;
                for ( $ret, @lists ) { $any++ if $_->elems }
                $any ? Inf : $#shifts + $#pops + 2 
            },
    );
}

sub shift         { $_[0]->{celems}() ? $_[0]->{cstart}() : undef }
sub pop           { $_[0]->{celems}() ? $_[0]->{cend}()   : undef }  

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
