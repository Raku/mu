package Pugs::Runtime::Match;

# Documentation in the __END__
use 5.006;
use strict;
use warnings;
use Data::Dumper;

use overload (
    '@{}'    => \&array,
    '%{}'    => \&hash,
    'bool'   => \&bool,
    '&{}'    => \&code,
    '""'     => \&flat,
    '0+'     => \&flat,
    fallback => 1,
);

sub new {
    my ($class, $match) = @_;
    my $self = \$match;
    bless $self, $class;
}

# XXX - this sub should set .from and .to in match nodes!
#     - it should be called only once, whenever there is an access
#       to: .from(); .to(); methods that call ._box_submatch() 
sub _str {
    my $match = $_[0];
    #print "STR: ", ref( $match ), " ", Dumper( $match ), "\n";
    return join( '', map { match::str( $_ ) } @$match )
        if ref( $match ) eq 'ARRAY';
    return _str( $match->{match} ) 
        if ref( $match ) eq 'HASH' && exists $match->{match};
    return join( '', map { _str( $_ ) } values %$match )
        if ref( $match ) eq 'HASH';
    return $match;
}

sub from {
    ${$_[0]}->{from} || 0;
}

sub to {
    #print 'TO: ', do{use Data::Dumper; Dumper(${$_[0]})};
    #my $str = _str( ${$_[0]} );
    #print "TO: $str\n";
    ${$_[0]}->{to} || ($_[0]->from + length( _str( ${$_[0]} ) ));
}

sub _box_submatch {
    my ($match, $submatch) = @_;
    my $m = { %$submatch };
    delete $m->{label};
    ref($match)->new($m);
}

sub flat {
    return '' unless defined ${$_[0]}->{capture};
    ${$_[0]}->{capture};
}

# return the capture
sub code {
    my $c = ${$_[0]}->{capture};
    return sub { $c };
}

# return the capture
sub capture {
    ${$_[0]}->{capture};
}

# return the bool value
sub bool {
    ${$_[0]}->{bool};
}

sub _get_captures {
    my @a;
    # special case: the whole match is a single capture
    return ${$_[0]} if exists ${$_[0]}->{label};
    if ( ref( ${$_[0]}->{match} ) eq 'ARRAY' ) {
        @a = @{${$_[0]}->{match}};
    }
    else {
        push @a, ${$_[0]}->{match};        
    }
    #print Dumper @a;
    while ( ref $a[-1] && exists $a[-1]{match} && ref( $a[-1]{match} ) eq 'ARRAY' ) {
        my $t = pop @a;
        push @a, @{$t->{match}};
    }
    return @a;
}

# as hash
sub hash {
    my @a = _get_captures( $_[0] );
    my %r;
    for my $m ( @a ) {
        next unless ref $m && exists $m->{label};
        if ( $m->{label} eq '*quantifier*' ) {
            my %h = %{ _box_submatch( $_[0], $m ) };
            while ( my ($k, $v) = each %h ) {
                my @v = ref($v) eq 'ARRAY' ? @$v : 
                        defined $v ? $v :
                        ();
                if ( ref( $r{$k} ) ne 'ARRAY' ) {
                    $r{$k} = [ $r{$k} ] if defined $r{$k};
                }
                push @{$r{$k}}, @v;
            }
            next;
        }
        next if $m->{label} eq '';
        if ( exists $r{ $m->{label} } ) {
            if ( ref( $r{$m->{label}} ) eq 'ARRAY' ) {
                push @{$r{$m->{label}}}, _box_submatch( $_[0], $m );                
            }
            else {
                $r{$m->{label}} = [ $r{$m->{label}}, _box_submatch( $_[0], $m ) ];
            }
        }
        else {
            $r{ $m->{label} } = _box_submatch( $_[0], $m );
        }
    }
    return \%r;
}

# as array
sub array {
    my @a = _get_captures( $_[0] );
    my @r;
    for my $m ( @a ) {
        next unless exists $m->{label};
        if ( $m->{label} eq '*quantifier*' ) {
            my @m = @{ _box_submatch( $_[0], $m ) };
            push @r, \@m if @m;
            next;
        }
        next if $m->{label} ne '';
        push @r, _box_submatch( $_[0], $m );                
    }
    return \@r;
}

1;

__END__

=head1 NAME 

Pugs::Runtime::Match - Match object

=head1 SYNOPSIS

    use Pugs::Compiler::Rule;

    my $rule = Pugs::Compiler::Rule->compile( '((.).).' );
    my $match = $rule->match( 'abc' );

    if ($match) {               # true
        print $match;           # "abc"
        print $match->from;     # 0
        print $match->to;       # 3
        print $match->[0];      # "ab"
        print $match->[0][0];   # "a"
    }

=head1 OVERLOAD INTERFACE

=head2 bool

When used as a boolean, the match object returns whether the match has
succeeded or not.

=head2 @{}

When used as an array reference, the match object returns positional
captures as match objects.

=head2 %{}

When used as a hash reference, the match object returns named captures
as match objects, keyed by their names.

=head2 &{}

When used as a code reference, the match object returns the I<result>
object; this is normally the entire match string, but may be arbitrary
objects returned from the rule using a C<return> clause.

=head2 ""

When used as a string, the match object returns the stringified version
of the result object (usually the entire match string).

=head2 0+

When used as a number, the match object returns the numified version
of the result object (usually the entire match string).

=head1 METHODS

=head2 from ()

Returns the initial position of the match.

=head2 to ()

Returns the final position of the match.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
