#!/usr/bin/pugs

use v6;
use Test;

plan 27;

use Set::Infinite; pass "(dummy instead of broken use_ok)";
use Set::Infinite;   # XXX should not need this
use Recurrence;

my $universe_recurr = Recurrence.new( 
    closure_next =>
        sub ( $x is copy ) { 
            return -Inf if $_ == -Inf;
            return  Inf if $_ ==  Inf;
            return $x + 1; 
        },
    closure_previous =>
        sub ( $x is copy ) { 
            return  Inf if $_ ==  Inf;
            return -Inf if $_ == -Inf;
            return $x - 1;
        },
    :is_universe(1),
);

# Creating the infinite set $u below appears to send pugs into
# an infinite loop.  We'll flunk instead for now.

for (1..9) { flunk "Infinite loop"; }

=begin infinite loop

my $u = Set::Infinite.new( 
    recurrence => $universe_recurr,
);

is( $u.start, -Inf, "start" );
is( $u.end  ,  Inf, "end" );
is( $u.stringify, '-Inf..Inf', "stringify" );

is( $u.start_is_open,   bool::false, "start_is_open" );
is( $u.end_is_open,     bool::false, "end_is_open" );

is( $u.start_is_closed, bool::true, "start_is_closed" );
is( $u.end_is_closed,   bool::true, "end_is_closed" );

is( $u.next( 10 ), 11, 'next' );
is( $u.previous( 10 ), 9, 'previous' );

=end

=cut

my $even_recurr = Recurrence.new( 
    closure_next =>     
        sub { 
            return -Inf if $_ == -Inf; 
            return  Inf if $_ ==  Inf; 
            return 2 * int( $_ / 2 ) + 2 
        },
    closure_previous => 
        sub { 
            return  Inf if $_ ==  Inf; 
            return -Inf if $_ == -Inf; 
            return 2 * int( ( $_ - 1 ) / 2 )
        },
    universe => $universe_recurr,
);

# More infinite loops ahead!  Creating $even_numbers sends pugs into
# another spin.

for (1..2) { flunk("Infinite loop"); }

=begin infinite loop

my $even_numbers = Set::Infinite.new( 
    recurrence => $even_recurr,
);

is( $even_numbers.next( 10 ), 12, 'next even' );
is( $even_numbers.previous( 10 ), 8, 'previous even' );

=end

=cut

# Unfortunately, the rest of this also creates infinite loops.
# So we'll skip the rest. ;(

for (1..15) { flunk("Infinite loop"); }

=begin infinite loops

{
    # union
    my $each_3_rec = Recurrence.new( 
        closure_next =>     
            sub { 
                return -Inf if $_ == -Inf; 
                return  Inf if $_ ==  Inf; 
                return 3 * int( $_ / 3 ) + 3;
            },
        closure_previous => 
            sub { 
                return  Inf if $_ ==  Inf; 
                return -Inf if $_ == -Inf; 
                return 3 * int( ( $_ - 1 ) / 3 );
            },
        universe => $universe_recurr, 
    );

    my $each_3_span = Set::Infinite.new( spans => Span.new( start => 10, end => 30 ) );
    my $each_3_spancode = $each_3_span.intersection( $each_3_rec );
    is( $each_3_span.ref, 
        'Set::Infinite', 
        'intersection isa Set::Infinite' );
    is( $each_3_span.stringify, 
        '[10,30]', 
        '10 to 30' );
    is( $each_3_spancode.stringify, 
        '12,15,18..24,27,30', 
        'each 3 from 10 to 30' );

    my $even_span = Set::Infinite.new( spans => Span.new( start => 20, end => 40 ) );
    my $even_spancode = $even_numbers.intersection( $even_span );
    is( $even_spancode.stringify, 
        '20,22,24..36,38,40', 
        'each 2 from 20 to 40' );

    my $result = $each_3_spancode.union( $even_spancode );
    is( $result.ref, 
        'Set::Infinite', 
        'union isa Set::Infinite' );
    is( $result.stringify, 
        '12,15,18,20,21,22..27,28,30,32,34,36,38,40', 
        'Recurrence Span union Recurrence Span' );

    # intersection
    $result = $each_3_spancode.intersection( $even_spancode );
    is( $result.stringify, 
        '24,30', 
        'Recurrence Span intersection Recurrence Span' );

    # difference
    $result = $each_3_spancode.difference( $even_spancode );
    is( $result.stringify, 
        '12,15,18,21,27', 
        'Recurrence Span difference Recurrence Span' );

    # complement
    $result = $each_3_spancode.complement();
    is( $result.stringify, 
        '-Inf..7,8,9,10,11,13..26,28,29,31,32,33..Inf',
        'Recurrence Span complement' );

    # union to a continuous span
    $result.add( Span.new( start => 100, end => 130 ) );
    is( $result.stringify,
        '-Inf..7,8,9,10,11,13..26,28,29,31,32,33..97,98,99,100,101,102..128,129,130,131,132,133..Inf',
        'Recurrence Span union Continuous Span' );

    # All segments are iteratable
    is( $result.next(5),   6, 'Recurrence Span complement is iteratable' );
    is( $result.next(11), 13, 'Recurrence Span complement is iteratable' );
    is( $result.next(50), 51, 'Recurrence Span complement is iteratable' );
}

my $odd_numbers = $even_numbers.complement;
is( $odd_numbers.next( 10 ),    11, 'odd recurrence' );
is( $odd_numbers.previous( 10 ), 9, 'odd recurrence' );

=end

=for later

{
    # -- intersection with a continuous span
    use Span::Num;
    my $continuous = Span::Num.new( start => 10, end => Inf, :end_is_open(bool::true) );
    is( $continuous.stringify, '[10,Inf)', 'continuous 10-Inf' );
    my $range = $universe.intersection( $continuous );
    isa_ok( $range, 'Span::Code', 'range from continuous' );
    is( $range.stringify, '10,11,12..Inf', 'range from continuous' );
    
    my $complement = $range.complement;
    is( $complement.stringify, '-Inf..7,8,9', 'range complement' );
}

{
    # -- intersection with a continuous span
    use Span::Num;
    my $continuous = Span::Num.new( start => -Inf, end => 10, :start_is_open(bool::true) );
    is( $continuous.stringify, '(-Inf,10]', 'continuous (-Inf,10]' );
    my $range = $universe.intersection( $continuous );
    isa_ok( $range, 'Span::Code', 'range from continuous' );
    is( $range.stringify, '-Inf..8,9,10', 'range from continuous' );
    
    my $complement = $range.complement;
    is( $complement.stringify, '11,12,13..Inf', 'range complement' );
}

{
    # -- intersection of odd numbers with a continuous span
    use Span::Num;
    my $continuous = Span::Num.new( start => -Inf, end => 10, :start_is_open(bool::true) );
    # continuous (-Inf,10]
    my $range = $odd_numbers.intersection( $continuous );
    isa_ok( $range, 'Span::Code', 'odd range from continuous' );
    is( $range.stringify, '-Inf..5,7,9', 'odd range from continuous' );
    
    my $complement = $range.complement;
    is( $complement.stringify, '-Inf..Inf', 'odd range complement' );

    {
    my $continuous = Span::Num.new( start => 6, end => 12 );
    # continuous [6..12]
    my $range = $complement.intersection( $continuous );
    is( $range.stringify, '6,8,10,11,12', 'odd range complement' );
    }

    {
    my $continuous = Span::Num.new( start => 4, end => 5 );
    my $range = $even_numbers.union( $continuous );
    my $range2 = Span::Num.new( start => 2, end => 9 );
    my $range3 = $range.intersection( $range2 );
    is( $range3.stringify, '2,4,5,6,8', 'even range union' );
    }

    {
    my $continuous = Span::Num.new( start => 4, end => 5 );
    my $range = $even_numbers.difference( $continuous );
    my $range2 = Span::Num.new( start => 2, end => 9 );
    my $range3 = $range.intersection( $range2 );
    is( $range3.stringify, '2,6,8', 'even range difference' );
    }
}

{
    # -- intersection with a continuous span
    use Span::Num;
    my $continuous = Span::Num.new( start => 10, end => 20 );
    is( $continuous.stringify, '[10,20]', 'continuous 10-20' );
    my $range = $universe.intersection( $continuous );
    isa_ok( $range, 'Span::Code', 'range from continuous' );
    is( $range.stringify, '10,11,12..18,19,20', 'range from continuous' );

    $continuous = Span::Num.new( start => 10, end => 20, end_is_open => 1 );
    is( $continuous.stringify, '[10,20)', 'continuous' );
    $range = $universe.intersection( $continuous );
    is( $range.stringify, '10,11,12..17,18,19', 'range from continuous semi' );
    
    my $complement = $range.complement;
    # XXX - universe slice should be written '(-Inf,Inf)'
    is( $complement.stringify, '-Inf..Inf', 'range complement' );

    my $complement1 = $complement.intersection( Span::Num.new( start => -Inf, end => 15 ) );
    is( $complement1.stringify, '-Inf..7,8,9', 'complement 1' );
    $complement1 = $complement.intersection( Span::Num.new( start => 15, end => Inf ) );
    is( $complement1.stringify, '20,21,22..Inf', 'complement 2' );
}
{
    # -- intersection with a discrete span
    use Span::Int;
    my $continuous = Span::Int.new( start => 10, end => 20 );
    is( $continuous.stringify, '[10,20]', 'continuous' );
    my $range = $universe.intersection( $continuous );
    is( $range.stringify, '10,11,12..18,19,20', 'range from discrete' );
}
{
    my $set = $universe.complement;
    is( $set.start,  undef, "start empty set" );
    is( $set.end  ,  undef, "end" );
    is( $set.stringify, '', "stringify" );
}
{
    # 0 .. Inf
    my $span1 = Span::Code.new( 
        closure_next =>        sub { $_ >= 0 ?? $_ + 1 !!    0 },
        closure_previous =>    sub { $_ > 0 ??  $_ - 1 !! -Inf },
        complement_next =>     sub { $_ < 1 ??  $_ + 1 !!  Inf },
        complement_previous => sub { $_ < 0 ??  $_ - 1 !!   -1 },
        universe => $universe );
    
    is( $span1.start,    0, "start" );
    is( $span1.end  ,  Inf, "end" );

    # -Inf .. 10
    my $span3 = Span::Code.new( 
        closure_next =>         sub { $_ < 10 ??  $_ + 1 !!  Inf },
        closure_previous =>     sub { $_ < 11 ??  $_ - 1 !!   10 },
        complement_next =>      sub { $_ >= 10 ?? $_ + 1 !!   11 },
        complement_previous =>  sub { $_ > 11 ??  $_ - 1 !! -Inf },
        universe => $universe );
    
    is( $span3.start, -Inf, "start" );
    is( $span3.end  ,   10, "end" );

    is( $span1.intersects( $span3 ), bool::true, 'intersects' );

    {
        my $span2 = $span1.complement;
        is( $span2.start, -Inf, "start" );
        is( $span2.end  ,   -1, "end" );
    }
    {
        my $span4 = $span3.complement;
        is( $span4.start,   11, "start" );
        is( $span4.end  ,  Inf, "end" );
        is( $span4.stringify, '11,12,13..Inf', "stringify" );
    }
    {
        my $span5 = $span1.intersection( $span3 );
        is( $span5.start,  0, "start" );
        is( $span5.end  , 10, "end" );
        is( $span5.stringify, '0,1,2..8,9,10', "stringify" );
    }
    {
        my $span5 = $span1.difference( $span3 );
        is( $span5.start,  11, "start" );
        is( $span5.end  , Inf, "end" );
    }
    {
        my $span5 = $span3.difference( $span1 );
        is( $span5.start, -Inf, "start" );
        is( $span5.end  ,   -1, "end" );
    }
    {
        my $span5 = $span3.union( $span1 );
        is( $span5.start, -Inf, "start" );
        is( $span5.end  ,  Inf, "end" );
    }
}

=cut

=for later

is( $span.size, 2, "real size" );
# is( $span.size( density => 1 ), 3, "integer size" );

my $span2 = Span::Num.new( 
    start => 2, end => 4, start_is_open => bool::false, end_is_open => bool::false );

my $span3 = Span::Num.new( 
    start => 4, end => 6, start_is_open => bool::false, end_is_open => bool::false );

is( $span.intersects( $span2 ), bool::true, 'intersects' );

is( $span.intersects( $span3 ), bool::false, 'doesn\'t intersect' );

{
    my @a = $span.complement;
    # XXX inconsistent stringification of -Inf
    is( @a[0].stringify ~ ' ' ~ @a[1].stringify, '(-Inf,1) (3,Inf)', 'complement' );
}

is( $span.intersection( $span2 ).stringify, '[2,3]', 'intersection' );

is( $span.union( $span2 ).stringify, '[1,4]', 'union' );

=cut
