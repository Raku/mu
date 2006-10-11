#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 17;
 
use Perl6::Value;
use Perl6::Value::List;

use constant Inf => Perl6::Value::Num::Inf;

{
  # end of stream
  my $a = Perl6::Value::List->from_single( 1 .. 2 );
  is( $a->shift, 1, 'iter 0' );
  is( $a->shift, 2, 'iter 1' );
  is( $a->shift, undef, 'end' );
}

{
  # end of lazy stream
  my $a = Perl6::Value::List->from_range( start => 1, end => 2, step => 1 );
  is( $a->shift, 1, 'iter 0' );
  is( $a->shift, 2, 'iter 1' );
  is( $a->shift, undef, 'end' );
}

{
  # 'Iter' object
  my $span = Perl6::Value::List->from_range( start => 0, end => Inf, step => 1 );
  is( $span->shift, 0, 'iter 0' );
  is( $span->shift, 1, 'iter 1' );
  
  is( $span->pop, Inf, 'pop' );
  is( $span->pop, Inf, 'pop' );
  
  # reverse
  my $rev = $span->reverse();
  isa_ok( $rev, 'Perl6::Value::List', 'reversed' );
  is( $rev->shift, Inf, 'shift reverse' );
  is( $rev->pop,   2,   'pop reverse' );
}

{
    # clone
    my $list1 = Perl6::Value::List->from_num_range( start => 3, end => 1000_000, step => 1 );
    my $list2 = $list1->clone;

    is( $list1->shift, 3, 'shift from list' );
    is( $list1->pop, 1000000, '... pop' );
        
    is( $list2->shift, 3, 'shift from clone' );
    is( $list2->pop, 1000000, '... pop' );
}

__END__

{
    # list mapped into array
    my $span0 = Perl6::Value::List->from_num_range( start => 3, end => &Inf, step => 1 );
    my $span1 = Perl6::Value::List->from_num_range( start => &Inf, end => 7, step => -1 );
    my $array = Perl6::Container::Array->new( items => [ $span0, $span1 ] );
    is( $array->shift, 3, 'lazy array shift' );
    is( $array->pop, 7, '... lazy array pop' );
    
    my $list = Perl6::Value::List->from_shared_array(
        array => $array, offset => 2, length => &Inf );
    is( $list->shift, 6, 'shift from list' );
    is( $list->pop, 8, '... pop' );
        
    my $list2 = Perl6::Value::List->from_shared_array(
        array => $array, offset => 2, length => &Inf );
    is( $list2->shift, 6, 'shift from another list, same buffer' );
    is( $list2->pop, 8, '... pop' );
}

{
  # shared list
  my $span = Perl6::Value::List->from_num_range( start => 0, end => 100, step => 1 );
  my $list0 = Perl6::Value::List->new;
  {
    # start of inner scope
    my $list1 = Perl6::Value::List->new;
    
    $span->dup_list( $list0, $list1 );
    is( $list0->shift, 0, 'iter 0 - dup list - shift' );
    is( $list1->shift, 0, '...  1' );
    is( $list1->shift, 1, '...  1' );
    is( $list0->shift, 1, '...  0' );
  
    is( $list0->pop, 100, 'iter 0 - dup list - pop' );
    is( $list0->pop,  99, '...  0 ' );
    is( $list1->pop, 100, '...  1' );
    is( $list1->pop,  99, '...  1' );
    is( $list0->pop,  98, '...  0' );
    
    # end of inner scope
  }
  # warn "outer scope - list1 data should have been destroyed";
}
