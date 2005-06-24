#!/usr/bin/pugs

use v6;
use Test;

plan 8;

# This next group added by Darren Duncan following discovery while debugging ext/Locale-KeyedText:
{
  sub foo( $arg ) { return $arg.ref }
  sub make_array() { return ['were','there'] }
  sub make_hash() { return {'z'=>'y','x'=>'w'} }
#  my ($temp, @temp, %temp);

#  @temp = ();
#  is( foo(\@temp), 'Array', "named-def empty array arg defined via array var" );
#  $temp = [];
#  is( foo($temp), 'Array', "anon-def empty array arg defined via scalar var" );
  is( foo([]), 'Array', "anon-def empty array arg defined inline" );

#  @temp = ('hello');
#  is( foo(\@temp), 'Array', "named-def 1-elem array arg defined via array var" );
#  $temp = ['hello'];
#  is( foo($temp), 'Array', "anon-def 1-elem array arg defined via scalar var" );
  is( foo(['hello']), 'Array', "anon-def 1-elem array arg defined inline" );

#  @temp = ('hello','world');
#  is( foo(\@temp), 'Array', "named-def 2-elem array arg defined via array var" );
#  $temp = ['hello','world'];
#  is( foo($temp), 'Array', "anon-def 2-elem array arg defined via scalar var" );
  is( foo(['hello','world']), 'Array', "anon-def 2-elem array arg defined inline" );

#  $temp = make_array();
#  is( foo($temp), 'Array', "2-elem array arg, sub-returned, invoked via scalar var" );
  is( foo(make_array()), 'Array', "2-elem array arg, sub-returned, invoked inline" );

#  %temp = ();
#  is( foo(\%temp), 'Hash', "named-def empty hash arg defined via hash var" );
#  $temp = hash();
#  is( foo($temp), 'Hash', "anon-def empty hash arg defined via scalar var" );
  is( foo(hash()), 'Hash', "anon-def empty hash arg defined inline" );

#  %temp = ('a'=>'b');
#  is( foo(\%temp), 'Hash', "named-def 1-elem hash arg defined via hash var" );
#  $temp = {'a'=>'b'};
#  is( foo($temp), 'Hash', "anon-def 1-elem hash arg defined via scalar var" );
  is( foo({'a'=>'b'}), 'Hash', "anon-def 1-elem hash arg defined inline" );

#  %temp = ('a'=>'b','c'=>'d');
#  is( foo(\%temp), 'Hash', "named-def 2-elem hash arg defined via hash var" );
#  $temp = {'a'=>'b','c'=>'d'};
#  is( foo($temp), 'Hash', "anon-def 2-elem hash arg defined via scalar var" );
  is( foo({'a'=>'b','c'=>'d'}), 'Hash', "anon-def 2-elem hash arg defined inline" );

#  $temp = make_hash();
#  is( foo($temp), 'Hash', "2-elem hash arg, sub-returned, invoked via scalar var" );
  is( foo(make_hash()), 'Hash', "2-elem hash arg, sub-returned, invoked inline" );
}
