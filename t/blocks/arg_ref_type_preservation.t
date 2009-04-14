use v6;

use Test;

plan 8;

# This next group added by Darren Duncan following discovery while debugging ext/Locale-KeyedText:
{
  sub foo( $arg ) { return $arg.WHAT }
  sub make_array() { return ['were','there'] }
  sub make_hash() { return {'z'=>'y','x'=>'w'} }
  is( foo([]), ::Array, "anon-def empty array arg defined inline" );

  is( foo(['hello']), ::Array, "anon-def 1-elem array arg defined inline" );

  is( foo(['hello','world']), ::Array, "anon-def 2-elem array arg defined inline" );

  is( foo(make_array()), ::Array, "2-elem array arg, sub-returned, invoked inline" );

  is( foo(hash()), ::Hash, "anon-def empty hash arg defined inline" );

  is( foo({'a'=>'b'}), ::Hash, "anon-def 1-elem hash arg defined inline" );

  is( foo({'a'=>'b','c'=>'d'}), ::Hash, "anon-def 2-elem hash arg defined inline" );

  is( foo(make_hash()), ::Hash, "2-elem hash arg, sub-returned, invoked inline" );
}
