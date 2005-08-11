#=infix
infix:«<»( $a, $b );      # $a.infix:«<»( $b );
infix:«<=>»( $a, $b );    # $a.infix:«<=>»( $b );
infix:<¥>( @a, @b );      # @a.infix:<¥>( @b );
#=circumfix
circumfix:«{ }»($a, 'foo'); # $a.circumfix:«{ }»('foo');
#=postcircumfix
postcircumfix:«{ }»($a, 'foo');
# $a.postcircumfix:«{ }»('foo');
