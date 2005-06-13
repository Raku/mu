#=infix
$a < $b		# $a.infix:«<»( $b );
$a <=> $b	# $a.infix:«<=>»( $b );
@a ¥ @b		# @a.infix:<¥>( @b );

#=circumfix
$a{'foo'}		# $a.circumfix:«{ }»('foo');

#=postcircumfix
$a.{'foo'}		# $a.postcircumfix:«{ }»('foo');
