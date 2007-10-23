use strict;

$::Pair = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto => $::Pair, 
    name=>"Pair",parent=>[$::meta_Value],methods=>
    {
    hash => sub {
            my $key = ::DISPATCH(::DISPATCH( $_[0]{_value}{key}, "Str" ),"p5landish");
            #print "value = ",::DISPATCH(::DISPATCH( $_[0]{_value}{value}, "Str" ),"p5landish")," ";
            #print "PAIR: $key => $_[0]{_value}{value} \n";
            my $h = ::DISPATCH($::Hash,"new", { _hash => { $key => $_[0]{_value}{value} } } );
            return $h;
        },
});

1;
