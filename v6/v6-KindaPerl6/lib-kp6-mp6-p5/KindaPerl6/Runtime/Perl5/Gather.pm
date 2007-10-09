use strict;
use Coro;
use Scalar::Util; 

# %::GATHER  holds the inside-out-ish lazy list instances
# take() is defined in GLOBAL.pm

$::Gather = KindaPerl6::Runtime::Perl5::MOP::make_class(
    proto => $::Gather, 
    name=>"Gather",parent=>[$::meta_Array],methods=>
    {
    
    new => sub {
            my $code = $_[1];
            my $gather_finished = ::DISPATCH( $::Bit, 'new', 0 ); 
            my $gather_coro = Coro::async {
               ::DISPATCH( $code, "APPLY" );
               # cleanup the pointer to the lazy buffer
               delete $::GATHER{ Scalar::Util::refaddr( $Coro::current ) };
               $gather_finished->{_value} = 1;
               return;
             };
            my $buf = ::DISPATCH( $::Array, 'new' );
            $::GATHER{ Scalar::Util::refaddr( $gather_coro ) } = $buf->{_value}{_array};
            my $v = {
                %{ $_[0] },
                _value => {
                        code      => $code,   # used by .perl
                        buf       => $buf,
                        finished  => $gather_finished,
                    },  
            };
        },
    _more => sub { Coro::cede() },
    INDEX=>sub {
            my $key = ::DISPATCH(::DISPATCH($_[1],"int"),"p5landish");
            while ( ! $_[0]{_value}{finished}{_value} 
                &&  $key > $#{ $_[0]{_value}{buf}{_value}{_array} } 
               )
            {
                Coro::cede();  
            };
            return ::DISPATCH($::Cell,"new",{cell=>\($_[0]{_value}{buf}{_value}{_array}[$key])});
        },
}, );

