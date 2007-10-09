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
            my $gather_finished; 
            my $gather_coro = Coro::async {
               ::DISPATCH( $code, "APPLY" );
               # cleanup the pointer to the lazy buffer
               delete $::GATHER{ Scalar::Util::refaddr( $Coro::current ) };
               $gather_finished = 1;
               return;
             };
            #print "Coro: $gather_coro \n";
            $::GATHER{ Scalar::Util::refaddr( $gather_coro ) } = [];
            #return ( $::GATHER{ Scalar::Util::refaddr( $gather_coro ) }, \$gather_finished );

            my $v = {
                %{ $_[0] },
                _value => {
                        _array    => $::GATHER{ Scalar::Util::refaddr( $gather_coro ) },
                        _finished => \$gather_finished,
                    },  
            };
        },
    INDEX=>sub {
            # TODO
            my $key = ::DISPATCH(::DISPATCH($_[1],"int"),"p5landish");
            while ( ! ${ $_[0]{_value}{_finished} }
                &&  $key > $#{ $_[0]{_value}{_array} } 
               )
            {
                Coro::cede();  
            };
            return ::DISPATCH($::Cell,"new",{cell=>\$_[0]{_value}{_array}[$key]});
        },
    STORE=>sub {
            die "can't STORE to a gather/take";
        },
}, );

