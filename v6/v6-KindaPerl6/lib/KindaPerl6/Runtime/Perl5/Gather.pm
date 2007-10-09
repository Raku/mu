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
            #print "# Coro started at ", Scalar::Util::refaddr( $gather_coro ), "\n";
            my $buf = ::DISPATCH( $::Array, 'new' );
            $::GATHER{ Scalar::Util::refaddr( $gather_coro ) } = $buf->{_value}{_array};
            my $v = {
                %{ $_[0] },
                _value => {
                        code      => $code,   # used by .perl
                        buf       => $buf,
                        finished  => $gather_finished,
                        _coro     => Scalar::Util::refaddr( $gather_coro ),
                    },  
            };
        },
    _more => sub { Coro::cede() },
    _take => sub {
        Coro::cede();
        # XXX avoid nested coro bug ???
        #print "using ", $_[0]{_value}{_coro}, "\n";
        #$_[0]{_value}{buf}{_value}{_array} = $::GATHER{ $_[0]{_value}{_coro} };
        
        push @{ $::GATHER{ $_[0]{_value}{_coro} } }, 
            ::DISPATCH( $_[1], 'FETCH' );
        return $_[1];
    }
}, );

