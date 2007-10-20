use Kp6Ast;
say "package Call;";
for ((Call.HOW).methods()) -> $method {
    say "sub $method {
        ::DISPATCH(@_);
    }";
}
for ((Call.HOW).attributes()) -> $attr {
    say "sub $attr {@_ == 1 ? ::DISPATCH(\$_[0],\"$attr\") : ::DISPATCH(::DISPATCH(\$_[0],\"$attr\"),'STORE',\$_[1])}";
}

#package Call;
#sub new { shift; bless {@_}, "Call" }
#sub invocant  { @_ == 1 ? ( $_[0]->{invocant} )  : ( $_[0]->{invocant}  = $_[1] ) }
