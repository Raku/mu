grammar MyGrammar {
    token digit :P5 {[[:digit:]]};
    token digits { \d+ };
    token val_int {
        <digits>
        { return ::Val::Int( 'int' => ~$/ ) }
    };
};
class Val::Int {
    has $.int;
    method true {
        1;            
    }
};

say '1..1';
$_ = '1';
say 'ok ' ~ MyGrammar.val_int();
