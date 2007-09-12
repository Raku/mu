grammar MyGrammar {
    token digit :P5 {[[:digit:]]};
    token digits { \d+ };
    token val_int {
        <digits>
        { return ::Val::Int( 'int' => ~$/ ) }
    };
};
module Main {
    say '1..1';

    $_ = '1';
    say 'ok ' ~ MyGrammar.val_int();
}
