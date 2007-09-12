grammar MyGrammar {
    token digits {  \d  [ <digits> | <''> ]  };
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
