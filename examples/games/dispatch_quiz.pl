use Test;

# Originally by Mark Stosberg

say "Welcome to the Pugs Dispatching Quiz!";
say;
say "For each scenerio, answer 1 or 2 to select which way"; 
say "you expect method dispatching to work. Ctrl-C to bail out. ";
say; 

my $quiz = eval(q{
-
    m1: %h
    m2: @a?
    call: <1 2 3>
-
    m1: @a?
-
    m1: 
    m2:
    call:
}, :lang<yaml>);

plan $quiz.elems;

for each($quiz) -> %h {
    # I think Perl6 is supposed to offer us a way to just
    # pass %h on through...
    ask( m1 => %h<m1>, m2 => %h<m2>, call => %h<call>);
}

sub ask (
    Str $m1,  
    Str $m2,
    Str $call) {
    my $q = q:s/given multi a ($m1) {1}; multi a ($m2) {2} pugs will dispatch a($call)/;
    print "\n$q to [1,2]: ";

    my $answer = eval q:s/ { multi a ($m1) {1} multi a ($m2) {2} a($call) } / ;

    is(
        =$*IN,
        $answer,
        $q
    );
}
