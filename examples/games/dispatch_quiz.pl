use Test;

say "Welcome to the Pugs Dispatching Quiz!";
say;
say "For each scenerio, answer 1 or 2 to select which way"; 
say "you expect dispatching to work. Ctrl-C to bail out. ";
say; 

my $quiz = eval(q{
- 
    m2: *%h
    call: a => 'b'
-
    m1: %h
    m2: @a?
    call: <1 2 3>
-
    m1: %h
    m2: @a
    call: <1 2 3>
-
    m1: %h?
    m2: @a?
    call:
- 
    m1: %h, *%h
    call: a => 'b'
-
    m1: @a?
-
    m1: 
    m2:
    call:
}, :lang<yaml>);

# here are some other dispatch oddities which don't fit into
# the defined quiz format yet:
# 1. with a sig of (*%h, *@a), a call of ( a => 'b'), /won't/ send
#    the args to %h. Why? Because 'a' is a named argument matching @a. 
#    ( Can you turn that off? )
#
# 2. Not quite a dispatch issue, but passing arguments through a wrapper
#    doesn't work like Perl5:
#        sub core (%h) { say "core" }  
#        sub wrapper (%h){  core(%h)  }
#        wrapper( a => b );
#    The problem is that %h gets seen as a single argument rather than a set of named parameters. 
#    Audreyt suggested this should work, but is unimplemented in pugs now:
#        sub wrapper (%h){  core(%h)  }


plan $quiz.elems * 2; # method and sub calls for each. 

for each($quiz) -> %h {
    # I think Perl6 is supposed to offer us a way to just
    # pass %h on through...
    ask( m1 => %h<m1>, 
         m2 => %h<m2>, 
         call => %h<call>, 
         is_method => 0
     );
    ask( m1 => %h<m1>, 
         m2 => %h<m2>, 
         call => %h<call>, 
         is_method => 1
     );
}

sub ask (
    Str $m1,  
    Str $m2,
    Str $call,
    Bool $is_method = 0
    ) {

    my ($method,$obj) = ('','');
    if ($is_method) {
        $method = 'method';
        $obj    = 'obj'; 
    }

    print "\nAlternatives:\n1:\tmulti $method a ($m1)\n2:\tmulti $method a ($m2)\n";
    print "Call:\t";
    print           "\$obj\." if $is_method;
    print                 "a($call)\nWill Pugs dispatch to {1} or {2}? ";

    my $answer = $is_method 
        ?? eval q:s/ { 
                            class Foo { 
                                multi method a ($m1) {1}
                                multi method a ($m2) {2}
                            };
                            Foo.new.a($call);
                }/
        !! eval q:s/ { multi a ($m1) {1} multi a ($m2) {2} a($call) } / ;

    if defined $answer {
        is(
            =$*IN,
            $answer,
            "Arguments ($call) to signatures ($m1) and ($m2)"
        );
    }
    else {
        ok(0, "Failed to compile test! error was was: $!" )
    }


}

# Originally by Mark Stosberg

