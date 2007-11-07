
use lib 
    'lib5', 
    '../v6-MiniPerl6/lib5';  # KindaPerl6::Visitor::EmitPerl5 needs this, because it is compiled with mp6
use KindaPerl6::Perl5::Runtime;
use KindaPerl6::Visitor::EmitPerl5;
use KindaPerl6::Perl5::Code;

package Main;
use Data::Dump::Streamer;

my $s1 = Code->new(
    pad => Pad->new(
        # my $x = 3;  # XXX - no initializers yet
        lexicals => [
            ( bless {
                decl => 'my',
                type => undef,
                var  =>                
                    ( bless {
                        sigil   => '$',
                        twigil  => '',
                        name    => 'x',
                      }, 'Var' 
                    ),            
              }, 'Decl'
            ),
        ],
    ),
    native_source => ' sub { $x++ } ',
    ast => undef,  # XXX
);

$s1->{pad}->eval( ' $x = 3 ' );   # initialize
#print Dump( $s1 );

print "s1: ",$s1->apply(), "\n";

print "clone\n";
my $s2 = $s1->clone;

print "s1: ",$s1->apply(), "\n";
print "s2: ",$s2->apply(), "\n";
print "s1: ",$s1->apply(), "\n";
print "s2: ",$s2->apply(), "\n";

1;

__END__
package main;

my $s1_clone;
my $s1 = bless { 
    my $x = 3;
    code  => sub { $x++ },
    clone => sub { my $x = $x; 
        code  => sub { $x++ },
        clone => ...,   # deep copy, OUTER?
    },
    $s1_clone = sub { sub { $x++ } };
    $s1_clone->();
};

print "s1: ",$s1->(), "\n";
print "clone\n";
my $s2 = $s1_clone->();
print "s1: ",$s1->(), "\n";
print "s2: ",$s2->(), "\n";
print "s1: ",$s1->(), "\n";
print "s2: ",$s2->(), "\n";

1;

__END__
=pod

* statements
* ast
* lexicals
* @CHECK
* signature
* clone
* perl

Lit::Code 
    %.pad;         #  is Mapping of Type; # All my/state/parameter variables
    %.state;       #  is Mapping of Exp;  # State initializers, run upon first entry 
    $.sig;         #  is Sig              # Signature
    @.body;        #  is Seq of Exp;      # Code body 
Pad
    $.outer
    @.lexicals
    $.namespace

=end

