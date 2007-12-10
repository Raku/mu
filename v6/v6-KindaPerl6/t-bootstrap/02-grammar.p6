
# how to run this program before bootstrapping:
#
# $ perl script/kp6 -Cperl5rx t-bootstrap/02-grammar.p6 | perl -I compiled/perl5-kp6-kp6/lib
#

say "1..15";
my $count=0;
sub ok($ok,$desc) {
    $count = $count + 1;
    if ($ok) {
        say "ok $count - $desc";
    } else {
        say "not ok $count - $desc";
    }
}

# digits

say "# ** now testing: <digits>";
$_ = '123';
my $MATCH = KindaPerl6::Grammar.digits();
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq '123','digits');

# val_int

say "# ** now testing: <val_int>";
$_ = '123';
my $MATCH = KindaPerl6::Grammar.val_int();
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Val::Int.new(int => '123')",'val_int');

# simple term

say "# ** now testing: <term>";
$_ = '...';
my $MATCH = KindaPerl6::Grammar.term();
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Apply.new(arguments => [  ], code => Var.new(namespace => [  ], name => 'die', twigil => '', sigil => '&'))",'term ...');

# simple term

say "# ** now testing: <term>";
$_ = 'Inf';
my $MATCH = KindaPerl6::Grammar.term();
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Apply.new(arguments => [  ], code => Var.new(namespace => [  ], name => 'Inf', twigil => '', sigil => '&'))",'term Inf');


# sigil 

say "# ** now testing: <sigil> with \$";
$_ = '$';
my $MATCH = KindaPerl6::Grammar.sigil();
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq '$','sigil $');


# $/ 

say "# ** now testing: <var> with \$/";
$_ = '$/';
my $MATCH = KindaPerl6::Grammar.var();
say "# survived";
say "# ",($MATCH.true);
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Var.new(namespace => [  ], name => '/', twigil => '', sigil => '$')",'$/');


# $var 

say "# ** now testing: <var>";
$_ = '$var';
my $MATCH = KindaPerl6::Grammar.var();
# say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Var.new(namespace => [  ], name => 'var', twigil => '', sigil => '$')",'$var');


# $var term

say "# ** now testing: <term> with \$var";
$_ = '$var';
my $MATCH = KindaPerl6::Grammar.term();
# say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Var.new(namespace => [  ], name => 'var', twigil => '', sigil => '$')",'$var term');


#term

say "# ** now testing: <term>";
$_ = '123';
my $MATCH = KindaPerl6::Grammar.term();
# say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Val::Int.new(int => '123')",'term');


#exp

say "# ** now testing: <exp>";
$_ = '123';
my $MATCH = KindaPerl6::Grammar.exp();
# say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Val::Int.new(int => '123')",'term');


#exp

say "# ** now testing: bigger <exp>";
$_ = '123+456';
my $MATCH = KindaPerl6::Grammar.exp();
# say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Apply.new(arguments => [ Val::Int.new(int => '123'), Val::Int.new(int => '456') ], code => Var.new(namespace => [  ], name => 'infix:<+>', twigil => '', sigil => '&'))",
    'term');



say "# ** now testing: -> \$param { Inf } <term>";
$_ = '-> $param { Inf }';
my $MATCH = KindaPerl6::Grammar.term();
# say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Sub.new(name => undef, block => Lit::Code.new(body => [ Apply.new(arguments => [  ], code => Var.new(namespace => [  ], name => 'Inf', twigil => '', sigil => '&')) ], sig => Sig.new(invocant => Val::Undef.new(), positional => [ Lit::SigArgument.new(is_multidimensional => Val::Bit.new(bit => 0), has_default => Val::Bit.new(bit => undef), value => undef, is_slurpy => Val::Bit.new(bit => 0), is_optional => Val::Bit.new(bit => 0), key => Var.new(namespace => [  ], name => 'param', twigil => '', sigil => '$'), is_copy => Val::Bit.new(bit => 0), is_named_only => Val::Bit.new(bit => 0), type => '', is_rw => Val::Bit.new(bit => 0)) ]), pad => Pad.new( ... ), CATCH => undef, state => {  }))",
    'term');



say "# ** now testing: -123 <term>";
$_ = '-(123)';
my $MATCH = KindaPerl6::Grammar.term();
#say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Apply.new(arguments => [ Val::Int.new(int => '123') ], code => Var.new(namespace => [  ], name => 'prefix:<->', twigil => '', sigil => '&'))",
    'term');


say "# ** now testing: list <term>";
$_ = '(456)';
my $MATCH = KindaPerl6::Grammar.term();
#say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Val::Int.new(int => '456')",
    'term');


say "# ** does it work yet? <exp_stmts>";
$_ = '123; 456';
my $MATCH = KindaPerl6::Grammar.exp_stmts();
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Val::Int.new(int => '123')",
    'exp_stmts');


say "# ** does it work yet? <parse>";
$_ = '123';
my $MATCH = KindaPerl6::Grammar.parse();
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Val::Int.new(int => '123')",
    'parse');


=begin

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
