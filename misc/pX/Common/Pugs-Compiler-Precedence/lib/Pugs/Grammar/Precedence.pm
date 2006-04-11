package Pugs::Grammar::Precedence;

# Documentation in the __END__
use 5.006;
use strict;
use warnings;

my %relative_precedences = (
    tighter => sub {
        splice( @{$_[0]->{levels}}, $_[1], 0, [ $_[2] ] );
    },
    looser  => sub {
        splice( @{$_[0]->{levels}}, $_[1]+1, 0, [ $_[2] ] );
    },
    equal   => sub {    
        push @{$_[0]->{levels}[$_[1]]}, $_[2];
    },
);

# note: S06 - 'chain' can't be mixed with other types in the same level
my %rule_templates = (
    prefix_non =>        
        "'name' exp         \n\t{ \$out= {op1 => 'name', exp1 => \$_[2],} }", 
    circumfix_non =>     
        "'name' exp 'name2' \n\t{ \$out= {op1 => 'name', op2 => 'name2', exp1 => \$_[2],} }",  
    infix_right =>       
        "exp 'name' exp     \n\t{ \$out= {op1 => 'name', exp1 => \$_[1], exp2 => \$_[3],} }",
    postfix_non =>       
        "exp 'name'         \n\t{ \$out= {op1 => 'name', exp1 => \$_[2],} }", 
    postcircumfix_non => 
        "exp 'name' exp 'name2' \n\t{ \$out= {op1 => 'name', op2 => 'name2', exp1 => \$_[2], exp2 => \$_[4],} }", 
    infix_left =>        
        "exp 'name' exp     \n\t{ \$out= {op1 => 'name', exp1 => \$_[1], exp2 => \$_[3],} }", 
    infix_non =>         
        "exp 'name' exp     \n\t{ \$out= {op1 => 'name', exp1 => \$_[1], exp2 => \$_[3],} }", 
    ternary_non =>       
        "exp 'name' exp 'name2' exp \n\t{ \$out= {op1 => 'name', op2 => 'name2', exp1 => \$_[1], exp2 => \$_[3], exp3 => \$_[5],} }",
        
    # XXX
    infix_chain =>       
        "exp 'name' list_right  \n\t{ \$out= {op1 => 'name', exp1 => \$_[1], exp2 => \$_[3],} }",
    infix_list =>        
        "exp 'name' list_right \n\t{ \$out= {op1 => 'name', exp1 => \$_[1], exp2 => \$_[3],} }", 
);

sub new {
    my $class = shift;
    my $self = { levels => [], @_ };
    bless $self, $class; 
}

sub add_op {
    my ($self, $opt) = @_;
    #print "adding $opt->{name}\n";
    $opt->{assoc}  = 'non'    unless defined $opt->{assoc};
    $opt->{fixity} = 'prefix' unless defined $opt->{fixity};
    #my $fixity = $opt->{fixity};
    #$fixity .= '_' . $opt->{assoc} if $opt->{fixity} eq 'infix';
    for my $level ( 0 .. $#{$self->{levels}} ) {
        if ( grep { $_->{name} eq $opt->{other} } @{$self->{levels}[$level]} ) {
            #print "pos $level at $opt->{precedence} $opt->{other}\n";
            $relative_precedences{$opt->{precedence}}->($self, $level, $opt);
            return;
        }
    }
    if ( ! defined $opt->{precedence} ) {
        push @{$self->{levels}}, [ $opt ];
        return;
    }
    die "there is no precedence like ", $opt->{other};
}

sub emit_yapp {
    my ($self) = @_;
    my $s = "%{ my \$out; %}\n";
    for my $level ( reverse 0 .. $#{$self->{levels}} ) {            
        my %assoc;
        for ( @{$self->{levels}[$level]} ) {
            push @{$assoc{ $_->{assoc} }}, $_;
        }
        for ( keys %assoc ) {
            if ( @{$assoc{$_}} ) {
                my $a = $_;
                $a = 'nonassoc' if $a eq 'non';
                $a = 'left'     if $a eq 'list';
                $s .= "%$a " . 
                    join( ' ', map { "'$_->{name}'" } @{$assoc{$_}} ) .
                    "\n";
            }
        }
    }
    $s .= "%%\n" .
        "statement:  exp { return(\$out) } ;\n" .
        
        # XXX
        "list_left:  exp { \$out= [ \$_[1] ] }\n" .
        "    |   list_left ',' exp   { \$out= [ \@{\$_[1]}, \$_[3] ] } ;\n" .
        "list_right: exp { \$out= [ \$_[1] ] }\n" .
        "    |   exp ';' list_right  { \$out= [ \$_[1], \@{\$_[3]} ] } ;\n" .
        
        "exp:   NUM  { \$out= \$_[1] }\n" .
        "    |  VAR  { \$out= \$_[1] }\n" ;
    for my $level ( 0 .. $#{$self->{levels}} ) {            
        for my $op ( @{$self->{levels}[$level]} ) {
            my $t = $rule_templates{"$op->{fixity}_$op->{assoc}"};
            $t =~ s/$_/$op->{$_}/g for qw( name2 name );
            $s .= "    |  $t \n\t/* $op->{name} $op->{fixity} $op->{assoc} */\n";
        }
    }
    $s .= ";\n" .
        "%%\n";
    #print $s;
    return $s;
}

sub emit_grammar_perl5 {
    my $self = shift;
    my $g = $self->emit_yapp();
    my($p)=new Parse::Yapp(input => $g);
    return $p->Output(classname => $self->{grammar} );
}

sub exists_op { die "not implemented" };
sub delete_op { die "not implemented" };
sub get_op    { die "not implemented" };
sub inherit_category { die "not implemented" };
sub inherit_grammar  { die "not implemented" };
sub merge_category   { die "not implemented" };
sub code  { die "not implemented" }
sub match { die "not implemented" }
sub perl5 { die "not implemented" }

1;

__END__

=head1 NAME 

Pugs::Grammar::Precedence - Engine for Perl 6 Rule operator precedence

=head1 SYNOPSIS

  use Pugs::Grammar::Precedence;

  # example definition for "sub rxinfix:<|> ..."
  
  my $rxinfix = Pugs::Grammar::Precedence->new( 
    grammar => 'rxinfix',
  );
  $rxinfix->add_op( 
    name => '|',
    assoc => 'left',
    fixity => 'infix',
  );

Pseudo-code for usage inside a grammar:

    sub new_proto( $match ) {
        return ${$match<category>}.add_op( 
            name => $match<name>, 
            fixity => ..., 
            precedence => ...,
        );
    }

    rule prototype {
        proto <category>:<name> <options>
        { 
            return new_proto($/);
        }
    }

    rule statement {
        <category.parse> ...
    }

=head1 DESCRIPTION

This module provides an implementation for Perl 6 operator precedence.  

=head1 METHODS

=head2 new ()

Class method.  Returns a category object.

options:

=item * grammar => $category_name - the name of this category 
(a namespace or a Grammar name).

=head2 add_op ()

Instance method.  Adds a new operator to the category.

options:

=item * name => $operator_name - the name of this operator, such as '+', '*'

=item * name2 => $operator_name - the name of the second operator in
an operator pair, such as circumfix [ '(', ')' ] or ternary [ '??', '!!' ]. 

 # precedence=>'tighter', 
 #   tighter/looser/equiv 
 # other=>'+', 
 # fixity => 
 #  infix/prefix/circumfix/postcircumfix/ternary
 # assoc =>
 #  left/right/non/chain/list
 # rule=>$rule 
 #  (is parsed) 

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

Summary of Perl 6 Operators: L<http://dev.perl.org/perl6/doc/design/syn/S03.html>

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
