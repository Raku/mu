package Pugs::Grammar::Category;

# Documentation in the __END__
use 5.006;
use strict;
use warnings;

use Pugs::Grammar::Base;  # not 'use base'
use Pugs::Grammar::Rule;
use Pugs::Runtime::Match;
use Pugs::Emitter::Rule::Perl5;

my %relative_precedences = (
    tighter => sub { splice( @{$_[0]->{levels}}, $_[1]-1, 0, [ $_[2] ] ) },
    looser  => sub { splice( @{$_[0]->{levels}}, $_[1]+1, 0, [ $_[2] ] ) },
    equal   => sub {   push( @{$_[0]->{levels}[$_[1]]}, $_[2] ) },
);

# parsed: - the rule replaces the right side
# note: S06 - 'chain' can't be mixed with other types in the same level
my %rule_templates = (
    prefix =>          '<op> <equal>', 
    circumfix =>       '<op> <parse> <op2>',  
    infix_right =>     '<equal> <op> <tight>',
    postfix =>         '<tight> <op>', 
    postcircumfix =>   '<tight> ( <op> <parse> <op2> )+', 
    infix_left =>      '<tight> <op> <equal>', 
    infix_non =>       '<tight> <op> <tight>', 
    infix_chain =>     '<tight> ( <op> <tight> )+',
    infix_list =>      '<tight> ( <op> <tight> )+',
    ternary =>         '<tight> <op> <parse> <op2> <equal>',
);

sub new {
    my ($class, $opt) = @_;
    my $self = { levels => [], %$opt };
    bless $self, $class; 
}

sub exists_op { die "not implemented" };
sub delete_op { die "not implemented" };
sub get_op    { die "not implemented" };
sub inherit_category { die "not implemented" };
sub inherit_grammar  { die "not implemented" };
sub merge_category   { die "not implemented" };

sub add_op {
    my ($self, $opt) = @_;
    #print "adding $opt->{name}\n";
    $opt->{assoc} = 'left' unless defined $opt->{assoc};
    for ( 0 .. $#{$self->{levels}} ) {
        if ( grep { $_->{name} eq $opt->{other} } @{$self->{levels}[$_]} ) {
            $relative_precedences{$opt->{precedence}}->($self, $_, $opt);
            return;
        }
    }
    push @{$self->{levels}}, [ $opt ];
}

sub code { 
    warn "not implemented";
}

sub match {
    warn "not implemented";
}

sub perl5 {
    warn "not implemented";
}

sub emit_perl6_rule {
    my ($self, $level, $default) = @_;
    my @rules;
    my $tight = $level ? 'r' . ($level - 1) : $self->{operand};
    my $equal = "r$level";
    $equal = "parse" if $level == $#{$self->{levels}};
    for my $op ( @{$self->{levels}[$level]} ) {
        my $rule = $op->{fixity};
        $rule .= '_' . $op->{assoc} if $rule eq 'infix';
        my $template = $rule_templates{$rule};
        
        # is-parsed
        if ( $op->{rule} ) {
            $template =~ s/<op>.*/<op> $op->{rule}/sg;
        }
        
        my $term0 = $template =~ s/<tight>/\$<term0>:=(<$tight>)/sgx;
        #$template =~ s/<tight>/<$tight>/sgx;
        
        my $term2 = $template =~ s/<parse>/\$<term2>:=(<parse>)/sgx;
        
        my $term1 = $template =~ s/<equal>/\$<term1>:=(<$equal>)/sgx;
        #$template =~ s/<equal>/<$equal>/sgx;
        
        $template =~ s/<op>   /\$<op1>:=(<'$op->{name}'>)/sgx;
        my $op2 = $template =~ s/<op2>  /\$<op2>:=(<'$op->{name2}'>)/sgx;
        
	#print "fixity: ", $op->{fixity}, "\n";
        $template .= ' { return Pugs::AST::Expression->operator($/, fixity => "' . $op->{fixity} . '", assoc => "' . $op->{assoc} . '" ) } ';
        #print "Added rule: $template\n";
        
        push @rules, $template;
    }
    push @rules, '$<term>:=(<' . $tight . '>) { return Pugs::AST::Expression->term($/) } ';

    # @rules = _optimize( @rules );
    
    return $rules[0] unless $#rules;
    @rules = map { "   [ $_ ] \n" } @rules;
    #print "Rules: \n@rules\n";
    return "[ " . join( ' | ', @rules ) . " ]";
}

sub emit_grammar_perl6 {
    # item=>'<item>',
    my ($self, $default) = @_;
    #print "emitting grammar in perl6\n";
    my $s = "";
    for my $level ( 0 .. $#{$self->{levels}} ) {
        my $equal = "r$level";
        $equal = "parse" if $level == $#{$self->{levels}};
        $s = $s . "    rule $equal { " .
            emit_perl6_rule($self, $level, $default) .
            " }\n";
    }
    return "grammar $self->{name} { \n$s}\n";
}

sub _quotemeta { my $s = shift; $s =~ s!'!\\'!g; $s }

sub emit_grammar_perl5 {
    # item=>'<item>',
    my ($self, $default) = @_;
    #print "emitting grammar in perl5\n";
    my $s = "";
    for my $level ( 0 .. $#{$self->{levels}} ) {
        my $equal = "r$level";
        $equal = "parse" if $level == $#{$self->{levels}};
        $s = $s . "    *$equal = Pugs::Compiler::Rule->compile( '" .
            _quotemeta( emit_perl6_rule($self, $level, $default) ) .
            "' )->code;\n";
    }
    return $s;  #"package $self->{name};\n$s\n";
}

sub _optimize {
    my @rules = reverse sort @_;
    my $i = 0;
    my $j = $i;
    my ($item) = $rules[$i] =~ /^(<.*?>)/;
    #print "item: $item\n";
    for my $i ( $i .. $#rules ) {
        last unless $rules[$i] =~ /^$item/;
        #print "optimize: $rules[$i]\n";
        $j = $i;
    }
    if ( $i != $j ) {
        #print "optimize $item in @rules[$i..$j]\n";
        my $len = length( $item );
        my @r = map { $_ eq $item ? '' : substr($_,$len) } @rules[$i..$j];
        pop @r if $r[-1] eq '';
        #print "optimized: [",join(",",@r),"] $i $j\n";
        splice( @rules, $i, (1+$j-$i), 
            "$item [ " . join( ' | ', @r ) . " ]?"
        );
        #print "optimized: [",join(",",@rules),"]\n";
    }
    return @rules;
}

1;

__END__

=head1 NAME 

Pugs::Grammar::Category - Engine for Perl 6 Rule categories

=head1 SYNOPSIS

  use Pugs::Grammar::Category;

  # example definition for "sub rxinfix:<|> ..."
  
  my $rxinfix = Pugs::Grammar::Category->new( 
    name => 'rxinfix',
    operand => 'rxatom',
    # XXX - default assoc, default fixity ?
  );
  $rxinfix->add_op( 
    name => '|',
    assoc => 'left',
    fixity => 'infix',
    block => sub {...},   # XXX - the return block could be generated automatically ?
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

This module provides an implementation for Perl 6 Rule categories.  

The module is still very unstable, and the algorithm is not optimized for speed yet.

=head1 METHODS

=head2 new ()

Class method.  Returns a category object.

options:

=item * name => $category_name - the name of this category 
(a namespace or a Grammar name).

=item * operand => $rule_name - the name of the rule that will parse operands.

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
 # block => ???
 #  (will do) 

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 Rules Spec: L<http://dev.perl.org/perl6/doc/design/syn/S05.html>

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
