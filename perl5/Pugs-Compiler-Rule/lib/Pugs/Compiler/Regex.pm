package Pugs::Compiler::Regex;

# Version in Pugs::Compiler::Rule
# Documentation in the __END__
use 5.006;
use strict;
use warnings;

use Pugs::Grammar::Base;  # not 'use base'
use Pugs::Grammar::Rule;
#use Pugs::Runtime::Rule;
use Pugs::Runtime::Match;
use Pugs::Emitter::Rule::Perl5;
use Pugs::Emitter::Rule::Perl5::Ratchet;
use Pugs::Compiler::RegexPerl5;

use Carp 'croak';
use Data::Dump::Streamer;
use Symbol 'qualify_to_ref';
use Digest::MD5 'md5_hex';

my $cache;
eval {
    require Cache::FileCache;
    $cache = new Cache::FileCache( { 'namespace' => 'v6-rules' } );
};

sub new { $_[0] }

sub compile {
    
    # $class->compile( $source )
    # $class->compile( $source, { p=>1 } )
    # $class->compile( $source, { signature => $sig } ) -- TODO

    my ( $class, $rule_source, $param ) = @_;

    return Pugs::Compiler::RegexPerl5->compile( $rule_source, $param )
        if exists $param->{P5} || exists $param->{Perl5};
#warn length($rule_source);

    my $self = { source => $rule_source };

    #print Dump @_;

    # XXX - should use user's lexical pad instead of an explicit grammar?
    $self->{grammar}  = delete $param->{grammar}  || 
                        'Pugs::Grammar::Base';
    $self->{ratchet}  = delete $param->{ratchet}  || 
                        0;
    $self->{p}        = delete $param->{pos}      ||
                        delete $param->{p};
                        # default = undef;
    $self->{sigspace} = delete $param->{sigspace} ||
                        delete $param->{s}        || 
                        0;

    warn "Error in rule: unknown parameter '$_'" 
        for keys %$param;

    my $digest = md5_hex(Dump($self));
    my $cached;

    if ($cache && ($cached = $cache->get($digest))) {
	$self->{perl5} = $cached;
    }
    else {

        #print 'rule source: ', $self->{source}, "\n";
        $self->{ast} = Pugs::Grammar::Rule->rule( 
            $self->{source} );
        die "Error in rule: '$rule_source' at: '$self->{ast}{tail}'\n" if $self->{ast}{tail};
        #print 'rule ast: ', do{use Data::Dump::Streamer; Dump($self->{ast}{capture})};

        if ( $self->{ratchet} ) {
            $self->{perl5} = Pugs::Emitter::Rule::Perl5::Ratchet::emit( 
                 $self->{grammar}, $self->{ast}{capture}, $self );
        }
        else {
            $self->{perl5} = Pugs::Emitter::Rule::Perl5::emit( 
                $self->{grammar}, $self->{ast}{capture}, $self );
        }
        #print 'rule perl5: ', do{use Data::Dump::Streamer; Dump($self->{perl5})};

        $cache->set($digest, $self->{perl5}, 'never') if $cache;
    }

    local $@;
    $self->{code} = eval 
        $self->{perl5};
    die "Error in evaluation: $@\nSource:\n$self->{perl5}\n" if $@;

    bless $self, $class;
}

sub code { 
    my $rule = shift; 
    sub { 
        # XXX - inconsistent parameter order - could just use @_, or use named params
        my ( $grammar, $str, $flags, $state ) = @_; 
        $rule->match( $str, $grammar, $flags, $state ); 
    } 
}

sub match {
    my ( $rule, $str, $grammar, $flags, $state ) = @_; 
    
    return Pugs::Runtime::Match->new( { bool => 0 } )
        unless defined $str;   # XXX - fix?
        
    $grammar ||= $rule->{grammar};
    #print "match: grammar $rule->{grammar}, $_[0], $flags\n";
    #print "match: Variables: ", Dump ( $flags->{args} ) if defined $flags->{args};

    my $p = defined $flags->{p} 
            ? $flags->{p} 
            : $rule->{p};

    if ( defined $p ) {
        #print "flag p";
        #print "match: grammar $rule->{grammar}, $str, %$flags\n";
        #print $rule->{code};

        # XXX BUG! - $rule->{code} disappeared - in t/08-hash.t ???
        unless ( defined $rule->{code} ) {
            local $@;
            $rule->{code} = eval 
                $rule->{perl5};
            die "Error in evaluation: $@\nSource:\n$rule->{perl5}\n" if $@;
        }
        
        my %args;
        %args = %{$flags->{args}} if defined $flags && defined $flags->{args};
        $args{p} = $p;
        
        my $match = $rule->{code}( 
            $grammar,
            $str, 
            $state,
            \%args,
        );
        #$p = 0 if $p eq 'undef';  # XXX - bug - 'undef' as string in t\06-subrule.t
        eval { $match->data->{from} = \(0 + $p) };   # XXX
        return $match;  
    }

    foreach my $i (0..length($str)) {
        my $match = $rule->{code}( 
            $grammar,
            substr($str, $i),
            $state,
            $flags->{args},
        );
        $match or next;   
        eval { $match->data->{from} = $i unless defined $match->data->{from} };   # XXX
        return $match;  
    }
    return Pugs::Runtime::Match->new( { bool => 0 } );   # XXX - fix?
}

sub install {
  my($class, $name, @etc) = @_;

  ## If we have a fully qualified name, use that, otherwise extrapolate.
  my $rule = index($name, '::') > -1 ? $name : scalar(caller)."::$name";
  my $slot = qualify_to_ref($rule);

  croak "Can't install regex '$name' as '$rule' already exists"
    if *$slot{CODE};

  *$slot = $class->compile(@etc)->code;
}

sub _str { defined $_[0] ? $_[0] : 'undef' }
sub _quot { 
    my $s = $_[0];
    $s =~ s/\\/\\\\/sg;
    return $s;
}

sub perl5 {
    my $self = shift;
    return "bless {\n" . 
        "  grammar "  .  "=> q(" . _str( $self->{grammar} )  . "),\n" . 
        "  ratchet "  .  "=> q(" . _str( $self->{ratchet} )  . "),\n" . 
        "  p "        .  "=> " . _str( $self->{p} )        . ",\n" . 
        "  sigspace " .  "=> q(" . _str( $self->{sigspace} ) . "),\n" . 
        "  code "     .  "=> "   . $self->{perl5}    . ",\n" . 
        "  perl5 "    .  "=> q(" . _quot( $self->{perl5} )  . "), }, " . 
        "q(" . ref($self) . ")";
}

1;

__END__

=head1 NAME 

Pugs::Compiler::Regex - Compiler for Perl 6 Regex

=head1 DESCRIPTION

This module provides an implementation for Perl 6 Regex.
See L<Pugs::Compiler::Rule> for documentation.

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
