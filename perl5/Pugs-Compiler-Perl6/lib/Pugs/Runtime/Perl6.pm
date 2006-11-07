
package Pugs::Runtime::Perl6;

use strict;
use warnings;

use Data::Dumper;
use Data::Bind;
#use Lexical::Alias;
use Sub::Multi;
use PadWalker;
use IO::File ();
use Pugs::Compiler::Regex ();
use List::Util; # 'reduce'

$::_V6_BACKEND = 'BACKEND_PERL5';

# TODO - see Pugs::Runtime::Grammar for metaclass stuff

use constant Inf => 100**100**100;
use constant NaN => Inf - Inf;

sub perl {
    local $Data::Dumper::Terse    = 1;
    my $can = UNIVERSAL::can($_[0] => 'perl');
    if ($can) {
        $can->($_[0]);
    }
    else {
        Dumper($_[0]);
    }
}

sub yaml {
    my $can = UNIVERSAL::can($_[0] => 'yaml');
    if ($can) {
        $can->($_[0]);
    }
    else {
        require YAML::Syck;
        YAML::Syck::Dump($_[0]);
    }
}

    sub setup_type {
        my $type = $_[0];
        eval q!
            package Pugs::Runtime::Perl6::ReadWrite! . $type . q!;
                use overload (
                    'x='  => sub { 
                        die 'Trying to store an object of wrong type'
                            unless ref( $_[1] ) =~ /::! . $type . q!$/;
                        ${$_[0]} = $_[1];
                        $_[0];
                    },
                    '='  => sub { 
                        die 'Trying to store an object of wrong type'
                            unless ref( $_[1] ) =~ /::! . $type . q!$/;
                        ${$_[0]} = $_[1];
                        $_[0];
                    },
                    fallback => 0,
                );
            package Pugs::Runtime::Perl6::ReadOnly! . $type . q!;
                use overload (
                    'x='  => sub { 
                        die "Modification of a read-only value attempted";
                    },
                    '='  => sub { 
                        die "Modification of a read-only value attempted";
                    },
                    fallback => 0,
                );
        !;
    }
    
    setup_type( $_ ) for qw( Bool Str Int Num );
        
    sub pad_depth {
        local $@;
        my $idx = 0;
        $idx++ while eval { PadWalker::peek_my($idx) };
        $idx;
    }
    
    sub eval_preprocess {
        my ($string, $lang);
        Data::Bind->arg_bind(\@_);
        $lang ||= 'perl6';
        my $eval_string;
        Data::Bind::bind_op2(\$eval_string, \$string);
        # print "LANG: $lang\n";
        if ($lang eq 'yaml') {
            # print "YAML: $eval_string\n";
            my $code = 
            'do{
                require YAML::Syck;
                # interoperability with other YAML/Syck bindings:
                $YAML::Syck::ImplicitTyping = 1;
                YAML::Syck::Load(\'' . $string . '\' );
            }';
            Data::Bind::bind_op2(\$eval_string, \$code);
            # print "YAML: $eval_string\n";
        }
        elsif ($lang eq 'perl6') {
            require Pugs::Compiler::Perl6;
            my $p6 = Pugs::Compiler::Perl6->compile( $string );
            Data::Bind::bind_op2(\$eval_string, \$p6->{perl5});
        }
        elsif ($lang ne 'perl5') {
            die;
        }
        return $eval_string;
    }
    
    Data::Bind->sub_signature(\&eval_preprocess, { var => '$string' }, { var => '$lang', optional => 1});
    
    sub setup_class {
        my ($class) = caller;
        no strict 'refs';
        my @foo = split /::/, $class;
        my $last = pop @foo;
        no strict 'refs';
        no warnings 'redefine';  # Moose already does this?
        *{'::'.$class} = sub {
            my ($pos, $nam) = @_;
            if (@$pos == 1) {
                my ($payload) = @$pos;
                bless($$payload, $class->meta->name);
            }
            elsif (@$pos) {
                bless([map { $$_ } @$pos], $class->meta->name);
            }
            elsif (%$nam) {
                bless({ map { $_ => ${$nam->{$_}} } keys %$nam }, $class->meta->name);
            }
            else {
                $class->meta->name;
            }
        };
    }

package Pugs::Runtime::Perl6::IO;
    use base 'IO::Handle';
    
    unless ( defined $::_V6_STDIN ) {
        $::_V6_STDIN = new Pugs::Runtime::Perl6::IO;
        unless ($::_V6_STDIN->fdopen(fileno(STDIN),"r")) {
            warn "Can't open \$*IN";
        }
    }
    
    sub slurp {
        my $self = $_[0];
        my $content;
        local $/; 
        $content = <$self>;
        return bless \$content, 'Pugs::Runtime::Perl6::Scalar';
    }

package Pugs::Runtime::Perl6::Routine;
    use B ();
    use Devel::Caller ();
    
    sub new {
        my ($class, $cv) = @_;
        bless { cv => B::svref_2object($cv) }, $class;
    }
    
    sub code {
        $_[0]->{cv}->object_2svref;
    }
    
    sub name {
        my $self = shift;
        my $cv = $self->{cv};
        return '&'.$cv->GV->STASH->NAME . '::' . $cv->GV->NAME;
    }
    
    sub package {
        $_[0]->{cv}->GV->STASH->NAME;
    }
    
    sub arity {
        my $cv = Data::Bind::_get_cv($_[0]->code);
        use Data::Dumper;
        return *$cv->{sig} ? *$cv->{sig}->arity : 0;
    }

package Pugs::Runtime::Perl6::ReadWrite;
    use overload (
        'x='  => sub { 
            ${$_[0]} = $_[1];
            $_[0];
        },
        '='  => sub { 
            ${$_[0]} = $_[1];
            $_[0];
        },
        fallback => 0,
    );
package Pugs::Runtime::Perl6::ReadOnly;
    use overload (
        'x='  => sub { 
            die "Modification of a read-only value attempted";
        },
        '='  => sub { 
            die "Modification of a read-only value attempted";
        },
        fallback => 0,
    );

package Pugs::Runtime::Perl6::Scalar;
    use Scalar::Util qw(looks_like_number);
    
    sub perl {
        local $Data::Dumper::Terse    = 1;
        local $Data::Dumper::Sortkeys = 1;
        my $dumped = join(', ', Data::Dumper::Dumper(@_));
        $dumped =~ s/\n$//;
        return $dumped;
    }
        
    sub yaml {
        require YAML::Syck;
        no warnings 'once';
        # interoperability with other YAML/Syck bindings:
        $YAML::Syck::ImplicitTyping = 1;
        YAML::Syck::Dump( $_[0] );
    }
   
    sub join {
        # print "Trying to join a Scalar: ", Data::Dumper::Dumper(@_); 
        my $str = pop @_;
        join( $str, @_ );
    }

    sub defined { CORE::defined(@_) }
    
    # TODO - rename this to 'WHAT'
    sub ref : method {
        # XXX: should use Data::Bind callconv
        #print "ref: ", Data::Dumper::Dumper( @_ );
        my $self = $_[0];
        eval {
          return $$self->WHAT
            if UNIVERSAL::can( $$self, 'WHAT' );
          return 'Scalar'
            if ! defined $$self;
        };
        
        my $ref = CORE::ref(@_);
        
        return 'Code'  if ref($self) eq 'CODE';
        return 'Hash'  if ref($self) eq 'HASH';
        return 'Array' if ref($self) eq 'ARRAY';
        unless ($ref) {
            return 'Num' if looks_like_number($self);
            return 'Str';
        }
        if ($self->can('meta')) {
            return $self->meta->name;
        }
        die 'unknown type';
    }
    
    sub isa {
        my $self = $_[0];
        return $self->isa( $_[1] )
            if     Scalar::Util::blessed $self 
                && UNIVERSAL::can( $self, 'isa' );
        return 1 if $_[1] eq 'Hash'  && ref($_[0]) eq 'HASH';
        return 1 if $_[1] eq 'Array' && ref($_[0]) eq 'ARRAY';
        return 1 if $_[1] eq 'Str'   && defined $_[0];
        return 1 if $_[1] eq 'Num'   && defined $_[0];
        return 1 if $_[1] eq 'Code'  && ref($_[0]) eq 'CODE';
        return 0;
    }
    
    sub eval { 
        my $s = ${$_[0]};
        #warn "eval $s\n";
        Pugs::Runtime::Perl6::eval( [ \$s, \'perl6' ], {} )     # '
    }
    
    sub sort { 
        sort @_ 
    }
    
    sub chars { 
        length "@_" 
    }
    
    sub reverse { 
        my $s = reverse $_[0];
        $s;
    }
    
    sub words { 
        # todo - parameter handling
        my $s = $_[0];
        $s =~ s/^\s+//;
        my @tmp = split( /\s+/, $s ); 
    }

    sub value { 
        # XXX misdispatched pair.value
        $_[0]
    }
    sub key { 
        # XXX misdispatched pair.key
        $_[0]
    }
    sub values { 
        # XXX misdispatched hash/array.values
        $_[0]
    }
    
package Pugs::Runtime::Perl6::Array;

    sub map {
        my ($code, @array);
        Data::Bind->arg_bind(\@_);
        my $run = ref($code) eq 'Pugs::Runtime::Perl6::Routine' ? $code->code : $code;
        my $arity = Pugs::Runtime::Perl6::Routine->new($run)->arity || 1;
    
        return map $run, @array if $arity == 1;
    
        my @result;
        my $i = 0;
        while ($i <= $#array) {
      my @x = @array[$i..$i+$arity-1];
      push @result, $run->([map { \$_ } @x], {});
      $i += $arity;
        }
        return @result;
    }
    
    Data::Bind->sub_signature(\&map, { var => '$code', type => 'Code' }, { var => '@array'} );

package Pugs::Runtime::Perl6::Hash;
    use overload (
        '""'     => \&str,
        '0+'     => sub { scalar keys %{$_[0]} },
        'bool'   => sub { 1 },
        fallback => 1,
    );
    sub str {
        join( "\n",
            map {
                $_ . "\t" . $_[0]{$_}
            }
            keys %{$_[0]}
        );
    }

package Pugs::Runtime::Perl6::Bool;
    use overload (
        '""'     => sub { ${$_[0]} ? 'Bool::True' : 'Bool::False' },
        '0+'     => sub { ${$_[0]} },
        'bool'   => sub { ${$_[0]} },
        fallback => 1,
    );
    sub WHAT { 
        'Bool';  # XXX box
    }
    sub isa { $_[0]->WHAT eq $_[1] }
package Pugs::Runtime::Perl6::Str;
    use overload (
        '""'     => sub { ${$_[0]} },
        '0+'     => sub { ${$_[0]} },
        'bool'   => sub { ${$_[0]} },
        fallback => 1,
    );
    sub pos { pos( ${$_[0]} ) }
    sub WHAT { 
        'Str';  # XXX box
    }
    sub isa { $_[0]->WHAT eq $_[1] }
package Pugs::Runtime::Perl6::Int;
    use overload (
        '""'     => sub { ${$_[0]} },
        '0+'     => sub { ${$_[0]} },
        'bool'   => sub { ${$_[0]} },
        '++'     => sub { ${$_[0]}++ },
        'x='     => sub { "Assigning into a value is bogus" },
        fallback => 1,
    );
    sub WHAT { 
        'Int';  # XXX box
    }
    sub isa { $_[0]->WHAT eq $_[1] }
package Pugs::Runtime::Perl6::Num;
    use overload (
        '""'     => \&str,
        '0+'     => sub { ${$_[0]} },
        'bool'   => sub { ${$_[0]} },
        '++'     => sub { ${$_[0]}++ },
        fallback => 1,
    );
    sub WHAT { 
        'Num';  # XXX box
    }
    sub isa { $_[0]->WHAT eq $_[1] }
    sub str { 
        my $n = ${$_[0]}; 
        $n == 'Inf' 
        ? 'Inf'
        : $n == 'NaN'
        ? 'NaN'
        : $n
    }

1;

__END__

=pod

=head1 NAME 

Pugs::Runtime::Perl6

=head1 DESCRIPTION

Provides runtime routines for the Perl6-in-Perl5 compiled code

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
