
package Perl6::MetaModel::Parser;

use strict;
use warnings;

our $VERSION = '0.01';

use Perl6::MetaModel;

use Perl6::MetaModel::Parser::Env;
use Perl6::MetaModel::Parser::Tokenizer;

sub new {
    my $class = shift;
    my $self = bless {
        tokenizer => Perl6::MetaModel::Parser::Tokenizer->new,
        env       => Perl6::MetaModel::Parser::Env->new,
        code      => undef
    } => $class;
    return $self;
}

sub tokenizer { (shift)->{tokenizer} }
sub env       { (shift)->{env}       }

our $DEBUG = 0;

sub parse {
    my ($self, $source) = @_;
    $self->tokenizer->tokenize($source);
    my $i = $self->tokenizer->iterator;
    my $in_method = 0;
    while ($i->hasNextToken) {
        my $token = $i->nextToken;
        if ($token eq 'class') {
            $self->_close_current_class;            
            my $class = $self->_parse_class($i);
            $self->env->set('current_class' => $class);            
        }
        elsif ($token eq '{') {
            $self->env->enter_scope;
        }
        elsif ($token eq '}') {
            $self->_close_current_class unless $in_method;            
            $self->env->leave_scope;
        }
        elsif ($token eq 'has') {
            my $current_class = $self->env->get('current_class');
            $self->_discard_whitespace($i);
            my $attr_name = $i->nextToken;
            $current_class->add_attribute($attr_name => ::make_attribute($attr_name));
        }  
        elsif ($token eq 'is') {
            my $current_class = $self->env->get('current_class');
            $self->_discard_whitespace($i);
            my $superclass = $i->nextToken;
            $current_class->superclasses([ 
                @{$current_class->superclasses}, 
                $::{'*'}->FETCH($superclass) 
            ]);
        }  
        elsif ($token eq 'method') {
            my $current_class = $self->env->get('current_class');
            $self->_discard_whitespace($i);
            my $method_name = $i->nextToken;
            my ($type, $method) = $self->_parse_method($i);
            if ($type eq 'class') {
                $current_class->add_singleton_method($method_name, $method);               
            }
            else {
                $current_class->add_method($method_name, $method);
            }
        }                             
        else {
            $self->env->set(
                'acc' => ($self->env->get('acc') || '') . $token
            ) if $in_method;
        }
    }
}

## private methods

sub _discard_whitespace {
    my ($self, $i) = @_;
    while ($i->lookAheadToken =~ /\s+/) {
        $i->nextToken;
    }
}

sub _parse_class {
    my ($self, $i) = @_;
    warn "found a class ..." if $DEBUG;
    my $class = $::Class->new();
    $self->_discard_whitespace($i);
    $class->name($i->nextToken);
    $self->_discard_whitespace($i);
    if ($i->lookAheadToken eq 'is') {
        $self->_parse_superclasses($i, $class);
    }
    warn "the class  is named " . $class->name if $DEBUG;  
    return $class;  
}

sub _close_current_class {
    my $self = shift;
    if (my $current_class = $self->env->get('current_class')) {
        unless (@{$current_class->superclasses}) {
            $current_class->superclasses([ $::Object ]);
        }
        $::{'*'}->STORE($current_class->name => $current_class);
        $self->env->set('current_class' => undef);         
    }      
}

sub _parse_superclasses {
    my ($self, $i, $class) = @_;
    my @supers;
    do {
        $i->nextToken; # remove the is
        $self->_discard_whitespace($i);
        push @supers => $i->nextToken;
        $self->_discard_whitespace($i);
    } while $i->lookAheadToken eq 'is';
    
    warn "got the list of supers :: [" . (join "], [" => @supers) . "]" if $DEBUG;
    $class->superclasses([ map { $::{'*'}->FETCH($_) } @supers ]) if @supers;
}

sub _parse_method {
    my ($self, $i) = @_;
    my ($method_type, @params) = ('', ());
    $self->_discard_whitespace($i);    
    if ($i->lookAheadToken() eq '(') {
        ($method_type, @params) = $self->_parse_parameters($i);
    }
    $i->skipTokensUntil('{');
    $i->nextToken; # now discard the {
    my $method_source = "sub {\n";
    if (@params) {
        $method_source .= 'my (' . (join ", " => @params) . ') = @_;' . "\n";
    }
    my $curly_count = 1;
    while ($i->hasNextToken) {
        my $next = $i->nextToken;    
        if ($next eq '{') {
            $curly_count++; 
            $method_source .= $next;
        }
        elsif ($next eq '}') {
            $curly_count--;
            last if $curly_count == 0; 
            $method_source .= $next;            
        }    
        else {
            $method_source .= $next;
        }
    }
    $method_source .= "\n}";
    warn "pre: " . $method_source if $DEBUG;
    $method_source = $self->_post_process_method($method_source);
    warn "post: " . $method_source if $DEBUG;    
    my $method = eval $method_source;
    die "method eval failed : $@" if $@;
    return ($method_type, ::make_method($method));
}

sub _parse_parameters {
    my ($self, $i) = @_;
    $i->nextToken; # discard the (
    my ($method_type, @params);
    my @tokens = $i->collectTokensUntil(')');
    if ($tokens[0] eq 'Class') {
        $method_type = 'class';
        shift @tokens;
    }
    elsif ($tokens[0] eq $self->env->get('current_class')->name) {
        $method_type = 'instance';
        shift @tokens;
    }
    foreach my $token (@tokens) {
        next if $token =~ /\s+/;
        if ($token =~ /^(.*)\:$/) { # an invocant
            push @params => $1;
        }
        else {
            push @params => $token;
        }        
    }
    warn "method type: $method_type => params: [" . (join ", " => @params) . ']' if $DEBUG;
    return ($method_type, @params);
}

sub _post_process_method {
    my ($self, $method_source) = @_;
    
    $method_source =~ s/(\w+)\./$1\-\>/g;
    $method_source =~ s/\~/\./g;    
    
    # scalar access
    $method_source =~ s/(\$[.:]\w+)/\:\:opaque_instance_attr\(\$\:\:SELF, \'$1\'\)/g;
    
    # array access
    $method_source =~ s/(\@[.:]\w+)/\@\{\:\:opaque_instance_attr\(\$\:\:SELF, \'$1\'\)\}/g;
    
    # hash followed by a key access
    $method_source =~ s/(\%[.:]\w+)\{/\:\:opaque_instance_attr\(\$\:\:SELF, \'$1\'\)\-\>\{/g;  
    # hash followed by whitespace or semi-colon (EOL) 
    $method_source =~ s/(\%[.:]\w+)(\s|\;)/\%\{\:\:opaque_instance_attr\(\$\:\:SELF, \'$1\'\)\}/g;            
    
    $method_source =~ s/\$\?SELF/\$\:\:SELF/g;
    $method_source =~ s/\$\?CLASS/\$\:\:CLASS/g;   
    
    $method_source =~ s/next METHOD/\:\:next_METHOD\(\)/g;            
    
    return $method_source;
}

1;