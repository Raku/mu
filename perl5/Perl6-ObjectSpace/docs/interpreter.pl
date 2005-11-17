#!/usr/bin/perl

use strict;
use warnings;

use Perl6::Runtime;
use Perl6::Util::SExpParser;

=pod

NOTE:
This is a very basic first attempt at a primitive 
s-expr compiler to generate p5 code, it's really 
pretty bad actually, and the s-expr got quite messy
as well. Although that could be fixed with macros.

=cut

{
    package Interpreter;
    
    use Data::Dumper;
    
    my %__classes__ = map { $_ => undef } 
        qw/
        num bit str symbol nil type  
        list hash reference block 
        closure closure::env closure::signature closure::params
        opaque method attribute C3
        /;
    
    sub new { bless \(my $var) => shift }
    
    sub parse {
        my ($self, $source) = @_;
        my $sexpr = Perl6::Util::SExpParser->parse($source);
        #warn Dumper $sexpr;
        return $self->evaluate($sexpr);
    }
    
    my $depth = 0;
    
    sub evaluate {
        my ($self, $sexpr) = @_;
        
        $depth++;
        
        # flags
        my $in_string = 0;
        my $in_block = 0;
        
        my @t;
        my @buffer;
        
        my @x = @{$sexpr};
        foreach my $i (reverse(0 .. $#{$sexpr})) {
            my $t = $sexpr->[$i];
            if (ref($t) eq 'ARRAY') {          
                push @t => $self->evaluate($t);
                next;
            }         
            
            # deal with strings   
            if ($t eq '"') {
                if ($in_string) {
                    $in_string = 0;
                    push @t => "'" . (join ' ' => reverse @buffer) . "'";
                    @buffer = ();
                }
                else {
                    $in_string = 1;
                }
            }
            elsif ($in_string) {
                push @buffer => $t;          
            }
            
            # deal with blocks
            elsif ($t =~ /^\{|\}$/) {
                if ($in_block) {
                    $in_block = 0;
                    my @body;
                    foreach my $statement (@t) {
                        if (!@body) {
                            #warn "our first arg $arg";
                            push @body => $statement
                        }
                        elsif ($statement =~ /^\-\>/) {
                            #warn "hello there";
                            $body[-1] .= $statement;
                        }
                        else {
                            #warn "our other args $arg";                        
                            push @body => $statement;
                        }
                    }                    
                    
                    @t = 'sub { my $e = shift; ' . (join '; ' => reverse @body) . '; }';
                }
                else {
                    $in_block = 1;
                }
            }            
            elsif ($in_block) {
                push @buffer => $t;          
            }            
            
            # deal with numbers
            elsif ($t =~ /^\d+$/) {
                push @t => $t;
            }            
            
            # variables
            elsif ($t =~ /^\$|\%|\@|\&/) {
                if (@t) {
                    $t .= (pop @t);
                }
                push @t => $t;
            }       
            
            # types
            elsif ($t =~ /^\<.*\>$/) {
                push @t => 'q' . $t;
            }      
            
            # classes             
            elsif (exists $__classes__{$t}) {               
                my $message = pop @t;
                @t = ($t . $message);                
            }
            
            # misc keywords
            elsif ($t eq 'returns' ||
                   $t eq 'params'  ) {
                my $type = pop @t;
                @t = ($t . ' => ' . $type);                    
            }
            
            # if all else fails, they are messages
            else {
                #warn "compiling args for $t";
                my $args = '';
                foreach my $arg (@t) {
                    if (!$args) {
                        #warn "our first arg $arg";
                        $args .= $arg
                    }
                    elsif ($arg =~ /^\-\>/) {
                        #warn "hello there";
                        $args .= $arg;
                    }
                    else {
                        #warn "our other args $arg";                        
                        $args .= ', ' . $arg;
                    }
                }
                @t = ('->' . $t . "($args)");                
            }
        } 
        
        $depth--;
        
        reverse @t;    
    }
}

my $source = q|
($::ENV create 
    ("*::WALKMETH"
        (closure new 
            (($::ENV)
             (closure::signature new 
                 ((params  (closure::params new 
                                ((symbol new ("&dispatcher" <block> ))
                                 (symbol new ("$label"      <symbol>))
                                 (symbol new ("?%opts"      <hash>  )))))
                  (returns <method>)))
             {
                 ($e create ("$method"  ($nil::NIL)))
                 ($e create ("$current" ((($e get ("&dispatcher")) do))))
                 ((block new 
                    (($e) 
                     {
                         (((($e get ("$current")) send ("has_method" ($e get ("$label"))))) 
                            and (block new 
                                    (($e)
                                     {
                                         ($e set ("$method" ((($e get ("$current")) send ("get_method" ($e get ("$label"))))))) 
                                     })))
                        ($e set ("$current" ((($e get ("&dispatcher")) do))))
                     })) do_while 
                            (block new 
                                (($e) 
                                {
                                    (((($e get ("$current")) is_not_nil)) 
                                        and (block new 
                                                (($e) 
                                                {
                                                    (($e get ("$method")) is_nil)
                                                })))
                                }))) 
                    ($e get ("$method"))                        
             }))))
|;

=pod

($::ENV create 
    ("*::WALKMETH"
        (closure new 
            (($::ENV)
             (closure::signature new 
                 ((params  (closure::params new 
                                ((symbol new ("&dispatcher" <block> ))
                                 (symbol new ("$label"      <symbol>))
                                 (symbol new ("?%opts"      <hash>  )))))
                  (returns <method>)))
             {
                 ($e create ("$method"  ($nil::NIL)))
                 ($e create ("$current" ((($e get ("&dispatcher")) do))))
                 ((block new 
                    (($e) 
                     {
                         (((($e get ("$current")) send ("has_method" ($e get ("$label"))))) 
                            and (block new 
                                    (($e)
                                     {
                                         ($e set ("$method" ((($e get ("$current")) send ("get_method" ($e get ("$label"))))))) 
                                     })))
                        ($e set ("$current" ((($e get ("&dispatcher")) do))))
                     })) do_while 
                            (block new 
                                (($e) 
                                {
                                    (((($e get ("$current")) is_not_nil)) 
                                        and (block new 
                                                (($e) 
                                                {
                                                    (($e get ("$method")) is_nil)
                                                })))
                                }))) 
                    ($e get ("$method"))                        
             }))))

$::ENV->create(
    "*::WALKMETH", 
    closure->new(
        $::ENV, 
        closure::signature->new(
            params => closure::params->new(
                        symbol->new("&dispatcher", q<block>), 
                        symbol->new("$label", q<symbol>), 
                        symbol->new("?%opts", q<hash>)
                    ), 
            returns => q<method>
        ), 
        sub { 
            my $e = shift; 
            $e->create("$method", $nil::NIL); 
            $e->create("$current", $e->get("&dispatcher")->do()); 
            block->new($e, sub { 
                my $e = shift; 
                $e->get("$current")
                  ->send("has_method", $e->get("$label"))
                  ->and(
                      block->new($e, sub { 
                          my $e = shift; 
                          $e->set("$method", $e->get("$current")->send("get_method", $e->get("$label"))); 
                      })
                  ); 
                $e->set("$current", $e->get("&dispatcher")->do()); 
            })->do_while(
                block->new($e, sub { 
                    my $e = shift; 
                    $e->get("$current")
                      ->is_not_nil()
                      ->and(
                          block->new($e, sub { 
                              my $e = shift; 
                              $e->get("$method")->is_nil(); 
                          })
                      ); 
                })
            ); 
            $e->get("$method"); 
        }
    )
);


 
(($::ENV) create 
    ("*::WALKCLASS" 
        (closure new             
            (($::ENV)
             (closure::signature new 
                ((params  (closure::params new (symbol new ("&dispatcher" <block>))))
                 (returns <method>)))
            {  (($e get ("&dispatcher")) do) }))))


$::ENV->create(
    "*::WALKCLASS", 
    closure->new(
        $::ENV, 
        closure::signature->new(
            closure::params->new(
                symbol->new("&dispatcher", q<block>)
            ), 
            returns => q<method>
        ), 
        sub { 
            my $e = shift; 
            $e->get("&dispatcher")->do() 
        }
    )
);

=cut

my $i = Interpreter->new;
my $code = join "" => $i->parse($source), '';

warn "\n" . $code . "\n";

eval $code;
warn $@ if $@;

