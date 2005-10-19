#!/usr/bin/perl

use strict;
use warnings;

use lib 'lib', '../lib';

use Perl6::MetaModel;
use Perl6::MetaModel::Parser;

do $ARGV[1] if $ARGV[1];

sub draw {
    my ($class) = @_;
    my $output = "graph test {\n";
      
    $output .= _draw($::Object);    

    $output .= "}\n";
    return $output;
}

my %SEEN;

sub _draw_role_node {
    my ($node) = @_;
    return "node_" . $node->id . " [ " . 
                    "shape = box, " . 
                    "fontcolor = orange, " . 
                    "color = orange, " . 
                    "style = rounded, " .
                    "label = \"" . $node->name . "(" . $node->id . ")\" ". 
                    "];\n";
}

sub _draw_class_node {
    my ($node) = @_;
    return "node_" . $node->id . " [ " . 
                    "shape = box, " . 
                    ($node->id > 6 ? 
                        ($node->class == $::EigenClass ? 
                            "color = grey60, " .
                            "fontcolor = grey60, "
                            :
                            "color = green, " .
                            "fontcolor = green, "                                                          
                            )
                        :                        
                        '') .
                    "style = rounded, " . 
                    "label = \"" . $node->name . "(" . $node->id . ")\" " . 
                    "];\n";  
}

sub _draw {
    my ($class) = @_;
    my $output;
    if (not exists $SEEN{$class}) {
        $output .= _draw_class_node($class);
        $output .= "node_" . $class->class->id . " -- node_" . $class->id . " [ dir = back, style = dotted, color = red ];\n";
    }
    
    if ($class->can('subclasses')) { # roles don't have subclasses
        foreach my $subclass (@{$class->subclasses}) {
           $output .= _draw($subclass);     
        }
    }
    
    if ($class->can('superclasses')) { # roles don't have subclasses
        foreach my $superclass (@{$class->superclasses}) {
           $output .= "node_" . $superclass->id . " -- node_" . $class->id . " [ dir = back, style = dashed, color = blue ];\n";       
        }
    }    
    
    foreach my $role (@{$class->roles}) {
        $output .= _draw_role($role);
        $output .= "node_" . $role->id . " -- node_" . $class->id . " [ dir = back, style = solid, color = orange ];\n";       
    } 
    
    $SEEN{$class} = 1;       
    return $output;
}

sub _draw_role {
    my ($role) = @_;
    my $output = _draw_role_node($role);   
    foreach my $subrole (@{$role->roles}) {
        $output .= _draw_role($subrole);
        #$output .= "node_" . $subrole->id . " -- node_" . $role->id . " [ dir = back, style = solid, color = orange ];\n";       
    }       
    return $output;
}

open OUTFILE, ">", $ARGV[0];
print OUTFILE draw();
close OUTFILE;

__END__

=pod

  Class<..........<...........................
    ^            :                           :
    :            :                           :
CustomMeta  CustomMeta2                 CustomMeta3
    ^            ^                           ^
    :            :                           :
  eFoo<.........eBar<.........eBaz<.........eBlah
    ^            ^             ^             ^
    |            |             |             |
   Foo<.........Bar<..........Baz<..........Blah

=cut

my $CustomMeta  = class 'CustomMeta'  => { is => [ $::Class ] };
my $CustomMeta2 = class 'CustomMeta2' => { is => [ $::Class ] };
my $CustomMeta3 = class 'CustomMeta3' => { is => [ $::Class ] };
my $eFoo        = class 'eFoo'        => { is => [ $CustomMeta ] };
my $eBar        = class 'eBar'        => { is => [ $CustomMeta2, $eFoo ] };
my $eBaz        = class 'eBaz'        => { is => [ $eBar ] };
my $eBlah       = class 'eBlah'       => { is => [ $CustomMeta3, $eBaz ] };

my $Foo = $eFoo->new('$:name' => 'Foo');
my $Bar = $eBar->new('$:name' => 'Bar');
my $Baz = $eBaz->new('$:name' => 'Baz');
my $Blah = $eBlah->new('$:name' => 'Blah');

=pod

  Class<...............<............................        
    ^^.......         :                            :
    :       :         :                            :
CustomMeta  :    CustomMeta2                   CustomMeta3
    ^       :         ^                            ^
    :       :         :                            :
  xFoo...>eFoo       xBar....>eBar<... eBaz      xBlah...>eBlah
    ^       ^         ^        :        ^^         ^        :
    |       :.........|........:        |:.........|........:
    |                 |                 |          |
   Foo<..............Bar<..............Baz<.......Blah

=cut

my $CustomMeta  = class 'CustomMeta'  => { is => [ $::Class ] };
my $CustomMeta2 = class 'CustomMeta2' => { is => [ $::Class ] };
my $CustomMeta3 = class 'CustomMeta3' => { is => [ $::Class ] };
my $eFoo        = class 'eFoo'        => { is => [ $::Class ] };
my $xFoo        = class 'xFoo'        => { is => [ $CustomMeta, $eFoo ] };
my $eBar        = class 'eBar'        => { is => [ $eFoo ] };
my $xBar        = class 'xBar'        => { is => [ $CustomMeta2, $eBar ] };
my $eBaz        = class 'eBaz'        => { is => [ $eBar ] };
my $eBlah       = class 'eBlah'       => { is => [ $eBaz ] };
my $xBlah       = class 'xBlah'       => { is => [ $CustomMeta3, $eBlah ] };

my $Foo = $xFoo->new('$:name' => 'Foo');
my $Bar = $xBar->new('$:name' => 'Bar');
my $Baz = $eBaz->new('$:name' => 'Baz');
my $Blah = $xBlah->new('$:name' => 'Blah');

print join ", " => map { $_->name } $Foo->class->MRO();
print "\n";
print join ", " => map { $_->name } $Bar->class->MRO();
print "\n";
print join ", " => map { $_->name } $Baz->class->MRO();
print "\n";
print join ", " => map { $_->name } $Blah->class->MRO();
print "\n";


__END__

my $script = shift;

warn "loading $script";

do $script;

warn join "\n" => keys %{$::{'*'}->FETCH('{}')};