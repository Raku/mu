# Access to kp6 ast definition file.
package KP6_AST_Def;
use strict;
use warnings;

sub ast_def_filename {
    my $fn = __FILE__;
    $fn =~ s/\.p.$/\.data/;
    $fn;
}
sub ast_def_load {
    my($callback)=@_;
    my $fn = ast_def_filename();
    open(F,"<$fn") or die $!;
    my $def = join("",<F>); close(F);
    $def =~ s/^node(?=\()/$callback/mg if $callback;
    eval($def) or die $!;
}

our @nodes;
ast_def_load('def_node');

sub nodes { @nodes }
sub node_from_name {
    my($cls,$name)=@_;
    for my $node (@nodes) {
	return $node if($node->{name} eq $name);
    }
    return undef;
}

sub def_node {
    my($node_def)=@_;
    $node_def =~ s/^\s+//; $node_def =~ s/\s+$//;
    my($name,@field_defs) = split(/\s+/,$node_def);
    my @fields = map{KP6_AST_Def::NodeField->new($_)} @field_defs;
    my $node = KP6_AST_Def::Node->new($name,\@fields);
    push(@nodes,$node);
}

{ package KP6_AST_Def::NodeField;
    sub new {
        my($cls,$def)=@_;
	# Some fields have different types in has() than in method attribs.
	# The meaning is not yet clear to me.  They may even just be typos.
	# An extra sigil on the end of some of the field name definitions
	# records the type in attribs.
	$def =~ /^(([\$\@\%])\.(.+?))([\$\@\%])?$/ or die "invalid field def: '$def'";
	my($name,$sigil,$identifier,$attrib_sigil)=($1,$2,$3,$4);
	$attrib_sigil ||= $sigil;
	my $attrib_name = $attrib_sigil.'.'.$identifier;
	my $self = {
	    name => $name,
	    sigil => $sigil,
	    identifier => $identifier,
	    attrib_name => $attrib_name,
	    attrib_sigil => $attrib_sigil,
	};
	bless $self,$cls;
    }
    sub name { $_[0]{name} }
    sub identifier { $_[0]{identifier} }
    sub attrib_name { $_[0]{attrib_name} }
}

{ package KP6_AST_Def::Node;
    sub new {
	my($cls,$name,$fields)=@_;
	my $self = { name => $name, fields => $fields };
	bless $self,$cls;
    }
    sub name { $_[0]{name} }
    sub fields { @{$_[0]{fields}} }
}

1;
