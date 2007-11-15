
use lib 'lib';
use Language::Toto::Grammar;
use Template;
use strict;
use Data::Dumper;

my $toto = do { local $/; <> };
my $template_src = do { local $/; open FILE, 'xpdl.tt' or die 'argh'; <FILE> };

my $m = Language::Toto::Grammar->TOP( $toto );

my %Workflow;

$Workflow{processes} = [];

my @blocks = @{ $m->{block} };
# print scalar(@blocks), " blocks \n";

for ( @blocks ) {
    push @{ $Workflow{processes} }, extract_block($_);
}

# print Dumper( \%Workflow );

my $template = Template->new;
$template->process(\$template_src, \%Workflow)
    or die $template->error;



sub extract_block {
    my $m = shift;
    my %block;
    my @arrows;
    
    my $type = ${ $m->{block_header}{block_type} };
    # print "BLOCK type = $type \n";
    my $ident = ${ $m->{block_header}{ident} };
    # print " ident = $ident \n";
    my $name = join( " ", @{ $m->{block_name}{text} } );
    # print " name = $name \n";
    my $description = join( "\n", @{ $m->{block_description}{text} } );
    # print " description = $description \n";
    my @transition = @{ $m->{block_transition} };
    # print " ",scalar( @transition ), " transitions \n";
    my @activities;
    push @activities, {
        id => $ident,
    };
    for ( @transition ) {
        my $t = extract_transition($_);
        push @activities, $t;
        if ( $t->{type} eq 'New' ) {
            push @arrows, { 
                to => $ident, 
                from => $t->{id},
                id => $t->{id} . "_to_" . ${ident},
            };
        }
        else {
            push @arrows, { 
                from => $ident, 
                to => $t->{id},
                id => "${ident}_to_$t->{id}",
            };
        }
    }

    #print Dumper( @arrows );

    return {
        id => $ident,
        name => $name,
        activities => \@activities,
        transitions => \@arrows,
    };
}

sub extract_transition {
    my $m = shift;
    my @activities;
    
    my $type = ${ $m->{type} };
    # print " TRANSITION type = $type \n";
    if ( exists $m->{plugin} ) {
        # print "  PLUGIN\n";
        my $plugin = ${ $m->{plugin} };
        # print "  plugin = $plugin \n";
        # print "\n";
    
        return {
            id => $plugin,
        };
    }
    elsif ( exists $m->{action} ) {
        my $action = ${ $m->{action} };
        # print "  action = $action \n";
        my $ident = ${ $m->{ident} };
        # print "  ident = $ident \n";
        # print "\n";
    
        return {
            id => $ident,
            type => $type,   # unused in xpdl
        };
    }
}

