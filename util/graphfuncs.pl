#!env perl -w
use strict;
#use GraphViz;
use Getopt::Long;

GetOptions \our %Conf, qw(--verbose|v no|n leaves|l cc|c=s@ allccs|a);

my $signature = qr/^([\w']*) ::/o;
my $fn;
my %funcs;
#my $g = GraphViz->new();

while (<>) {
	if($_ =~ $signature) { $fn = $1; next }
	#if($_ =~ $signature) { $fn = $1; $g->add_node($fn); next }
	if($fn) { $funcs{$fn} .= $_ }
}

our %nodes;
for my $key (keys %funcs) {
	for my $func (keys %funcs) {
		next if $func eq $key;
		print "$key -> $func\n" if $Conf{verbose} && $funcs{$key} =~ /\b$func\b/;
        push @{ $nodes{$key} }, $func if $funcs{$key} =~ /\b$func\b/;
#		$g->add_edge($key, $func) if($funcs{$key} =~ $func)
	}
}

if ($Conf{leaves}) {
    print "$_\n" for grep { not defined $nodes{$_} } keys %funcs;
}

if ($Conf{cc}) {
    my %ccset = map { $_ => 1 } @{ $Conf{cc} };
    comp_cc(\%ccset);
    print "$_\n" for sort keys %ccset;
}

if ($Conf{allccs}) {
    $|++ if $Conf{verbose};
    my %allccs;
    for my $f (keys %nodes) {
        print "." if $Conf{verbose};
        my %cc = ($f => 1);
        $allccs{$f} = [ keys %{comp_cc(\%cc)} ];
    }
    print ::Y(\%allccs),"\n";
}
#print $g->as_png unless $Conf{no};

sub comp_cc {
    my ($ccset) = @_;
    my $oldn = 0;
    #::YY(\%nodes);
    while ($oldn != keys %$ccset) {
        $oldn = keys %$ccset;
        for my $f (keys %$ccset) {
            $ccset->{$_} = 1 for @{ $nodes{$f} };
        }
    }
    $ccset;
}

sub ::Y { require YAML; YAML::Dump(@_) }
sub ::YY { require Carp; Carp::confess(::Y(@_)) }
