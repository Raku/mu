# WV 06/05/2007
package Repl;
use Moose;
no warnings;

has 'subref' => (
	is => 'rw', 
	required => 1, 
);

has 'prompt' => (is => 'ro', isa => 'Str', default => '');
has 'motd' => (is => 'ro', isa => 'Str', default => '');

sub run {
	my $s=shift;
	print $s->motd,"\n",$s->prompt;
	while (<STDIN>) {		
		/\:q(uit)*$/i && exit();
		chomp $_;
		my $res=$s->subref->($_);
		print $res,"\n",$s->prompt;
	}
}

1;
