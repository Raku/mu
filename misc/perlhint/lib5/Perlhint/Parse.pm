package Perlhint::Parse;

use Carp;
use Storable qw(dclone);
use strict; 
use warnings;

sub new {
    my ($class, $conf_ref) = @_;
    my $self = {};
    bless $self, $class;
    $self->{filename} = $conf_ref->{filename} 
        || confess("Usage: " . __PACKAGE__ . "->new( {filename => \$fn} );\n");
    $self->_parse();

    return $self;
}

sub _parse {
    my $self = $_[0];
    my $fn = $self->{filename};
    open (my $fh, '<:utf8', $fn) or die "Can't open file '$fn' for reading: $!";
    my @records;
    my $previous_key = qq{};
    my %current_record;

    INPUT:
    while (my $line = <$fh>){
        chomp $line;
        next INPUT if $line =~ m{^ [#]}msx;
        if (length($line)== 0){
            $previous_key = qq{};
            next INPUT;
        }

        if ($line =~ m{^\s}msx){
            # continuation of previous line
            if (!$previous_key){
                confess("Parse error in file '$fn', line $.: \n" 
                        . "\t Line begins with whitespaces but does not continue a previous value\n"); 

            }
            else {
                $line =~ s{^ \s+}{}msx;
                $current_record{$previous_key} .= $line . "\n";
            }
                
        }
        else {
            # ordinary record
            $line =~ m{^ 
                    (\w+):  # key
                    \s* 
                    (.*)    # value
                    $}xms || confess("Parse error in file '$fn', line $.:\n"
                            . "\tLine not in format 'key: value'");
            my ($key, $value) = ($1, $2);
            if ($value =~ m{\\$}xms) {
                $value =~ s{ \\ $}{}xms;
            } else {
#                warn "Preserving line endings";
                $value .= "\n";
            }
            if ($key eq "key"){
                if (%current_record){
                    for (keys %current_record){
                        chomp $current_record{$_};
                    }
                    push @records, dclone(\%current_record);
                    %current_record = ();
                }
            }
            $current_record{$key} = $value;
            $previous_key = $key;
        }
    }
    if (%current_record){
        for (keys %current_record){
            chomp $current_record{$_};
        }
        push @records, \%current_record;
    }
    close $fh;

    $self->{records} = \@records;
}

sub records {
    return shift->{records};
}

1;
# vim: sw=4 ts=4 expandtab
