package EvalbotExecuter;

=head1 NAME

EvalbotExecuter - Execution of external programs for evalbot

=head1 SYNOPSIS

    use EvalbotExecuter;

    sub my_evalbot_executer {
        my ($program, $fh, $filename) = @_;

        # execute $program, and write the result to 
        # $fh, which is a filehandle opened for reading. 
        # $filename is the name of that file.
        
        # we make a very stupid 'eval': remove all 
        # vowels, and write it:

        $program =~ s/[aeiou]//g;
        print $fh $program;
        close $fh;

        # the return value doesn't really matter
        return;
    }

    # somewhere else in the program, run it:
    EvalbotExecuter::run('say "foo"', \&my_evalbot_executer);
    
=head1 DESCRIPTION

EvalbotExecuter is basically a wrapper around a function that executes an
external program.

Currently it does the following:

=over

=item *

Set up a temporary file that should capture the output

=item *

Fork a child process

=item *

Set resource limits in the child process

=item *

call an external function that starts an external process

=item * 

collects the contents of the temporary file, postprocess it, and unlink 
the temp file.

=back

=cut


use strict;
use warnings;
use utf8;
use BSD::Resource;
use Carp qw(confess);
use File::Temp qw(tempfile);
use Encode qw(encode);

my $max_output_len = 290;

sub run {
    my ($program, $executer) = @_;
    my $response = _fork_and_eval($program, $executer);
    if (!length $response){
        $program = '( ( do { ' . $program . "\n} ).perl ).print";
        $response = _fork_and_eval($program, $executer);
        if (!length $response){
            $response = "No output (you need to produce output to STDOUT)";
        } else {
            $response = "RESULT[$response]";
        }
    } else {
        $response = "OUTPUT[$response]";
    }
    my $newline = '␤';
    $response =~ s/\n/$newline/g;
    if (length $response > $max_output_len){
        $response = substr $response, 0, $max_output_len - 3;
        $response .= '...';
    }
    return $response;
}

sub _fork_and_eval {
    my ($program, $executer) = @_;

# the forked process should write its output to this tempfile:
    my ($fh, $filename) = tempfile();

    my $fork_val = fork;
    if (!defined $fork_val){
        confess "Can't fork(): $!";
    } elsif ($fork_val == 0) {
        _set_resource_limits();
        &$executer($program, $fh, $filename);
        close $fh;
        exit;
    } else {
# server
        wait;
    }

# gather result
    close $fh;
    open ($fh, '<', $filename) or confess "Can't open temp file <$filename>: $!";
    my $result;
    {
        local $/;
        $result = join '', <$fh>;
    }
    unlink $filename;
    return $result;
}

sub _set_resource_limits {
# stolen from evalhelper-p5.pl
# 5s-7s CPU time, 100 MiB RAM, maximum of 500 bytes output.
    setrlimit RLIMIT_CPU,   15, 20                   or confess "Couldn't setrlimit: $!\n";
    setrlimit RLIMIT_VMEM,  180 * 2**20, 200 * 2**20 or confess "Couldn't setrlimit: $!\n";
# PIL2JS writes to a tempfile.
    setrlimit RLIMIT_FSIZE, 50000, 50000,            or confess "Couldn't setrlimit: $!\n";
}

1;
# vim: sw=4 ts=4 expandtab syn=perl
