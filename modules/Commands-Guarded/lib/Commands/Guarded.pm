module Commands::Guarded-0.0.1;
use v6;

sub step(*@args) is export(:DEFAULT) {
  my Commands::Guarded $step .= new(*@args);

  given want {
    when Void { $step.do }
    default   { $step    }
  };
}

# Verbosity on (or off); defaults to env variable or 0
my $verbose = exists $*ENV<GUARDED_VERBOSE> ?? $*ENV<GUARDED_VERBOSE> :: 0;
sub verbose(Bool $v) is rw is export(:other) { $verbose }

class Commands::Guarded {
  has Str $.name;
  has Code ($.ensure, $.using, $.rollback, $.sanity);

  submethod BUILD(
    Str $.name,
    Code $.ensure, Code $.using, Code ?$.rollback, Code ?$.sanity,
  ) {}

  method :diag(Str $msg) {
    say $*ERR: $msg if verbose;
  }

  # Rollback handlers
  our @rollbacks;

  method :register_rollback(*@args) {
    if $.rollback {
      push @rollbacks, [$.rollback => \@args];
    }
  }

  sub clear_rollbacks() is export(:other) {
    @rollbacks = ();
  }

  method :do_rollbacks() {
    while pop @rollbacks {
      my $sub  = $^rollback.key;
      my @args = $^rollback.value;
      $sub.(*@args);
    }
  }

  method :fail(Str $reason) { 
    .:do_rollbacks;
    die $reason;
  }

  method :check_sanity(*@args) {
    if $.sanity {
      $.sanity(*@args) or
	.:fail("Sanity check for {$.name}(@args) failed\n");
     }
  }

  method :do_pre_using(*@args) {
     .:check_sanity     .(*@args);
     .:register_rollback.(*@args);
     return $.ensure    .(*@args);
  }

  method do(*@args) {
    .:diag("Step {$.name}(@args)... ");

    unless .:do_pre_using(*@args) {
      my $res = $.using(*@args);
      .:check_sanity(@*args);

      if $.ensure(*@args) {
        .:diag("succeeded\n");
	return $res;
      } else {
	.:fail("failed\n")
      }
    }

    .:diag("skipped\n");
    return;
  }

  method do_foreach(*@args) {
    my @usings;

    for @args -> $arg {
      unless .:do_pre_using($arg) {
        push @usings, $arg;
      } else {
        .:diag("Step {$.name}(@args)... skipped\n");
      }
    }

    for @usings -> $arg {
      .:diag("Step {$.name}(@args)... ");
      $.using($arg);
      .:check_sanity($arg);

      if $.ensure($arg) {
        .:diag("succeeded\n");
      } else {
        .:diag("failed\n");
      }
    }

    return;
  }
}

1;

__END__
=head1 NAME

Commands::Guarded - Better scripts through guarded commands

=head1 SYNOPSIS

  use Commands::Guarded;
  
  my $var = 0;
  
  step
    name   => "something",
    ensure => { $var == 1 },
    using  => { $var = 1 };
  # $var is now 1
  
  step
    name   => "nothing",
    ensure => { $var == 1 },
    using  => { $var = 2 }; # bug!
  # $var is still 1 (good thing too), because the using
  # block wasn't ever called, because the result of the
  # ensure block was already true!
  
  my $brokeUnless5 = step
    name   => "brokenUnless5",
    ensure => { $var == 5 },
    using  => { $var = shift };
  # nothing happens yet
  
  say "var: $var"; # prints 1
  $brokeUnless5.do(5);
  say "now var: $var"; # prints 5
  
  step
    name   => "fail",
    ensure => { $var == 3 },
    using  => { $var = 2 };
  # Exception thrown here

=head1 DESCRIPTION

This module implements a deterministic, rectifying variant on
Dijkstra's guarded commands.  Each named step is passed two blocks: an
C<ensure> block that defines a test for a necessary and sufficient
condition of the step, and a C<using> block that will cause that
condition to obtain.

If C<step> is called in void context (i.e., is not assigned to
anything or used as a value), the step is run immediately, as in this
pseudocode:

  unless (ENSURE) {
    USING;
    die unless ENSURE;
  }

If C<step> is called in scalar or array context, execution is deferred
and instead a Commands::Guarded object is returned, which can be
executed as above using the C<do> method.  If C<do> is given
arguments, they will be passed to the C<ensure> block and (if
necessary) the C<using> block.

The interface to Commands::Guarded is thus a hybrid of exported
subroutines (see B<SUBROUTINES> below) and non-exported methods (see
B<METHODS>).

For a detailed discussion of the reason for this module's existence,
see B<RATIONALE> below.

=head1 SUBROUTINES

=over

=item step ...

Defines a new guarded command step.  If called in void context, the step
is executed immediately.  If called in scalar or array context (i.e.,
in an expression or assignment), a Commands::Guarded object is
returned (see B<METHODS> below).

NAME is a string that will be printed on failure (also see C<verbose>
below).

EXPR is one or more Commands::Guarded blocks (see B<BLOCKS> below).
Typically at least a C<ensure> and C<using> block will be included.

Note that because C<step> is a subroutine and not a control structure
(though it acts like one in void context), it typically must be
followed by a semicolon.  It's recommended therefore to use the style

  step
    name   => "name",
    ensure => { ... },
    using  => { ... };

so as not to forget it.

=item verbose = SCALAR

(Not exported by default.)  If true, will print output not only on
failure of a step, but also at the beginning of a step (i.e., after
the C<ensure> block is first run) indicating whether the condition
failed ("Doing I<step name>") or succeeded ("Skipping I<step name>").
Also prints a message ("Step I<step name> succeeded") if the C<ensure>
condition now obtains after running C<using>.

Whether or not C<verbose> is set, an exception will be thrown if the
condition fails to obtain after running C<using>, with the message
"Step I<step name> failed at line...".

Besides using this subroutine, the environment variable
I<GUARDED_VERBOSE> can also be used to control this behavior without
modifying the code.  I<GUARDED_VERBOSE> will set the I<default>
behavior of C<verbose>; when set to a true value, the script will run
as if a C<verbose(1)> were specified at the beginning.  (A
C<verbose(0)> will always disable verbosity, no matter the value of
I<GUARDED_VERBOSE>.)

=item clear_rollbacks

(Not exported by default.)  Clears rollbacks.  See C<rollback> in the
section B<BLOCKS> below.

=back

=head1 BLOCKS

=over

=item ensure block

Defines a test for the step.  Should return true if the condition of
the test has been met, false otherwise.  It's common to write ensure
blocks as a chain of boolean expressions:

  ensure => { -d "$*ENV{HOME}" and fgrep qr/^$userid:/, '/etc/passwd' }

but it is also possible to use C<return> for more complicated tests:

  ensure => {
    for @dirs -> $dir {
      return 0 unless -d $dir;
    }
    return 1
  }

A true return from C<ensure> will cause the script to continue
execution.  A false return can have two possible effects: it will run
the step's C<using> block, or, if the C<using> block has already been
run, it will throw an exception.

=item using block

Defines the code to affect the condition in C<ensure>.  If the containing
step's C<ensure> block returns a false value, the block will be run.

=item sanity block

Defines a sanity check for a step.  Like C<ensure>, the block should
define a condition.  The condition is checked at the beginning of the
enclosing step (prior to C<ensure>), and again after running the
C<using> block (if the C<using> block is run, of course).  If it
returns a false value, an exception is thrown with the message "Sanity
check for I<step name> failed".

Note given this behavior that a sanity check should specify an
I<invariant> condition, i.e. something you expect to be true whether
or not the step has run with success or failure.  For example:

  step
    name   => "removeScratch",
    ensure => { not fgrep qr|^\S*\s+/scratch|, '/etc/fstab' },
    using  => {...},
    sanity => { # Don't lose boot partition!
      fgrep qr|^\S*\s+/boot\s|, '/etc/fstab'
    };

=item rollback block

Defines a rollback action for the step.  If this step, I<or any
following step>, fails (either through C<ensure> verification or
C<sanity> check failure), the rollback will be run.  If multiple
rollbacks are defined, they will be run in LIFO (Last-In, First-Out)
order.

B<Warning>: if an exception (C<die> or C<croak>) is thrown in your
rollback, the script will stop and other rollbacks will not be called.
If you truly intend to abort all previously set rollbacks, you should
use C<clear_rollbacks>.  You can (and probably should in most cases)
call C<clear_rollbacks> itself from within a C<rollback> block:

  step
    name     => "clearRollbacks",
    ensure   => {...},
    using    => {...},
    rollback => { 
      clear_rollbacks;
      ...;
    };

=back

=head1 METHODS

=over

=item .do

=item .do(ARGS)

Executes a step, possibly with arguments.  If arguments are supplied,
they will be passed to every block within the step.  Note that the
arguments are read-only within the block (i.e., attempting to modify
an element of @_ will throw an exception), though you can use
C<shift>, etc.

Some attempt is made to deal with return values, so you can get
something approximating a reasonable result from C<do> when the
C<using> block has executed.  But the author has not found a
real-world need for return values, so their behavior is not very
well-defined.  (Feel free to contact him if you believe you have a
solution.)

=item .do_foreach(LIST)

For each item of LIST, check C<ensure>, passing the item as an
argument.  After all C<ensure>s have been run, run C<using> with those
arguments whose C<ensure> failed.  Return values are not supported.
At present, multiple arguments for each call are not supported, either
(though you can certainly simulate that using a list-of-lists, if you
write your blocks to take an arrayref).

=back

=head1 RATIONALE

People often intuitively refer to some sorts of executables as
"scripts" and others as "programs."  When pressed for a definition,
they will often fall back on language-specific criteria (such as
whether the program is compiled or interpreted) that really do not
capture the essence of the difference between scripting and more
general-purpose programming.

A I<script> generally differs from other programs in the following ways
(there are exceptions):

=over

=item 1.

It makes heavy use of the external environment in which it
runs

=item 2.

It exports no complex data structures (though it may use them)

=item 3.

It has no outer event loop and does not daemonize (a simple
interactive prompt loop does not count)

=item 4.

It is usually run by the author, the author's agent (I<cron>,
etc.), or by a system administrator, rather than by the anonymous
"user"

=item 5.

It has as its primary purpose ensuring that some desired state
obtains in the system on which it runs (with "system" being defined as
broadly as necessary).

=back

Much has been written on good programming methodology, but in general
such methodologies have general-purpose programs in mind.  When
applied to scripts, which are generally very high-level and procedural
in nature, the methodologies can rapidly result in unreadable
spaghetti, with more code devoted to methodology than to method.

Most scripters react in one of two ways: they either let the spaghetti
ensue, or they throw up their hands and write fragile code.

=head2 An example

Suppose you want to write a script to mount a scratch directory from
an NFS server.  (This would usually be accomplished via a shell
language such as I<bash>, but for the sake of argument let's suppose
that you're writing in Perl, because you need access to another module
or perhaps just because you like Perl better.)

An optimistic implementation on a Red Hat Linux machine might be:

  # Add mount to filesystem table
  my $FSTAB = open ">>/etc/fstab";
  say $FSTAB: "$source:$scratch /net/$source/$scratch nfs $mount_opts";
  close $FSTAB;
  # Create mountpoint
  mkdir $scratch;
  # Symlink to /scratch
  symlink "/net/$source/$scratch", '/scratch';
  # Start NFS services automatically at boot
  system "/sbin/chkconfig --level 3 portmap on";
  system "/sbin/chkconfig --level 3 nfslock on";
  # Start NFS services
  system "/sbin/service portmap start";
  system "/sbin/service nfslock start";
  # Mount at boot time
  system "/sbin/chkconfig --level 3 netfs on";
  # Mount now
  system "/sbin/service netfs start";

With no error-checking at all, this script would blindly charge on
oblivious to any problems.  If anything at all went wrong, the user
would be left to pick up the pieces afterwards.  Running the script a
second time could be perilous, as the print statement would continue
to append to I</etc/fstab> even if it had previously succeeded.

Good scripters will check for errors.  The most common response to
such errors is to abort:

  # Add mount to filesystem table
  my $FSTAB = open ">>/etc/fstab"
    or die "Can't open fstab for appending: $!\n";
  say $FSTAB: "$source:$scratch /net/$source/$scratch nfs $mount_opts";
  close $FSTAB;
  # Create mountpoint
  mkdir $scratch
    or die "Can't create directory $scratch: $!\n";
  # Symlink to /scratch
  symlink "/net/$source/$scratch", '/scratch'
    or die "Can't make symlink to /scratch: $!\n";
  # Start NFS services automatically at boot
  system "/sbin/chkconfig --level 3 portmap on";
  if $? {
    die "Couldn't chkconfig on portmap\n";
  }
  system "/sbin/chkconfig --level 3 nfslock on";
  if $? {
    die "Couldn't chkconfig on nfslock\n";
  }
  # Start NFS services
  system "/sbin/service portmap start";
  if $? {
    die "Couldn't start portmap\n";
  }
  system "/sbin/service nfslock start";
  if $? {
    die "Couldn't start nfslock\n";
  }
  # Mount at boot time
  system "/sbin/chkconfig --level 3 netfs on";
  if $? {
    die "Couldn't start nfslock\n";
  }
  # Mount now
  system "/sbin/service netfs start";
  if $? {
    die "Couldn't start netfs\n";
  }

This implementation is certainly less likely to cause weird results,
but it is by no means perfect.  There are now nine places where the
script may abnormally terminate, leaving the task incomplete and the
user still to pick up the pieces.  If the script aborts early, the
user may choose to try to fix the problem encountered and then
manually revert to the initial state so that the script can be
re-executed.  

But if the user misses any of the steps (say, deleting the line in
I</etc/fstab>), the script will blithely carry on, unaware that some
steps of the task are already done.  (Worse yet, the first response of
many users to an unexpected error message is simply to try the command
again.)

If the script aborts late in the process, the user may try to fix the
encountered problem and then finish the task manually.  This too, is
fraught with peril--and the entire point of automating the task was to
reduce the chance of operator error!

One last observation about this new script--the functional code of the
script has now been largely obscured by the error-checking code.  In a
larger, more complicated script, the code could rapidly degenerate into
an unreadable mass.

Judicious use of a subroutine to factor out some of the error-checking
improves readability somewhat:

  sub doOrDie(*@args) {
    system @args;
    if $? {
      die "Couldn't @args\n";
    }
  }
  # Add mount to filesystem table
  my $FSTAB = open ">>/etc/fstab"
    or die "Can't open fstab for appending: $!\n";
  say $FSTAB: "$source:$scratch /net/$source/$scratch nfs $mount_opts";
  close $FSTAB;
  # Create mountpoint
  mkdir $scratch
    or die "Can't create directory $scratch: $!\n";
  # Symlink to /scratch
  symlink "/net/$source/$scratch", '/scratch'
    or die "Can't make symlink to /scratch: $!\n";
  # Start NFS services automatically at boot
  doOrDie "/sbin/chkconfig --level 3 portmap on";
  doOrDie "/sbin/chkconfig --level 3 nfslock on";
  # Start NFS services
  doOrDie "/sbin/service portmap start";
  doOrDie "/sbin/service nfslock start";
  # Mount at boot time
  doOrDie "/sbin/chkconfig --level 3 netfs on";
  # Mount now
  doOrDie "/sbin/service netfs start";

But suppose the system already had a preexisting mountpoint or
symlink?  This hardly seems like good reason for the script to
entirely fail.  The problem is that naive error-checking as above is
I<syntactic> in basis--a result of conditions intrinsic to the
implementation of the script--rather than being I<semantic>--i.e.,
relating to the state the script is trying to bring about.

=head2 Guarded commands to the rescue

These observations have resulted in the development of this module.
Using guarded commands, the script can be written more resiliently,
more clearly, and in many cases, more easily.  

The first step in writing a script using guarded commands is to
decompose the actions desired into a set of procedures, or I<steps>.
The above script can be so decomposed by observing the comments
marking each action of the script:

  # Add mount to filesystem table
  # Create mountpoint
  # Symlink to /scratch
  # Start NFS services automatically at boot
  # Start NFS services
  # Mount at boot time
  # Mount now

These are the script's steps.  (In this script, like many in system
automation programming, the steps are strictly linear, with each
dependent on one or more steps prior.  Some scripts will have more
complicated dependencies, loops, conditionals and the like.)  For each
step, one needs to define two things:

=over

=item 1.

A I<necessary and sufficient> condition to judge whether the step has
been completed.

=item 2.

Code that will cause that condition to come into being.

=back

To take the first step, "add mount to filesystem table," a necessary
and sufficient condition can be expressed as

  `cat /etc/fstab` =~ m|^$source:$scratch\s+/net/$source/$scratch|

Note first that this check is I<semantic> in nature.  The code above
would have created exactly one space between the two fields, but the
regex allows for any amount of whitespace.  One might be tempted to
write the condition as

  `cat /etc/fstab` eq "$source:$scratch /net/$source/$scratch nfs $mount_opts\n"

since that is the text that the script will be writing out.  But the
script will be more resilient with the first condition, because it
expresses exactly what later steps in the script I<need>, no 
more, no less: that I</etc/fstab> contain an entry that will cause the
desired filesystem to be mounted in the desired place via NFS.
If conditions change--for example, a new machine is preconfigured with
a suitable I<fstab> entry--the script will continue to function.

Having written the condition for the step--expressed in an C<ensure>
block--the scripter then turns to how to bring the condition about.
In this case, the code can be written

  my $fstab = open ">>/etc/fstab";
  say $fstab: "$source:$scratch /net/$source/$scratch nfs $mount_opts";

Note that we do not check the return value of C<open>.  There is no
need.  If we fail to open I</etc/fstab>, the C<print> will fail.  If
the C<print> fails, there will be no I<fstab> entry corresponding to
the regex above, and the script will fail for want of having obtained
the condition.  It may seem wrong at first--even blasphemous!--to
willfully ignore the return value of a call like C<open>.  This is the
first Lesson:

=over

=item B<Lesson 1.>

Trust your conditions, and the rest will follow.

=back

The entire script, rewritten with guarded commands, looks like this:

  use Commands::Guarded <:default>;
  use System::Utils     <fgrep appendf>;
  
  step
    name   => "Add mount to filesystem table",
    ensure => {
      fgrep qr|^$source:$scratch\s+/net/$source/$scratch|, 
	"/etc/fstab"
    },
    using  => {
      my $fstab = appendf '/etc/fstab';
      say $fstab:
        "$source:$scratch /net/$source/$scratch nfs $mount_opts";
    };
  step
    name   => "Create mountpoint",
    ensure => { -d $scratch },
    using  => { mkdir $scratch };
  step
    name   => "Symlink to /scratch",
    ensure => { readlink '/scratch' eq "/net/$source/$scratch" },
    using  => { symlink "/net/$source/$scratch", '/scratch' };
  step
    name   => "Start NFS services automatically at boot",
    ensure => {
      fgrep qr/3:on/, '/sbin/chkconfig --list portmap|' and
      fgrep qr/3:on/, '/sbin/chkconfig --list nfslock|';
    },
    using  => {
      system "/sbin/chkconfig --level 3 portmap on";
      system "/sbin/chkconfig --level 3 nfslock on";
    };
  step
    name   => "Start NFS services",
    ensure => {
      fgrep qr/running/, "/sbin/service portmap status|" and
      fgrep qr/running/, "/sbin/service nfslock status|";
    },
    using  => {
      system "/sbin/service portmap start";
      system "/sbin/service nfslock start";
    };
  step
    name   => "Mount at boot time",
    ensure => { fgrep qr/3:on/, '/sbin/chkconfig --list netfs|' },
    using  => { system "/sbin/chkconfig --level 3 netfs on" };
  step
    name   => "Mount now",
    ensure => { fgrep qr|^$source:$scratch\b|, 'df|' },
    using  => { system "/sbin/service netfs start" };
  
With guarded commands, this script has numerous advantages over the
previous ones:

=over

=item *

It is more B<resilient>.  Because its checks are semantic in nature,
the script will react properly to minor changes in the environment
that would derail a conventionally written script.

=item *

If it fails due to some unforeseen problem, it can be rerun once the
problem has been fixed.  It will automatically pick up exactly where
it left off.

=item *

Not only can this script be used to cause the intended state to come
into being (in this case, mounting the scratch filesystem), it can be
used to I<verify> that the state exists. If it exits without failure,
then the desired state is verified.  

It would be perfectly reasonable, for instance, to include the above
script in a I<crontab> entry run periodically.  If something went
awry--e.g., the directory were unmounted or I<portmap> was removed
from the init list--the script would notice this problem and repair
it.

=item *

Only I<important> tests--the semantic ones--need to be written.  There
is no need to check every possible error condition of every line of
code.

=item *

The error-checking code and the running code are held together in a
single C<step> command, but are separated into C<ensure> and C<using>
blocks.  This results in a more readable script without error-checking
spaghetti.

=back

=over

=item B<Lesson 2.>

With guarded commands, you can afford to be audacious.

=back

If I<every> line of code in a script that has a side effect is put
into C<using> blocks, the script becomes much less dangerous.  There
is a smaller chance that the script will "run away" and do something
unexpected and horrible.  Each step is checked after a C<using> block
is run, and so long as your semantic tests are correct, the script
will halt if things start to go awry.

=over

=item B<Lesson 3.>

Take note of idempotence, and turn it to your advantage.

=back

A function I<f> is I<idempotent> if it has the property

=over

I<f>(I<f>(I<x>)) = I<f>(I<x>)

=back

in other words, something is idempotent if doing it twice has the same
effect as doing it once.  Many UNIX tools have the property of
idempotence: I<ln -f>, I<cp>, and I<rsync> are three examples.  (Some
tools look like they're idempotent but aren't, e.g. I<mount>: you can
mount the same filesystem twice on the "same", overlapping
mountpoints.)

Idempotence is a large part of the power of Commands::Guarded.  If you
put every expression with side-effects (or, more precisely, side
effects that will persist beyond the life of the script, e.g. writing
files) into a C<using> block, each successful step in the script becomes
idempotent.  In turn, a script that completes successfully is also
idempotent: you can run it again and it should change nothing.

Knowing about idempotence can help you in writing your C<ensure> and
C<using> blocks.  For instance, the step above

  step
    name   => "Start NFS services automatically at boot",
    ensure => {
      fgrep qr/3:on/, '/sbin/chkconfig --list portmap|' and
      fgrep qr/3:on/, '/sbin/chkconfig --list nfslock|';
    },
    using  => {
      system "/sbin/chkconfig --level 3 portmap on";
      system "/sbin/chkconfig --level 3 nfslock on";
    };

Has been made simpler by noting the idempotence of I<chkconfig>.  It's
possible that I<portmap> is already enabled but <nfslock> is not,
causing the C<ensure> to fail.  But because the I<chkconfig>
statements in the C<using> block are idempotent, it is safe to run the
line

  system "/sbin/chkconfig --level 3 portmap on";

again, even if I<portmap> is already enabled.

=head1 EXPORTS

By default only C<step> is exported.

The following import tags can be used:

=over

=item C<:default>

Imports the default C<step>.  This is also what you get if you just say

  use Commands::Guarded;

but you will need to use one of these tags if you import another tag
or named sub.

=item C<:other>

Imports C<clear_rollbacks> and C<verbose>.

=back

=head1 SEE ALSO

E. W. Dijkstra, "Guarded commands, nondeterminacy and formal
derivation of programs," I<Communications of the ACM>, Vol. 18, No. 8,
1975, pp. 453-458.  Describes guarded commands in a fundamentally
different form than implemented in this module.

This module was first presented in an Invited Talk at the 18th Annual
System Administration Conference, Atlanta, 18 Nov 2004, sponsored by
SAGE L<http://www.sage.org/> and USENIX L<http://www.usenix.org/>.
See L<http://www.usenix.org/events/lisa04/>.  (Please note that
because this was an Invited Talk, information is not included in the
proceedings of that conference.)

=head1 TODO

=over

=item *

A method to selectively clear rollbacks.  This is complicated because
the same rollback codeblock might be registered several times with
different arguments using C<do(ARGS)>.

=item *

Rational behavior when C<ensure> or (and?) C<using> are omitted.
Today it just throws an error.

=item *

A reasonable way to extend and subclass.  You could do it today, but
it would be relatively tough--which is why it's not documented.

=back

=head1 ACKNOWLEDGMENTS

I would like to thank Damian Conway for his invaluable assistance on
this module, including on naming of the constructs and the module
itself, and for pointing out to me Dijkstra's prior work.

Thanks and love to J.D., for keeping me sane.  As sane as I ever am,
anyway.

=head1 AUTHOR

Trey Harris, E<lt>treyharris@gmail.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2005 by Ingo Blechschmidt (port to Perl 6)

Copyright 2004 by Trey Harris

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=cut
