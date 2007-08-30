=begin pod

=head1 NAME

Sys::Statistics::Linux - Front-end module to collect system statistics

=head1 SYNOPSIS

   use Sys::Statistics::Linux;

   my $lxs = Sys::Statistics::Linux.new;

   $lxs.set(
      sysinfo   => 1,
      cpustats  => 1,
      procstats => 1,
      memstats  => 1,
      pgswstats => 1,
      netstats  => 1,
      sockstats => 1,
      diskstats => 1,
      diskusage => 1,
      loadavg   => 1,
      filestats => 1,
      processes => 1,
   );

   sleep 1;

   my %stat = $lxs.get;

=head1 NOTE NOTE NOTE

Only new(), set() and get() are useable at the moment. Statistics for option Processes
aren't implemented yet. This documentation is the old p5 documentation. I'm working on
it.

=head1 DESCRIPTION

Sys::Statistics::Linux is a front-end module and gather different linux system informations
like processor workload, memory usage, network and disk statistics and a lot more. Refer the
documentation of the distribution modules to get more informations about all possible statistics.

=head1 MOTIVATION

My motivation is very simple. Every linux administrator knows the well-known tool sar of sysstat.
It helps me a lot of time to search for system bottlenecks and to solve problems but it's hard to
parse the output to store different statistics into a database. So I though to develope
Sys::Statistics::Linux. It's not a replacement but it should make it simpler to you to write your
own system monitor.

If Sys::Statistics::Linux doesn't provide statistics that are strongly needed then let me know it.

=head1 TECHNICAL NOTE

This distribution collects statistics by the virtual F</proc> filesystem (procfs) and is
developed on default vanilla kernels. It is tested on x86 hardware with the distributions SuSE
(SuSE on s390 and s390x architecture as well), openSUSE, RHLE, Fedora, Debian, Ubuntu, Asianux,
Slackware and Mandriva on kernel versions 2.4 and/or 2.6 and should run on all linux kernels with
a default vanilla kernel as well. It's possible that it doesn't run on all linux distributions
if some procfs features are deactived or too much modified. As example the linux kernel 2.4 can
compiled with the option C<CONFIG_BLK_STATS> what turn on or off block statistics for devices.

=head1 DELTAS

The statistics C<CpuStats>, C<ProcStats>, C<PgSwStats>, C<NetStats>, C<DiskStats> and C<Processes>
are deltas, for this reason it's necessary to initialize the statistics first before the data
can be prepared by C<get()>. These statistics can be initialized with the methods C<new()>, C<set()>
and C<init()>. Any option that is set to TRUE (1) will be initialized by the call of C<new()>
or C<set()>. The call of C<init()> reinitialize all statistics that are set to 1. By the call
of C<get()> the initial statistics will be updated automatically. Please refer the METHOD section
to get more information about the calls of C<new()>, C<set()> and C<get()>.

Another exigence is to sleep for while - at least for one second - before the
call of C<get()> if you want to get useful statistics. The options C<SysInfo>, C<MemStats>,
C<SockStats>, C<DiskUsage>, C<LoadAVG> and C<FileStats> are no deltas. If you need only one
of these informations you don't need to sleep before the call of C<get()>.

The method C<get()> then prepares all requested statistics and returns it as a hash reference.
The inital statistics will be updated. You can turn on and off options with C<set()>.

=head1 OPTIONS

All options are identical with the package names of the distribution. To activate the gathering
of statistics you have to set the options by the call of C<new()> or C<set()>. In addition you
can delete, set pause or create new statistics with C<set()> and re-init all statistics with
C<init()>.

The options must be set with on of the following values:

    0 - deactivate statistics
    1 - create a new object and init statistics if necessary
    2 - create a new object if not exists but wouldn't init statistics

In addition it's possible to handoff a process list for option C<Processes>.

    my $lxs = Sys::Statistics::Linux.new(
       Processes => {
          init => 1,
          pids => [ 1, 2, 3 ]
       }
   );

To get more informations about the statistics refer the different modules of the distribution.

   sysinfo     -  Collect system informations             with Sys::Statistics::Linux::SysInfo.
   cpustats    -  Collect cpu statistics                  with Sys::Statistics::Linux::CpuStats.
   procstats   -  Collect process statistics              with Sys::Statistics::Linux::ProcStats.
   memstats    -  Collect memory statistics               with Sys::Statistics::Linux::MemStats.
   pgswstats   -  Collect paging and swapping statistics  with Sys::Statistics::Linux::PgSwStats.
   netstats    -  Collect net statistics                  with Sys::Statistics::Linux::NetStats.
   sockstats   -  Collect socket statistics               with Sys::Statistics::Linux::SockStats.
   diskstats   -  Collect disk statistics                 with Sys::Statistics::Linux::DiskStats.
   diskusage   -  Collect the disk usage                  with Sys::Statistics::Linux::DiskUsage.
   loadavg     -  Collect the load average                with Sys::Statistics::Linux::LoadAVG.
   filestats   -  Collect inode statistics                with Sys::Statistics::Linux::FileStats.
   processes   -  Collect process statistics              with Sys::Statistics::Linux::Processes.

=head1 METHODS

=head2 new()

Call C<new()> to create a new Sys::Statistics::Linux object. You can call C<new()> with options.
This options would be handoff to the method C<set()>.

Without options

         my $lxs = Sys::Statistics::Linux.new;

Or with options

         my $lxs = Sys::Statistics::Linux.new(CpuStats => 1);

Would do nothing

         my $lxs = Sys::Statistics::Linux.new(CpuStats => 0);

Take a look to C<set()> for more informations.

=head2 set()

Call C<set()> to activate or deactivate options. The following example would call C<new()> and
C<init()> of C<Sys::Statistics::Linux::CpuStats> and delete the object of
C<Sys::Statistics::Linux::SysInfo>:

         $lxs.set(
            Processes =>  0, # deactivate this statistic
            PgSwStats =>  1, # activate the statistic and calls new() and init() if necessary
            NetStats  =>  2, # activate the statistic and call new() if necessary but not init()
         );

=head2 get()

Call C<get()> to get the collected statistics. C<get()> returns the statistics as a hash reference.

         my $stats = $lxs.get;

=head2 init()

The call of C<init()> re-init all statistics that are necessary for deltas and if the option is
higher than 0.

         $lxs.init;

=head2 search(), psfind()

NOT YET IMPLEMENTED

=head2 settime()

NOT YET IMPLEMENTED

=head2 gettime()

NOT YET IMPLEMENTED

=head1 EXAMPLES

A very simple perl script could looks like this:

         use Sys::Statistics::Linux;

         my $lxs = Sys::Statistics::Linux.new( CpuStats => 1 );
         sleep(1);
         my %stats  = $lxs.get;
         my %cpu   := %stats<CpuStats><cpu>;

         say "Statistics for CpuStats (all)\n";
         say "  user      %cpu<user>";
         say "  nice      %cpu<nice>";
         say "  system    %cpu<system>";
         say "  idle      %cpu<idle>";
         say "  ioWait    %cpu<iowait>";
         say "  total     %cpu<total>";

=head1 DEPENDENCIED

No at the moment.

=head1 EXPORTS

No exports.

=head1 TODOS

   * Are there any wishs from your side? Send me a mail!

=head1 REPORTING BUGS

Please report all bugs to <jschulz.cpan(at)bloonix.de>.

=head1 AUTHOR

Jonny Schulz <jschulz.cpan(at)bloonix.de>.

=head1 COPYRIGHT

Copyright (c) 2006, 2007 by Jonny Schulz. All rights reserved.

This program is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

=cut

#package Sys::Statistics::Linux;
#our $VERSION = '0.15';

class Sys::Statistics::Linux-0.001;

use v6-alpha;

#use strict;
#use warnings;
#use Carp qw(croak);
#use POSIX qw(strftime);
#use UNIVERSAL;
#use UNIVERSAL::require;
use lib '/home/bloonix/pugs/ext/Sys-Statistics-Linux/lib';

sub croak (*@m) { die @m } # waiting for Carp::croak

#sub new {
#   my $class = shift;
#   my $self = bless {
#      opts => {
#         SysInfo   =>  0,
#         CpuStats  =>  0,
#         ProcStats =>  0,
#         MemStats  =>  0,
#         PgSwStats =>  0,
#         NetStats  =>  0,
#         SockStats =>  0,
#         DiskStats =>  0,
#         DiskUsage =>  0,
#         LoadAVG   =>  0,
#         FileStats =>  0,
#         Processes =>  0,
#      },
#      init  => {},
#      stats => {},
#      obj   => {},
#   }, $class; 
#   $self->set(@_) if @_;
#   return $self;
#}

has %.inits;
has %.stats;
has %.objects;
has %.regmaps;
has %.options;

submethod BUILD (*%p) {
    #               alias               module name               init()
    self.register('sysinfo',   'Sys::Statistics::Linux::SysInfo',   0);
    self.register('cpustats',  'Sys::Statistics::Linux::CpuStats',  1);
    self.register('procstats', 'Sys::Statistics::Linux::ProcStats', 1);
    self.register('memstats',  'Sys::Statistics::Linux::MemStats',  0);
    self.register('pgswstats', 'Sys::Statistics::Linux::PgSwStats', 1);
    self.register('netstats',  'Sys::Statistics::Linux::NetStats',  1);
    self.register('sockstats', 'Sys::Statistics::Linux::SockStats', 0);
    self.register('diskstats', 'Sys::Statistics::Linux::DiskStats', 1);
    self.register('diskusage', 'Sys::Statistics::Linux::DiskUsage', 0);
    self.register('loadavg',   'Sys::Statistics::Linux::LoadAVG',   0);
    self.register('filestats', 'Sys::Statistics::Linux::FileStats', 0);
    self.register('processes', 'Sys::Statistics::Linux::Processes', 1);
    self.set(%p);
}

method register ($a, $m, $i) {
    self.regmaps{$a} = { class => $m, init => $i };
    self.options{$a} = 0;
}

#sub set {
#   my $self  = shift;
#   my $class = ref $self;
#   my $args  = ref($_[0]) eq 'HASH' ? shift : {@_};
#   my $opts  = $self->{opts};
#   my $obj   = $self->{obj};
#   my $stats = $self->{stats};
#   my $pids  = ();
#
#   if (ref($args->{Processes}) eq 'HASH') {
#      $pids = $args->{Processes}->{pids};
#      $args->{Processes} = $args->{Processes}->{init};
#   }
#
#   foreach my $opt (keys %{$args}) {
#
#      # validate the options
#      croak "$class: invalid option '$opt'"
#         unless exists $opts->{$opt};
#      croak "$class: invalid value for '$opt'"
#         unless $args->{$opt} =~ qr/^[012]\z/;
#
#      $opts->{$opt} = $args->{$opt};
#
#      if ($opts->{$opt}) {
#         my $package = $class."::".$opt;
#
#         # require mod if not loaded
#         unless ($obj->{$opt}) {
#            $package->require
#               or croak "$class: unable to load $package";
#         }
#
#         # create a new object if the object doesn't exist
#         # or create a new process list object if $pids is set
#         if ($opt eq 'Processes' && $pids) {
#            $obj->{$opt} = $package->new($pids);
#         } elsif (!$obj->{$opt}) {
#            $obj->{$opt} = $package->new;
#         }
#
#         # get initial statistics if the function init() exists
#         # and the option is set to 1
#         $obj->{$opt}->init() 
#            if $opts->{$opt} == 1
#            && UNIVERSAL::can($package, 'init');
#
#      } elsif ($stats->{$obj}) {
#         delete $stats->{$obj};
#      }
#   }
#}

method set (%p) {
    my %regmaps := self.regmaps;
    my %options := self.options;
    my %objects := self.objects;

    for %p.kv -> $k, $v {
        unless %options.exists($k) {
            croak("invalid option '$k'");
        }
        unless $v ~~ /^<[012]>$/ {
            croak("invalid value for option '$k'");
        }
        if $v {
            if !%objects.exists($k) {
                require %regmaps{$k}<class>;
                %objects{$k} = ::(%regmaps{$k}<class>).new;
            }
            if %regmaps{$k}<init> {
                %objects{$k}.init;
            }
        } else {
            self.stats{$k}.delete;
        }
        %options{$k} = $v;
    }
}

#sub init {
#   my $self  = shift;
#   my $class = ref $self;
#   my $obj   = $self->{obj};
#   my $opts  = $self->{opts};
#
#   foreach my $opt (keys %{$opts}) {
#      $obj->{$opt}->init()
#         if defined &{"$class::$opt::init"}
#         && $opts->{$opt} > 0;
#   }
#}

#sub settime {
#   my $self   = shift;
#   my $format = $_[0] ? $_[0] : '%Y-%m-%d %H:%M:%S';
#   $self->{timeformat} = $format;
#}

#sub gettime {
#   my $self = shift;
#   $self->settime(@_) unless $self->{timeformat};
#   my $tm = strftime($self->{timeformat}, localtime);
#   return wantarray ? split /\s+/, $tm : $tm;
#}

#sub get {
#   my $self   = shift;
#   my $opts   = $self->{opts};
#   my $stats  = $self->{stats};
#   my $obj    = $self->{obj};
#
#   foreach my $opt (keys %{$opts}) {
#      $stats->{$opt} = $obj->{$opt}->get()
#         if $opts->{$opt};
#   }
#
#   return $stats;
#}

method get () {
    my %inits   := self.inits;
    my %stats   := self.stats;
    my %options := self.options;
    my %objects := self.objects;

    for %options.kv -> $k, $v {
        if $v {
            %stats{$k} = %objects{$k}.get;
        }
    }

    return %stats;
}

#sub search {
#   my $self  = shift;
#   my $class = ref($self);
#
#   croak "$class: first argument have to be a hash ref"
#     unless $_[0] && ref($_[0]) eq 'HASH';
#   croak "$class: second argument have to be a hash ref"
#     if $_[1] && ref($_[1]) ne 'HASH';
#
#   my ($filter, $stats) = @_ == 2 ? @_ : (shift, $self->{stats});
#
#   # $stats and $filter must be set
#   return undef unless %{$stats} && %{$filter};
#
#   my $opts = $self->{opts};
#   my %hits = ();
#
#   foreach my $opt (keys %{$filter}) {
#
#      croak "$class: not a hash ref opt '$opt'"
#         unless ref($filter->{$opt}) eq 'HASH';
#      croak "$class: invalid option '$opt'"
#         unless exists $opts->{$opt};
#
#      # next if the object isn't loaded
#      next unless exists $stats->{$opt};
#
#      # we search for matches for each key that is defined
#      # in %filter and rebuild the tree until that key that
#      # matched the searched string
#
#      my $fref = $filter->{$opt};
#      my $sref = $stats->{$opt};
#
#      foreach my $x (keys %{$fref}) {
#
#         # if $fref->{$x} is a hash ref then the next key have to
#         # match the statistic key. this is used for statistics
#         # like NetStats or Processes that uses a hash key for the
#         # device name or process id. NetStats example:
#         #
#         # if
#         #
#         #    $fref->{eth0}->{ttbyt}
#         #
#         # is defined as a filter then the key "eth0" have to match
#         #
#         #    $sref->{eth0}
#         #
#         # then we look if "ttbyt" matched the searched statistic name
#         # and compare the values. if the comparing returns TRUE we the
#         # hash is copied until the matched key-value pair.
#
#         if (ref($fref->{$x}) eq 'HASH') {
#
#            # if the key $sref->{eth0} doesn't exists
#            # then we continue with the next defined filter
#            next unless exists $sref->{$x};
#
#            while ( my ($name, $value) = each %{$fref->{$x}} ) {
#               $hits{$opt}{$x}{$name} = $sref->{$x}->{$name}
#                  if exists $sref->{$x}->{$name}
#                  && $class->_compare($sref->{$x}->{$name}, $value);
#            }
#         } else {
#            foreach my $key (keys %{$sref}) {
#               if (ref($sref->{$key}) eq 'HASH') {
#                  $hits{$opt}{$key}{$x} = $sref->{$key}->{$x}
#                     if exists $sref->{$key}->{$x}
#                     && $class->_compare($sref->{$key}->{$x}, $fref->{$x});
#               } else { # must be a scalar now
#                  $hits{$opt}{$x} = $sref->{$x}
#                     if exists $sref->{$x}
#                     && $class->_compare($sref->{$x}, $fref->{$x});
#                  last;
#               }
#            }
#         }
#      }
#   }
#
#   return wantarray ? %hits : \%hits;
#}

#sub psfind {
#   my $self  = shift;
#   my $class = ref($self);
#
#   croak "$class: first argument have to be a hash ref"
#     unless $_[0] && ref($_[0]) eq 'HASH';
#   croak "$class: second argument have to be a hash ref"
#     if $_[1] && ref($_[1]) ne 'HASH';
#
#   my ($filter, $stats) = @_ == 2 ? @_ : (shift, $self->{stats});
#
#   return undef unless %{$stats->{Processes}} && %{$filter};
#
#   my @hits = ();
#   my $sref = $stats->{Processes};
#
#   foreach my $pid (keys %{$sref}) {
#      my $proc = $sref->{$pid};
#
#      while ( my ($key, $value) = each %{$filter} ) {
#         push @hits, $pid
#            if exists $proc->{$key}
#            && $class->_compare($proc->{$key}, $value);
#      }
#   }
#
#   return wantarray ? @hits : \@hits;
#}

#
# private stuff
#

#sub _compare {
#   my ($class, $x, $y) = @_;
#
#   if (ref($y) eq 'Regexp') {
#      return $x =~ $y;
#   } elsif ($y =~ s/^eq://) {
#      return $x eq $y;
#   } elsif ($y =~ s/^ne://) {
#      return $x ne $y;
#   } elsif ($y =~ s/^gt://) {
#      return $x > $y if $y =~ /^\d+\z/;
#      croak "$class: not a number for operator 'gt'";
#   } elsif ($y =~ s/^lt://) {
#      return $x < $y if $y =~ /^\d+\z/;
#      croak "$class: not a number for operator 'lt'";
#   } else {
#      croak "$class: bad search() / psfind() operator '$y'";
#   }
#
#   return undef;
#}

1;
