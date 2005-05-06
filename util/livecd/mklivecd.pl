#!/usr/bin/perl

use warnings;
use strict;

use Getopt::Long;
use LWP::Simple     qw<getstore>;
use File::Copy      qw<copy>;
use File::Basename  qw<basename>;
use Term::ANSIColor qw<:constants>;

$ENV{PATH} .= ":/sbin:/usr/sbin";

sub step {
  my %args = @_;
  local $_;

  $args{descr}  ||= "(no description)";
  $args{ensure} ||= sub { 1 };
  $args{using}  ||= sub {};
  $args{help}   ||= "";

  printf STDERR " %s*%s %s...\n", BOLD . BLUE, RESET, $args{descr};
  print STDERR "   : $_\n" for map { (/^\s*(.*)$/g)[0] } split "\n", $args{help};
  if($args{ensure}->()) {
    printf STDERR "   = skipped (%sgood%s).\n", BOLD . GREEN, RESET;
  } else {
    $args{using}->();
    if($args{ensure}->()) {
      printf STDERR "   = %sgood%s.\n", BOLD . GREEN, RESET;
    } else {
      printf STDERR "   = %sfailed%s.\n", BOLD . RED, RESET;
      exit 1;
    }
  }
}

my $kernel_uri   = "http://m19s28.vlinux.de/iblech/pugs/livecd-kernel.bin";
my $grub_uri     = "http://m19s28.vlinux.de/iblech/pugs/grub.tar.bz2";
my $kernel_local = "vmlinuz";
my $grub_local   = "grub.tar.bz2";
my $pugs         = "../../pugs";
my $initrd_gz    = "initrd.gz";
my $initrd_img   = "initrd.img";
my $initrd_mnt   = "/mnt/loop0";
my $initrd_size  = 20 * 1024;
my $cdroot       = "cdroot";
my $iso          = "cd.iso";

sub usage { die <<USAGE }
Usage: $0 [options]

$0 creates a very minimalistic Pugs Live CD.
It fetches kernel and GRUB from a public server (which can be specified below).
Then, it creates a initrd and copies your pugs binary and the shared libraries
it needs on it. Finally, it will call mkisofs to create a ISO9660 image.

You can stop this program at any time, it'll continue when you run it again.

*Note*: This program should be run as unpreviliged user, it will ask you to
enter commands requiring root privilegies.

*Note*: I only tested this program on Linux, but it should work on other *nix
systems, too. For now, I made the script stop if you aren't running under
Linux, you may want to change that.

Available options:
  --kernel-uri=http://path/to/kernel/to/use
    Selects the kernel the livecd should use.
    Defaults to "$kernel_uri".
  --kernel-local=vmlinuz
    Selects the local name for the kernel.
    Defaults to "$kernel_local".
  --grub-uri=http://path/to/kernel/to/use
    Selects the tar.bz2 GRUB is contained in.
    Defaults to "$grub_uri".
  --grub-local=grub.tar.bz2
    Selects the local name for the GRUB tar.bz2.
    Defaults to "$grub_local".
  --pugs=/path/to/pugs
    Selects the Pugs binary to use.
    Defaults to \"$pugs\".
  --initrd-gz=initrd.gz
    Selects the file the initrd.img will be gzipped to.
    Defaults to \"$initrd_gz\".
  --initrd-img=initrd.img
    Selects the file the initrd-dir will be put in.
    Defaults to \"$initrd_img\".
  --initrd-size=20480
    Selects the size of the initrd.img (in KiB).
    Defaults to \"$initrd_size\".
  --initrd-mnt=/mnt/mountpoint
    Selects the mountpoint to mount the initrd in.
    Defaults to \"$initrd_mnt\".
  --cdroot=cdroot
    Selects the directory which will later be the / of the CD.
    Defaults to \"$cdroot\".
  --iso=cd.iso
    Selects the final ISO9660 image name to use.
    Defaults to \"$iso\".
  --help
    Displays this help.

Options may be abbreviated to uniqueness.

Author:         Ingo Blechschmidt <iblech\@web.de>
Many thanks to: Michael Hartmann <michael.hartmann\@as-netz.de>
USAGE

GetOptions(
  "kernel-uri=s"   => \$kernel_uri,
  "grub-uri=s"     => \$grub_uri,
  "kernel-local=s" => \$kernel_local,
  "grub-local=s"   => \$grub_local,
  "pugs=s"         => \$pugs,
  "initrd-gz=s"    => \$initrd_gz,
  "initrd-img=s"   => \$initrd_img,
  "initrd-size=i"  => \$initrd_size,
  "initrd-mnt=s"   => \$initrd_mnt,
  "cdroot=s"       => \$cdroot,
  "iso=s"          => \$iso,
  help             => \&usage,
) or usage();
check_for_evil_chars($initrd_img, $initrd_gz);

my $welcomed = 0;
step
  descr  => "Welcome",
  ensure => sub { $welcomed },
  using  => sub { print STDERR "   : Press a key to continue... "; <STDIN>; $welcomed++ },
  help   => <<HELP;
    Have you read --help?
    You can stop this program by hitting ^C (<Ctrl>+<C>).
HELP

step
  descr  => "Checking for Linux",
  help   => "This program needs Linux to run correctly.",
  ensure => sub { $^O eq "linux" };

step
  descr  => "Fetching GRUB from \"$grub_uri\"",
  help   => "Remove \"$grub_local\" if you want to refetch GRUB.",
  ensure => sub { -r $grub_local and -s $grub_local },
  using  => sub { getstore $grub_uri => $grub_local };

step
  descr  => "Fetching kernel from \"$kernel_uri\"",
  help   => "Remove \"$kernel_local\" if you want to refetch the kernel.",
  ensure => sub { -r $kernel_local and -s $kernel_local },
  using  => sub { getstore $kernel_uri => $kernel_local };

step
  descr  => "Checking for Pugs binary",
  help   => "Compile Pugs if you haven't done so already.",
  ensure => sub { -r $pugs and -s $pugs };

my @libs;
step
  descr  => "Checking which shared libraries pugs requires",
  ensure => sub { @libs > 1 },
  using  => sub { @libs = ldd($pugs) },
  help   => <<HELP;
    We use 'ldd' to read the list of shared libraries (*.so) pugs
    requires. This is necessary so we can copy them on the CD later.
HELP

step
  descr  => "Creating an empty \"$initrd_img\"",
  ensure => sub { -r $initrd_img and -s $initrd_img == 1024 * $initrd_size },
  using  => sub { system "dd", "if=/dev/zero", "of=$initrd_img", "bs=1K", "count=$initrd_size" },
  help   => "We're now creating a zeroed image we will later mount.";

step
  descr  => "Creating an ext2 filesystem on \"$initrd_img\"",
  ensure => sub { `file $initrd_img` =~ /ext2/ },
  using  => sub { system "mkfs.ext2", "-F", $initrd_img };

step
  descr  => "Mounting \"$initrd_img\"",
  ensure => sub { `mount` =~ /\Q$initrd_img/ },
  using  => sub { print STDERR "   : Press a key to continue... "; <STDIN> },
  help   => <<HELP;
    Please mount \"$initrd_img\" into \"$initrd_mnt\" by entering the following
    command as root:
    # mount -o loop -t ext2 $initrd_img $initrd_mnt
HELP

my @dirs = map { "$initrd_mnt/$_" } "dev", "lib";
step
  descr  => "Creating directories " . join(", ", map { "$_" } @dirs),
  ensure => sub { -d $_ or return for @dirs; 1 },
  using  => sub { mkdir $_ for @dirs },
  help   => <<HELP;
    We need a temporary directory the initrd.gz will be built later from.
    We'll copy pugs and the libraries it needs to it.
HELP

step
  descr  => "Copying Pugs binary to \"$initrd_mnt/linuxrc\"",
  ensure => sub {
    -r "$initrd_mnt/linuxrc" and -x "$initrd_mnt/linuxrc" and
    -M "$initrd_mnt/linuxrc" <= -M $pugs;
  },
  using  => sub {
    copy $pugs => "$initrd_mnt/linuxrc";
    chmod 0755, "$initrd_mnt/linuxrc";
  };

step
  descr  => "Copying shared libraries to \"$initrd_mnt/lib\"",
  ensure => sub {
    -r "$_" && -x "$_" or return for map { "$initrd_mnt/lib/" . basename $_ } @libs;
    1;
  },
  using  => sub {
    for(@libs) {
      copy $_ => "$initrd_mnt/lib/" . basename $_;
      chmod 0755, "$initrd_mnt/lib/" . basename $_;
    }
  };

step
  descr  => "Copying necessary device files to \"$initrd_mnt/dev\"",
  ensure => sub { -c "$initrd_mnt/dev/console" },
  using  => sub { print STDERR "   : Press a key to continue... "; <STDIN> },
  help   => <<HELP;
    Please create a $initrd_mnt/dev/console by entering the following command
    as root:
    # mknod $initrd_mnt/dev/console c 5 1
HELP

step
  descr  => "Unmounting \"$initrd_img\"",
  ensure => sub { `mount` !~ /\Q$initrd_img/ },
  using  => sub { print STDERR "   : Press a key to continue... "; <STDIN> },
  help   => <<HELP;
    Please umount \"$initrd_img\" by entering the following command as root:
    # umount $initrd_img
HELP

my $ran_gzip = 0;
step
  descr  => "Compressing \"$initrd_img\"",
  ensure => sub { -r $initrd_gz and $ran_gzip },
  using  => sub { system "gzip -vvv -9 -c $initrd_img > $initrd_gz"; $ran_gzip++ };

step
  descr  => "Creating directory \"$cdroot\"",
  ensure => sub { -d $cdroot and -r $cdroot },
  using  => sub { mkdir $cdroot };

step
  descr  => "Unpacking GRUB in \"$cdroot\"",
  ensure => sub { -d "$cdroot/boot/grub" and -r "$cdroot/boot/grub/stage2_eltorito" },
  using  => sub { system "tar", "-xvjf", $grub_local, "-C", $cdroot };

step
  descr  => "Creating \"$cdroot/boot/grub/menu.lst\"",
  ensure => sub { -r "$cdroot/boot/grub/menu.lst" },
  using  => sub { open my $fh, ">", "$cdroot/boot/grub/menu.lst"; print $fh <<GRUB };
default  0
timeout  5

title    Pugs
root     (cd)
kernel   /boot/vmlinuz root=/dev/ram init=/linuxrc ramdisk_size=$initrd_size quiet
initrd   /boot/initrd.gz
boot
GRUB

step
  descr  => "Copying \"$initrd_gz\" to \"$cdroot/boot/initrd.gz\"",
  ensure => sub { -r "$cdroot/boot/initrd.gz" },
  using  => sub { copy $initrd_gz => "$cdroot/boot/initrd.gz" };

step
  descr  => "Copying kernel to \"$cdroot/boot/vmlinuz\"",
  ensure => sub { -r "$cdroot/boot/vmlinuz" },
  using  => sub { copy $kernel_local => "$cdroot/boot/vmlinuz" };

step
  descr  => "Creating final ISO",
  ensure => sub { -r $iso and -M $iso <= -M $initrd_gz },
  using  => sub {
    system
      "mkisofs",
      "-R",                                # Generate Rock Ridge directory information
      "-b" => "boot/grub/stage2_eltorito", # Set El Torito boot image name
      "-no-emul-boot",                     # Boot image is 'no emulation' image
      "-boot-load-size" => 4,              # Set numbers of load sectors
      "-boot-info-table",                  # Patch boot image with info table
      "-o" => $iso,                        # Set output file name
      $cdroot;
  };

sub ldd {
  my $bin = shift;
  my @so;
  local $_;
  
  open my $ldd, "-|", "ldd", $bin or die "Couldn't open pipe to \"ldd $bin\": $!\n";
    while(<$ldd>) {
      s{^.*?/}{} or next;
      push @so, "/" . (split " ")[0];
    }

  return @so;
}

# This is a kludge. But for now, as a simple script, it suffices.
# This script doesn't run as root, anyway.
sub check_for_evil_chars {
  local $_;
  for(@_) {
    die "Error: Parameter \"$_\" contains potential unsafe characters.\n"
      if /[ <>;&\\\$'"]/;
  }
}
