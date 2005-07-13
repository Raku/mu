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

  $args{help} =~ s/# (.*)$/# @{[BOLD . WHITE]}$1@{[RESET]}/m;

  printf STDERR "%s»%s %s%s%s... ", BOLD . BLUE, RESET, BOLD . RED, $args{descr}, RESET;

  if($args{ensure}->()) {
    printf STDERR "skipped (%sgood%s).\n", BOLD . GREEN, RESET;
  } else {
    if($args{help}) {
      printf STDERR "\b\n";
      printf STDERR "  %s:%s %s\n", BOLD . BLUE, RESET, $_ for map { (/^\s*(.*)$/g)[0] } split "\n", $args{help};
      printf STDERR "  %s»%s ", BOLD . BLUE, RESET;
    }
    $args{using}->();
    if($args{pause}) {
      printf STDERR "\b\b\b\b\b   %s:%s Press a key to continue... ", BOLD . BLUE, RESET;
      <STDIN>;
      printf STDERR "  %s»%s ", BOLD . BLUE, RESET;
    }
    if($args{ensure}->()) {
      printf STDERR "%sgood%s.\n", BOLD . GREEN, RESET;
    } else {
      printf STDERR "%sfailed%s.\n", BOLD . RED, RESET;
      exit 1;
    }
  }
}

my $kernel_uri   = "http://m19s28.vlinux.de/iblech/pugs/livecd-kernel.bin";
my $grub_uri     = "http://m19s28.vlinux.de/iblech/pugs/grub.tar.bz2";
my $kernel_local = "vmlinuz";
my $grub_local   = "grub.tar.bz2";
my $pugs         = "../../pugs";
my $pge          = "../../src/pge";
my $parrot_path  = "../../../parrot-trunk";
my $bash         = "/bin/bash";
my $inputrc      = "/etc/inputrc";
my $terminfo     = "/etc/terminfo/l/linux";
my $linuxrc      = "linuxrc";
my $welcome_p6   = "welcome.p6";
my $splashscreen = "splashscreen.txt";
my $lib6         = "../../blib6/lib";
my $initrd_gz    = "initrd.gz";
my $initrd_img   = "initrd.img";
my $initrd_mnt   = "/mnt/loop0";
my $initrd_size  = int 11.5 * 1024;
my $cdroot       = "cdroot";
my $iso          = "cd.iso";

sub usage { print <<USAGE; exit }
Usage: $0 [options]

$0 creates a very minimalistic Pugs Live CD.
It fetches kernel and GRUB from a public server (which can be specified below).
Then, it creates a initrd and copies your pugs binary and the shared libraries
it needs on it. Finally, it will call mkisofs to create a ISO9660 image.

You can stop this program at any time, it'll continue when you run it again.

Note: This program should be run as unpreviliged user, it will ask you to enter
      commands requiring root privilegies.

Note: The only option you'll have to specify is probably --initrd-mnt.

Note: I only tested this program on Linux, but it should work on other *nix
      systems, too. For now, I made the script stop if you aren't running under
      Linux, you may want to change that.

Note: A precompiled (but most of the time outdated) image is available at
      http://m19s28.vlinux.de/iblech/pugs/livecd.iso.

Note: If you want to modify the kernel used, you may want to use the
      configuration at
      http://m19s28.vlinux.de/iblech/pugs/livecd-kernel.config.

Available options and defaults:
  --kernel-uri=$kernel_uri
    mklivecd.pl automatically fetches a kernel from the Internet.
  --kernel-local=$kernel_local
    Locally, the kernel is saved as --kernel-local.
  --grub-uri=$grub_uri
    mklivecd.pl automatically fetches a GRUB tarball.
  --grub-local=$grub_local
    Locally, the GRUB tarball is saved as --grub-local.
  --pugs=$pugs
    mklivecd.pl takes --pugs as the binary to put on the CD.
  --parrot-path=$parrot_path
    mklivecd.pl takes --parrot-path as the root of an Parrot source directory.
  --pge=$pge
    mklivecd.pl takes --pge as the path of the PGE shipped with Pugs.
  --bash=$bash
    linuxrc needs a bash-compatible shell.
  --inputrc=$inputrc
    The readline library likes to have a /etc/inputrc.
  --terminfo=$terminfo
    To make, for example, the <Pos1> and <End> keys working, we need a terminfo
    file.
  --linuxrc=$linuxrc
    --linuxrc is the first script to run after the kernel has started.
  --welcome-p6=$welcome_p6
    --welcome-p6 is a Perl 6 program runnable by Pugs which will
    introduce Pugs.
  --splashscreen=$splashscreen
    The file referenced by --splashscreen will be displayed before booting.
  --lib6=$lib6
    --lib6 will be copied to the CD, too.
  --initrd-gz=$initrd_gz
    mklivecd.pl automatically generates a gzipped initrd suitable for GRUB.
  --initrd-img=$initrd_img
    --initrd-img is the temporary image of the initrd, which will be mounted.
  --initrd-size=$initrd_size
    --initrd-size specifies the size of the (uncompressed) initrd (in KiB).
  --initrd-mnt=$initrd_mnt
    --initrd-img will be mounted to --initrd-mnt in order to copy files to it.
  --cdroot=$cdroot
    --cdroot is the directory which will later be the / of the CD.
  --iso=$iso
    The final ISO9660 image name will be --iso.

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
  "bash=s"         => \$bash,
  "linuxrc=s"      => \$linuxrc,
  "welcome-p6=s"   => \$welcome_p6,
  "initrd-gz=s"    => \$initrd_gz,
  "initrd-img=s"   => \$initrd_img,
  "initrd-size=i"  => \$initrd_size,
  "initrd-mnt=s"   => \$initrd_mnt,
  "cdroot=s"       => \$cdroot,
  "iso=s"          => \$iso,
  help             => \&usage,
) or usage();
check_for_evil_chars($initrd_img, $initrd_gz, $initrd_mnt, $lib6, $parrot_path);

my $welcomed = 0;
step
  descr  => "Welcome",
  ensure => sub { $welcomed },
  using  => sub { $welcomed++ },
  pause  => 1,
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
  help   => "Remove \"$grub_local\" if you want mklivecd.pl to refetch GRUB.",
  ensure => sub { -r $grub_local and -s $grub_local },
  using  => sub { getstore $grub_uri => $grub_local };

step
  descr  => "Fetching kernel from \"$kernel_uri\"",
  help   => "Remove \"$kernel_local\" if you want mklivecd.pl to refetch the kernel.",
  ensure => sub { -r $kernel_local and -s $kernel_local },
  using  => sub { getstore $kernel_uri => $kernel_local };

step
  descr  => "Checking for Pugs binary",
  help   => "Compile Pugs if you haven't done so already.",
  ensure => sub { -r $pugs and -s $pugs };

step
  descr  => "Checking for Bash binary",
  ensure => sub { -r $bash and -s $bash };

my $pugs_version;
step
  descr  => "Querying Pugs for its version",
  ensure => sub { defined $pugs_version },
  using  => sub { $pugs_version = get_version($pugs) };

step
  descr  => "Checking for Parrot binary",
  ensure => sub { -r "$parrot_path/parrot" and -s "$parrot_path/parrot" };

my @modfiles;
step
  descr  => "Searching for module files",
  ensure => sub { @modfiles > 0 },
  using  => sub {
    @modfiles = map { (/^\Q$lib6\E\/(.+)$/)[0] }
		split "\000",
		`find $lib6 -print0`;
  };
my $newest_mod_stamp = 100000; # ugly
-M "$lib6/$_" <= $newest_mod_stamp and $newest_mod_stamp = -M "$lib6/$_"
  for @modfiles;

my @pfiles;
step
  descr  => "Searching for Parrot include files",
  ensure => sub { @pfiles > 0 },
  using  => sub {
    @pfiles = map { (/^\Q$parrot_path\/runtime\/parrot\/include\E(.+)$/)[0] }
	      grep { !/(^|\/)\.(?!\.)/ }
	      split "\000",
	      `find $parrot_path/runtime/parrot/include -print0`;
  };
my $newest_p_stamp = 100000; # ugly
-M "$parrot_path/runtime/parrot/include/$_" <= $newest_p_stamp and
  $newest_p_stamp = -M "$parrot_path/runtime/parrot/include/$_"
  for @pfiles;

my $rebuild;
step
  descr  => "Checking if we have to rebuild the initrd",
  ensure => sub { defined $rebuild },
  using  => sub {
    $rebuild = !(
      -r $initrd_gz and
      -M $initrd_gz <= -M $pugs and
      -M $initrd_gz <= -M "$parrot_path/parrot" and
      -M $initrd_gz <= -M $linuxrc and
      -M $initrd_gz <= -M $bash and
      -M $initrd_gz <= -M $inputrc and
      -M $initrd_gz <= -M $terminfo and
      -M $initrd_gz <= -M $welcome_p6 and
      -M $initrd_gz <= $newest_mod_stamp and
      -M $initrd_gz <= $newest_p_stamp and
      -d "$initrd_mnt/tmp"
    );
  };

if($rebuild) {
  my @libs;
  step
    descr  => "Checking which shared libraries pugs, parrot and bash require",
    ensure => sub { @libs > 1 },
    using  => sub { my %l; @l{ldd($pugs), ldd("$parrot_path/parrot"), ldd($bash)} = (); @libs = keys %l },
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
    pause  => 1,
    help   => <<HELP;
      Please mount \"$initrd_img\" into \"$initrd_mnt\" by entering the following
      command as root:
      # mount -o loop -t ext2 $initrd_img $initrd_mnt
HELP

  my @dirs = map { "$initrd_mnt/$_" }
	       "bin", "dev", "lib", "lib6", "tmp",
	       "etc", "etc/terminfo", "etc/terminfo/l";
  step
    descr  => "Creating directories " . join(", ", map { "$_" } @dirs),
    ensure => sub { -d $_ or return for @dirs; 1 },
    using  => sub { utime undef, undef, $initrd_img; mkdir $_ for @dirs },
    help   => <<HELP;
      We need a temporary directory the initrd.gz will be built later from.
      We'll copy pugs and the libraries it needs to it.
HELP

  my @files = (
    [$pugs                 => "$initrd_mnt/bin/pugs"],
    ["$parrot_path/parrot" => "$initrd_mnt/bin/parrot"],
    [$bash                 => "$initrd_mnt/bin/bash"],
    [$inputrc              => "$initrd_mnt/etc/inputrc"],
    [$terminfo             => "$initrd_mnt/etc/terminfo/l/linux"],
    [$linuxrc              => "$initrd_mnt/linuxrc"],
    [$welcome_p6           => "$initrd_mnt/welcome.p6"],
  );
  step
    descr  => "Copying Pugs, Parrot, Bash, inputrc, the terminfo description, linuxrc, and welcome.p6 to the initrd",
    help   => "Note: You might want to strip pugs and parrot to safe space.",
    ensure => sub {
      for(@files) {
	my ($src, $dest) = @$_;
        -r $dest and -x $dest and
	-M $dest <= -M $src   and
	-s $dest == -s $src
	  or return;
      }
      1;
    },
    using  => sub {
      utime undef, undef, $initrd_img;
      for(@files) {
	my ($src, $dest) = @$_;
	copy $src => $dest;
	chmod 0755, $dest;
      };
    };

  step
    descr  => "Copying Perl 6 modules to the initrd",
    ensure => sub { -r "$initrd_mnt/lib6/$_" or return for @modfiles; 1 },
    using  => sub {
      utime undef, undef, $initrd_img;
      for(@modfiles) {
	if(-d "$lib6/$_") {
	  mkdir "$initrd_mnt/lib6/$_";
	} else {
	  copy "$lib6/$_" => "$initrd_mnt/lib6/$_";
	}
      }
    };

  step
    descr  => "Copying Parrot include files to the initrd",
    ensure => sub { -r "$initrd_mnt/$_" or return for @pfiles; 1 },
    using  => sub {
      utime undef, undef, $initrd_img;
      for(@pfiles) {
	if(-d "$parrot_path/runtime/parrot/include/$_") {
	  mkdir "$initrd_mnt/$_";
	} else {
	  copy "$parrot_path/runtime/parrot/include/$_" => "$initrd_mnt/$_";
	}
      }
    };

  my @pge = map { s/^\Q$pge//; $_ } glob "$pge/* $pge/*/* $pge/*/*/*";
  step
    descr  => "Copying PGE to the initrd",
    ensure => sub { -r "$initrd_mnt/$_" or return for @pge; 1 },
    using  => sub {
      utime undef, undef, $initrd_img;
      for(@pge) {
	if(-d "$pge/$_") {
	  mkdir "$initrd_mnt/$_";
	} else {
	  copy "$pge/$_" => "$initrd_mnt/$_";
	}
      }
    };

  step
    descr  => "Copying shared libraries to \"$initrd_mnt/lib\"",
    ensure => sub {
      -r "$_" && -x "$_" or return for map { "$initrd_mnt/lib/" . basename $_ } @libs;
      1;
    },
    using  => sub {
      utime undef, undef, $initrd_img;
      for(@libs) {
	copy $_ => "$initrd_mnt/lib/" . basename $_;
	chmod 0755, "$initrd_mnt/lib/" . basename $_;
      }
    };

  step
    descr  => "Copying necessary device files to \"$initrd_mnt/dev\"",
    ensure => sub { -c "$initrd_mnt/dev/console" },
    pause  => 1,
    using  => sub { utime undef, undef, $initrd_img },
    help   => <<HELP;
      Please create a $initrd_mnt/dev/console by entering the following command
      as root:
      # mknod $initrd_mnt/dev/console c 5 1
HELP

  step
    descr  => "Unmounting \"$initrd_img\"",
    ensure => sub { `mount` !~ /\Q$initrd_img/ },
    pause  => 1,
    help   => <<HELP;
      Please umount \"$initrd_img\" by entering the following command as root:
      # umount $initrd_img
HELP

  step
    descr  => "Compressing \"$initrd_img\"",
    ensure => sub { -r $initrd_gz and -M $initrd_gz <= -M $initrd_img },
    using  => sub { system "gzip -vvv -9 -c $initrd_img > $initrd_gz" };
}

step
  descr  => "Creating directory \"$cdroot\"",
  ensure => sub { -d $cdroot and -r $cdroot },
  using  => sub { mkdir $cdroot };

step
  descr  => "Unpacking GRUB in \"$cdroot\"",
  ensure => sub { -r "$cdroot/boot/grub/stage2_eltorito" },
  using  => sub { system "tar", "-xvjf", $grub_local, "-C", $cdroot };

my $wrote_menulst = 0;
step
  descr  => "Creating \"$cdroot/boot/grub/menu.lst\"",
  ensure => sub { -r "$cdroot/boot/grub/menu.lst" and $wrote_menulst },
  using  => sub { open my $fh, ">", "$cdroot/boot/grub/menu.lst"; print $fh <<GRUB; $wrote_menulst++ };
default 0
timeout 0
color light-blue/black black/light-gray
hiddenmenu

title    $pugs_version
root     (cd)
kernel   /boot/vmlinuz root=/dev/ram rw init=/linuxrc ramdisk_size=$initrd_size quiet
initrd   /boot/initrd.gz
clear
cat      /boot/splashscreen.txt
GRUB

for(
  [$splashscreen => "$cdroot/boot/splashscreen.txt"],
  [$initrd_gz    => "$cdroot/boot/initrd.gz"],
  [$kernel_local => "$cdroot/boot/vmlinuz"],
) {
  my ($src, $dest) = @$_;

  step
    descr  => "Copying \"$src\" to \"$dest\"",
    ensure => sub { -r $dest and -M $dest <= -M $src },
    using  => sub { copy $src => $dest };
}

step
  descr  => "Creating final ISO image",
  ensure => sub { -r $iso and -M $iso <= -M $initrd_gz and -M $iso <= -M "$cdroot/boot/grub/menu.lst" },
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

sub get_version {
  my $bin = shift;

  open my $fh, "-|", $bin, "-V:pugs_version" or
    die "Couldn't open pipe to \"$bin -V:pugs_version\": $!\n";
  my $rev = <$fh>;
  $rev =~ s/^.*?: //g;

  return $rev;
}
