#!/usr/bin/perl

use strict;
use warnings;

unless (@ARGV == 2) {
    print <<"";
Usage:
    package-osx.pl [pugs|parrot|pugs-parrot] target_base_dir
        Run after make install'ing pugs and parrot under /usr/local

    exit;
}

my ($bundle, $dest) = @ARGV;

$dest =~ s#/([^/]+)/?$#/$1#;
my $remover = "/usr/local/bin/remove-$1.pl";

my (%symlinks, %change_files, @files);

if ($bundle =~ /parrot/) {
    @files = qw(
        /usr/local/bin/parrot-config
        /usr/local/bin/pbc_merge
        /usr/local/bin/disassemble
        /usr/local/bin/parrot
        /usr/local/bin/pbc_info
        /usr/local/bin/pbc_merge
        /usr/local/bin/pdb
        /usr/local/bin/pdump
        /usr/local/include/parrot
        /usr/local/lib/libreadline.5.1.dylib
        /usr/local/lib/libhistory.5.1.dylib
        /usr/local/lib/libparrot.0.4.4.dylib
        /usr/local/lib/libparrot.a
        /usr/local/lib/libparrot.dylib
        /usr/local/lib/parrot
        /usr/local/lib/pkgconfig/parrot.pc
        /usr/local/share/doc/parrot
        /usr/local/info/readline.info
        /usr/local/info/rluserman.info
        /usr/local/info/history.info
        /usr/local/man/man3/readline.3
        /usr/local/man/man3/history.3
        /usr/local/lib/libreadline.a
        /usr/local/lib/libhistory.a
    );
}
if ($bundle =~ /pugs/) {

    %symlinks = qw(
        /usr/local/bin/pugs ../../bin/pugs
        /usr/local/bin/pugscc ../../bin/pugscc
        /usr/local/bin/p6doc ../../bin/p6doc
    );

    %change_files = qw(
        /usr/local/lib/perl5/site_perl/jspugs.pl /Library/Perl/5.8.6/jspugs.pl
        /usr/local/lib/perl5/site_perl/PIL.pm /Library/Perl/5.8.6/PIL.pm
        /usr/local/lib/perl5/site_perl/pil2js.pl /Library/Perl/5.8.6/pil2js.pl
        /usr/local/lib/perl5/site_perl/PIL2JS.pm /Library/Perl/5.8.6/PIL2JS.pm
        /usr/local/lib/perl5/site_perl/pugs-smokejs.pl /Library/Perl/5.8.6/pugs-smokejs.pl
        /usr/local/lib/perl5/site_perl/runjs.pl /Library/Perl/5.8.6/runjs.pl
        /usr/local/lib/perl5/site_perl/Class/Rebless.pm /Library/Perl/5.8.6/Class/Rebless.pm
        /usr/local/lib/perl5/site_perl/Perl6 /Library/Perl/5.8.6/Perl6/
        /usr/local/lib/perl5/site_perl/PIL /Library/Perl/5.8.6/PIL
        /usr/local/lib/perl5/site_perl/PIL2JS /Library/Perl/5.8.6/PIL2JS
        /usr/local/lib/perl5/site_perl/Prelude/JS.pm /Library/Perl/5.8.6/Prelude/JS.pm
        /Users/dromano/dev/pugs/examples /usr/local/share/doc/pugs/examples
        /Users/dromano/dev/pugs/docs /usr/local/share/doc/pugs/docs
        /Users/dromano/dev/pugs/LICENSE /usr/local/share/doc/pugs/LICENSE
        /usr/local/usr/bin/p6doc /usr/local/bin/p6doc
        /usr/local/usr/bin/pugs /usr/local/bin/pugs
        /usr/local/usr/bin/pugscc /usr/local/bin/pugscc
        blank /System/Library/Perl6/
        /Users/dromano/dev/pugs/src/pge/run_pge.pir /System/Library/Perl6/darwin-thread-multi-2level/CORE/pugs/pge/run_pge.pir
    );
    unless (grep /pugs-parrot/, @ARGV) { 
        @files = ();
    }

    push @files, qw(
        /usr/local/lib/libreadline.5.1.dylib
        /usr/local/lib/libhistory.5.1.dylib
        /Library/Frameworks/GMP.Framework
        /Library/Perl6
        /Library/Perl/5.8.6/darwin-thread-multi-2level/auto/Perl6/Pugs/.packlist
        /Library/Perl/5.8.6/v6.pm
        /Library/Perl/5.8.6/Inline/Pugs.pm
        /usr/local/info/readline.info
        /usr/local/info/rluserman.info
        /usr/local/info/history.info
        /usr/local/man/man3/readline.3
        /usr/local/man/man3/history.3
        /usr/local/man/man3/Inline::Pugs.3pm
        /usr/local/man/man3/v6.3pm
        /usr/local/man/man3/Perl6::API.3
        /usr/local/man/man3/Perl6::API.3pm
        /usr/local/man/man3/Perl6::Bible.3pm
        /usr/local/man/man3/Perl6::Bible::A01.3pm
        /usr/local/man/man3/Perl6::Bible::A02.3pm
        /usr/local/man/man3/Perl6::Bible::A03.3pm
        /usr/local/man/man3/Perl6::Bible::A04.3pm
        /usr/local/man/man3/Perl6::Bible::A05.3pm
        /usr/local/man/man3/Perl6::Bible::A06.3pm
        /usr/local/man/man3/Perl6::Bible::A12.3pm
        /usr/local/man/man3/Perl6::Bible::A20.3pm
        /usr/local/man/man3/Perl6::Bible::E02.3pm
        /usr/local/man/man3/Perl6::Bible::E03.3pm
        /usr/local/man/man3/Perl6::Bible::E04.3pm
        /usr/local/man/man3/Perl6::Bible::E05.3pm
        /usr/local/man/man3/Perl6::Bible::E06.3pm
        /usr/local/man/man3/Perl6::Bible::E07.3pm
        /usr/local/man/man3/Perl6::Bible::S01.3pm
        /usr/local/man/man3/Perl6::Bible::S02.3pm
        /usr/local/man/man3/Perl6::Bible::S03.3pm
        /usr/local/man/man3/Perl6::Bible::S04.3pm
        /usr/local/man/man3/Perl6::Bible::S05.3pm
        /usr/local/man/man3/Perl6::Bible::S06.3pm
        /usr/local/man/man3/Perl6::Bible::S09.3pm
        /usr/local/man/man3/Perl6::Bible::S10.3pm
        /usr/local/man/man3/Perl6::Bible::S11.3pm
        /usr/local/man/man3/Perl6::Bible::S12.3pm
        /usr/local/man/man3/Perl6::Bible::S13.3pm
        /usr/local/man/man3/Perl6::Bible::S17.3pm
        /usr/local/man/man3/Perl6::Bible::S22.3pm
        /usr/local/man/man3/Perl6::Bible::S26.3pm
        /usr/local/man/man3/Perl6::Bible::S27.3pm
        /usr/local/man/man3/Perl6::Bible::S28.3pm
        /usr/local/man/man3/Perl6::Bible::S29.3pm
        /usr/local/man/man3/Perl6::Doc.3
        /usr/local/man/man3/Perl6::Doc.3pm
        /usr/local/man/man3/Perl6::FAQ::Capture.3
        /usr/local/man/man3/Perl6::FAQ::Capture.3pm
        /usr/local/man/man3/Perl6::FAQ::FUD.3
        /usr/local/man/man3/Perl6::FAQ::FUD.3pm
        /usr/local/man/man3/Perl6::Overview.3
        /usr/local/man/man3/Perl6::Overview.3pm
        /usr/local/man/man3/Perl6::Overview::Control.3
        /usr/local/man/man3/Perl6::Overview::Control.3pm
        /usr/local/man/man3/Perl6::Overview::Data.3
        /usr/local/man/man3/Perl6::Overview::Data.3pm
        /usr/local/man/man3/Perl6::Overview::File.3
        /usr/local/man/man3/Perl6::Overview::File.3pm
        /usr/local/man/man3/Perl6::Overview::Functions.3
        /usr/local/man/man3/Perl6::Overview::Functions.3pm
        /usr/local/man/man3/Perl6::Overview::Object.3
        /usr/local/man/man3/Perl6::Overview::Object.3pm
        /usr/local/man/man3/Perl6::Overview::Operator.3
        /usr/local/man/man3/Perl6::Overview::Operator.3pm
        /usr/local/man/man3/Perl6::Overview::Reduce.3
        /usr/local/man/man3/Perl6::Overview::Reduce.3pm
        /usr/local/man/man3/Perl6::Overview::Rule.3
        /usr/local/man/man3/Perl6::Overview::Rule.3pm
        /usr/local/man/man3/Perl6::Overview::Smartmatch.3
        /usr/local/man/man3/Perl6::Overview::Smartmatch.3pm
        /usr/local/man/man3/Perl6::Overview::Subroutine.3
        /usr/local/man/man3/Perl6::Overview::Subroutine.3pm
        /usr/local/man/man3/Perl6::Overview::Variable.3
        /usr/local/man/man3/Perl6::Overview::Variable.3pm
        /usr/local/man/man3/Perl6::Perl5::Differences.3
        /usr/local/man/man3/Perl6::Perl5::Differences.3pm
        /usr/local/man/man3/Perl6::Perl5::Docfinder.3
        /usr/local/man/man3/Perl6::Perl5::Docfinder.3pm
        /usr/local/man/man3/Perl6::Pugs.3
        /usr/local/man/man3/Perl6::Pugs.3pm
        /usr/local/man/man3/Perl6::Spec.3
        /usr/local/man/man3/Perl6::Spec.3pm
        /usr/local/man/man3/Perl6::Spec::CPAN.3
        /usr/local/man/man3/Perl6::Spec::CPAN.3pm
        /usr/local/man/man3/Perl6::Spec::Concurrency.3
        /usr/local/man/man3/Perl6::Spec::Concurrency.3pm
        /usr/local/man/man3/Perl6::Spec::Documentation.3
        /usr/local/man/man3/Perl6::Spec::Documentation.3pm
        /usr/local/man/man3/Perl6::Spec::Functions.3
        /usr/local/man/man3/Perl6::Spec::Functions.3pm
        /usr/local/man/man3/Perl6::Spec::Block.3pm
        /usr/local/man/man3/Perl6::Spec::Module.3pm
        /usr/local/man/man3/Perl6::Spec::Object.3pm
        /usr/local/man/man3/Perl6::Spec::Operator.3pm
        /usr/local/man/man3/Perl6::Spec::Overload.3pm
        /usr/local/man/man3/Perl6::Spec::Overview.3pm
        /usr/local/man/man3/Perl6::Spec::Package.3pm
        /usr/local/man/man3/Perl6::Spec::Rule.3pm
        /usr/local/man/man3/Perl6::Spec::Structure.3pm
        /usr/local/man/man3/Perl6::Spec::Subroutine.3pm
        /usr/local/man/man3/Perl6::Spec::Syntax.3pm
        /usr/local/man/man3/Perl6::Tutorial.3
        /usr/local/man/man3/Perl6::Tutorial.3pm
        /usr/local/man/man3/Pugs::Doc::Hack.3
        /usr/local/man/man3/Pugs::Doc::Hack.3pm
        /usr/local/man/man3/Pugs::Doc::Run.3
        /usr/local/man/man3/Pugs::Doc::Run.3pm

    );
}

sub change_files {
    my ($dest, %change_files) = @_;
    for my $orig (keys %change_files) {
        my $dirs = $change_files{$orig};
        $dirs =~ s#/[^/]+$##;
        make_dir("$dest$dirs") unless (-d "$dest$dirs");
    
        qx/cp -r $orig $dest$change_files{$orig}/;
        remove_svn("$dest$dirs");
        print "Copied $orig to $dest$change_files{$orig}\n";
    }
}

sub make_symlinks {
    my ($dest, %symlinks) = @_;
    make_dir("${dest}/usr/bin") unless (-d "${dest}/usr/bin");
    chdir("${dest}/usr/local/bin");
    qx/ln -s $_ $symlinks{$_}/ for (keys %symlinks);
}


sub make_dir {
    my ($dir) = @_;
    my $curr_dir = "";
    for (split m#/#, $dir) {
        $curr_dir .= "$_/";
        if (!-d $curr_dir) {
            mkdir $curr_dir;
            print "Made $curr_dir\n";
        }
    }
}

sub remove_svn {
    my ($dir) = @_;
    print "Removing .svn under $dir\n";
    if (-e "$dir/.svn") {
        print `rm -rf $dir/.svn`;
        print "Removed $dir/.svn\n";
    }
    for my $sub_dir (grep -d, <$dir/*>) {
        remove_svn("$sub_dir");
    }
}    
    
sub copy_files {
    my ($dest, @files) = @_;
    for my $from (@files) {
        my $dirs = $from;
        $dirs =~ s#/[^/]+$##;
        make_dir("$dest$dirs") unless (-d "$dest$dirs");

        if ($_ = qx/cp -r $from $dest$from 2>&1/) {
            print "Error copying $from to $dest$from: $_\n";
        }
        else {
            print "Copied $from to $dest$from\n";
        }
    }
}


# @files are all files that will be removed. The last one passed in is the
# remover that will be installed on the target machine (hence the $file[-1]).
sub create_remover {
    my ($dest, @files) = @_;
    my $dirs = $files[-1];
    $dirs =~ s#/[^/]+$##;
    make_dir($dirs) unless (-d $dirs);
    
    open  REMOVER, '>', "$dest$files[-1]" or die "Can't open $dest$files[-1]!";

    my @header = qq(
        #!/usr/bin/perl
        # $files[-1]
        use strict;
        use warnings;\n
        chdir '/usr/local/bin';\n
        my \@files = qw\(
    );
    @header = map { s/^(?:\s{9}|\s{5}$)//; $_ } split /$/m, $header[0];

    my @footer = qq(
        \ 
        \);\n
        for my \$file \(\@files\) \{
            if \( `which parrot` !~ /^no / and \$file =~ /\(?:parrot\).*\\.\(?:dylib|3|info\)\$/ \) \{
                print "Not removing \$file\\n"; 
                next;
            \}
            if \(\$file =~ /\(?:readline|history\).*\\.\(?:dylib|3|info\)\$/ \) \{
                print "Not removing \$file\\n"; 
                next;
            \}
            print "Removing \$file\\n"\;
            print "Removing \$file\\n"\;
            if \( \$_ \= qx\/rm \-rf \$file 2>&1 \/ \) \{
                print "Problem removing \$file\: \$_\\n";
            \}
            else \{
                print "Successfully removed \$file\\n";
            \}
        \}
    );
    @footer = map { s/^(?:\s{9}|\s{5}$)//; $_ } split /$/m, $footer[0];

    print REMOVER "$_\n" for @header;
    print REMOVER "    $_\n" for @files; 
    print REMOVER "$_\n" for @footer;

    close REMOVER;
    chmod 0755, "$dest$files[-1]";
    print "Made $dest$files[-1]\n";
}

sub chown_all {
    my ($dest) = @_;
    system("sudo -S chown -R root:wheel $dest < ~/.pass");
}

&change_files($dest, %change_files) if (keys %change_files > 0);
&copy_files($dest, @files);
&make_symlinks($dest, %symlinks) if (keys %symlinks > 0);
&create_remover($dest, values %change_files, @files, %symlinks, $remover);
&chown_all($dest);
