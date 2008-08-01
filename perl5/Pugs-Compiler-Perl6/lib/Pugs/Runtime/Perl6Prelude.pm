use v6-alpha;

class Str {};

class Bool { sub False { 0 }; sub True { 1 }};

module main;
enum bool <False True>;

sub true is export {
    $_[0] ?? bool::True !! bool::False
}

sub Inf is export { Pugs::Runtime::Perl6::Inf }
sub NaN is export { Pugs::Runtime::Perl6::NaN }

# sub slurp { ... } # Slurps in an entire file

module Perl6::Internals;

sub compile_pir($code) { ... }
sub exit($status)   is export { use v5; CORE::exit($status); use v6; }
# sub sleep($seconds) is export { use v5; CORE::sleep($seconds); use v6; }

sub open($file, :$w? ) { 
    use v5; 
    my $fh = Pugs::Runtime::Perl6::IO::File->new; 
    $fh->open($file, ($w ? 'w' : ()) ) || warn "can't open file $file"; 
    $fh; 
    use v6; 
}

module Pugs::Internals;

# Defining caller causes a v6 parse failure.
#sub caller(Class $kind = Any, Int $:skip = 0, Str :$label) { ... }
sub compile_file_to_yml($path) { ... }

sub install_pragma_value(Class $class, $value ) { ... }
sub current_pragma_value(Class $class)  { ... }

sub eval_perl6($code) { ... } 
sub eval_haskell($code) { ... }
sub eval_parrot($code) { ... }
sub eval_perl5($code) { ... }
sub eval_yaml($code) { ... }

sub exec($prog, Bool $flag, @args) { ... } # XXX flag undocumented in prelude.
sub hSeek(File $file, Int $position, Int $whence) { ... }
sub hSetBinaryMode($fh, Bool $binary) { ... }
sub localtime(Bool :$want_item, :$sec, :$pico) { ... }
sub openFile($filename, $mode) { ... }
sub require($module) { ... }
sub require_use_helper(Bool $is_use, $module) { ... }
sub runInteractiveCommand($command) { ... }
sub sprintf($conversion, @arg) { ... } #bad signature
sub use($module) { ... }

use Pugs::Runtime::Perl6AST;
# module v6::AST;
#
# $INC{"v6/AST.pm"}=1;  # mark this module as "used"
#
# sub node( $match, $type ) {
#    #say "new node $match $type\n";
#    $match;
# }
