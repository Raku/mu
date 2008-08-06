# TODO:
#
package Test::Base;
use 5.006001;
use Spiffy 0.30 -Base;
use Spiffy ':XXX';
our $VERSION = '0.54';

my @test_more_exports;
BEGIN {
    @test_more_exports = qw(
        ok isnt like unlike is_deeply cmp_ok
        skip todo_skip pass fail
        eq_array eq_hash eq_set
        plan can_ok isa_ok diag
        use_ok
        $TODO
    );
}

use Test::More import => \@test_more_exports;
use Carp;

our @EXPORT = (@test_more_exports, qw(
    is no_diff

    blocks next_block first_block
    delimiters spec_file spec_string 
    filters filters_delay filter_arguments
    run run_compare run_is run_is_deeply run_like run_unlike 
    WWW XXX YYY ZZZ
    tie_output no_diag_on_only

    find_my_self default_object

    croak carp cluck confess
));

field '_spec_file';
field '_spec_string';
field _filters => [qw(norm trim)];
field _filters_map => {};
field spec =>
      -init => '$self->_spec_init';
field block_list =>
      -init => '$self->_block_list_init';
field _next_list => [];
field block_delim =>
      -init => '$self->block_delim_default';
field data_delim =>
      -init => '$self->data_delim_default';
field _filters_delay => 0;
field _no_diag_on_only => 0;

field block_delim_default => '===';
field data_delim_default => '---';

my $default_class;
my $default_object;
my $reserved_section_names = {};

sub default_object { 
    $default_object ||= $default_class->new;
    return $default_object;
}

my $import_called = 0;
sub import() {
    $import_called = 1;
    my $class = (grep /^-base$/i, @_) 
    ? scalar(caller)
    : $_[0];
    if (not defined $default_class) {
        $default_class = $class;
    }
#     else {
#         croak "Can't use $class after using $default_class"
#           unless $default_class->isa($class);
#     }

    unless (grep /^-base$/i, @_) {
        my @args;
        for (my $ii = 1; $ii <= $#_; ++$ii) {
            if ($_[$ii] eq '-package') {
                ++$ii;
            } else {
                push @args, $_[$ii];
            }
        }
        Test::More->import(import => \@test_more_exports, @args)
            if @args;
     }
    
    _strict_warnings();
    goto &Spiffy::import;
}

# Wrap Test::Builder::plan
my $plan_code = \&Test::Builder::plan;
my $Have_Plan = 0;
{
    no warnings 'redefine';
    *Test::Builder::plan = sub {
        $Have_Plan = 1;
        goto &$plan_code;
    };
}

my $DIED = 0;
$SIG{__DIE__} = sub { $DIED = 1; die @_ };

sub block_class  { $self->find_class('Block') }
sub filter_class { $self->find_class('Filter') }

sub find_class {
    my $suffix = shift;
    my $class = ref($self) . "::$suffix";
    return $class if $class->can('new');
    $class = __PACKAGE__ . "::$suffix";
    return $class if $class->can('new');
    eval "require $class";
    return $class if $class->can('new');
    die "Can't find a class for $suffix";
}

sub check_late {
    if ($self->{block_list}) {
        my $caller = (caller(1))[3];
        $caller =~ s/.*:://;
        croak "Too late to call $caller()"
    }
}

sub find_my_self() {
    my $self = ref($_[0]) eq $default_class
    ? splice(@_, 0, 1)
    : default_object();
    return $self, @_;
}

sub blocks() {
    (my ($self), @_) = find_my_self(@_);

    croak "Invalid arguments passed to 'blocks'"
      if @_ > 1;
    croak sprintf("'%s' is invalid argument to blocks()", shift(@_))
      if @_ && $_[0] !~ /^[a-zA-Z]\w*$/;

    my $blocks = $self->block_list;
    
    my $section_name = shift || '';
    my @blocks = $section_name
    ? (grep { exists $_->{$section_name} } @$blocks)
    : (@$blocks);

    return scalar(@blocks) unless wantarray;
    
    return (@blocks) if $self->_filters_delay;

    for my $block (@blocks) {
        $block->run_filters
          unless $block->is_filtered;
    }

    return (@blocks);
}

sub next_block() {
    (my ($self), @_) = find_my_self(@_);
    my $list = $self->_next_list;
    if (@$list == 0) {
        $list = [@{$self->block_list}, undef];
        $self->_next_list($list);
    }
    my $block = shift @$list;
    if (defined $block and not $block->is_filtered) {
        $block->run_filters;
    }
    return $block;
}

sub first_block() {
    (my ($self), @_) = find_my_self(@_);
    $self->_next_list([]);
    $self->next_block;
}

sub filters_delay() {
    (my ($self), @_) = find_my_self(@_);
    $self->_filters_delay(defined $_[0] ? shift : 1);
}

sub no_diag_on_only() {
    (my ($self), @_) = find_my_self(@_);
    $self->_no_diag_on_only(defined $_[0] ? shift : 1);
}

sub delimiters() {
    (my ($self), @_) = find_my_self(@_);
    $self->check_late;
    my ($block_delimiter, $data_delimiter) = @_;
    $block_delimiter ||= $self->block_delim_default;
    $data_delimiter ||= $self->data_delim_default;
    $self->block_delim($block_delimiter);
    $self->data_delim($data_delimiter);
    return $self;
}

sub spec_file() {
    (my ($self), @_) = find_my_self(@_);
    $self->check_late;
    $self->_spec_file(shift);
    return $self;
}

sub spec_string() {
    (my ($self), @_) = find_my_self(@_);
    $self->check_late;
    $self->_spec_string(shift);
    return $self;
}

sub filters() {
    (my ($self), @_) = find_my_self(@_);
    if (ref($_[0]) eq 'HASH') {
        $self->_filters_map(shift);
    }
    else {    
        my $filters = $self->_filters;
        push @$filters, @_;
    }
    return $self;
}

sub filter_arguments() {
    $Test::Base::Filter::arguments;
}

sub have_text_diff {
    eval { require Text::Diff; 1 } &&
        $Text::Diff::VERSION >= 0.35 &&
        $Algorithm::Diff::VERSION >= 1.15;
}

sub is($$;$) {
    (my ($self), @_) = find_my_self(@_);
    my ($actual, $expected, $name) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    if ($ENV{TEST_SHOW_NO_DIFFS} or
         not defined $actual or
         not defined $expected or
         $actual eq $expected or 
         not($self->have_text_diff) or 
         $expected !~ /\n./s
    ) {
        Test::More::is($actual, $expected, $name);
    }
    else {
        $name = '' unless defined $name;
        ok $actual eq $expected,
           $name . "\n" . Text::Diff::diff(\$expected, \$actual);
    }
}

sub run(&;$) {
    (my ($self), @_) = find_my_self(@_);
    my $callback = shift;
    for my $block (@{$self->block_list}) {
        $block->run_filters unless $block->is_filtered;
        &{$callback}($block);
    }
}

my $name_error = "Can't determine section names";
sub _section_names {
    return @_ if @_ == 2;
    my $block = $self->first_block
      or croak $name_error;
    my @names = grep {
        $_ !~ /^(ONLY|LAST|SKIP)$/;
    } @{$block->{_section_order}[0] || []};
    croak "$name_error. Need two sections in first block"
      unless @names == 2;
    return @names;
}

sub _assert_plan {
    plan('no_plan') unless $Have_Plan;
}

sub END {
    run_compare() unless $Have_Plan or $DIED or not $import_called;
}

sub run_compare() {
    (my ($self), @_) = find_my_self(@_);
    $self->_assert_plan;
    my ($x, $y) = $self->_section_names(@_);
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    for my $block (@{$self->block_list}) {
        next unless exists($block->{$x}) and exists($block->{$y});
        $block->run_filters unless $block->is_filtered;
        if (ref $block->$x) {
            is_deeply($block->$x, $block->$y,
                $block->name ? $block->name : ());
        }
        elsif (ref $block->$y eq 'Regexp') {
            my $regexp = ref $y ? $y : $block->$y;
            like($block->$x, $regexp, $block->name ? $block->name : ());
        }
        else {
            is($block->$x, $block->$y, $block->name ? $block->name : ());
        }
    }
}

sub run_is() {
    (my ($self), @_) = find_my_self(@_);
    $self->_assert_plan;
    my ($x, $y) = $self->_section_names(@_);
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    for my $block (@{$self->block_list}) {
        next unless exists($block->{$x}) and exists($block->{$y});
        $block->run_filters unless $block->is_filtered;
        is($block->$x, $block->$y, 
           $block->name ? $block->name : ()
          );
    }
}

sub run_is_deeply() {
    (my ($self), @_) = find_my_self(@_);
    $self->_assert_plan;
    my ($x, $y) = $self->_section_names(@_);
    for my $block (@{$self->block_list}) {
        next unless exists($block->{$x}) and exists($block->{$y});
        $block->run_filters unless $block->is_filtered;
        is_deeply($block->$x, $block->$y, 
           $block->name ? $block->name : ()
          );
    }
}

sub run_like() {
    (my ($self), @_) = find_my_self(@_);
    $self->_assert_plan;
    my ($x, $y) = $self->_section_names(@_);
    for my $block (@{$self->block_list}) {
        next unless exists($block->{$x}) and defined($y);
        $block->run_filters unless $block->is_filtered;
        my $regexp = ref $y ? $y : $block->$y;
        like($block->$x, $regexp,
             $block->name ? $block->name : ()
            );
    }
}

sub run_unlike() {
    (my ($self), @_) = find_my_self(@_);
    $self->_assert_plan;
    my ($x, $y) = $self->_section_names(@_);
    for my $block (@{$self->block_list}) {
        next unless exists($block->{$x}) and defined($y);
        $block->run_filters unless $block->is_filtered;
        my $regexp = ref $y ? $y : $block->$y;
        unlike($block->$x, $regexp,
               $block->name ? $block->name : ()
              );
    }
}

sub _pre_eval {
    my $spec = shift;
    return $spec unless $spec =~
      s/\A\s*<<<(.*?)>>>\s*$//sm;
    my $eval_code = $1;
    eval "package main; $eval_code";
    croak $@ if $@;
    return $spec;
}

sub _block_list_init {
    my $spec = $self->spec;
    $spec = $self->_pre_eval($spec);
    my $cd = $self->block_delim;
    my @hunks = ($spec =~ /^(\Q${cd}\E.*?(?=^\Q${cd}\E|\z))/msg);
    my $blocks = $self->_choose_blocks(@hunks);
    $self->block_list($blocks); # Need to set early for possible filter use
    my $seq = 1;
    for my $block (@$blocks) {
        $block->blocks_object($self);
        $block->seq_num($seq++);
    }
    return $blocks;
}

sub _choose_blocks {
    my $blocks = [];
    for my $hunk (@_) {
        my $block = $self->_make_block($hunk);
        if (exists $block->{ONLY}) {
            diag "I found ONLY: maybe you're debugging?"
                unless $self->_no_diag_on_only;
            return [$block];
        }
        next if exists $block->{SKIP};
        push @$blocks, $block;
        if (exists $block->{LAST}) {
            return $blocks;
        }
    }
    return $blocks;
}

sub _check_reserved {
    my $id = shift;
    croak "'$id' is a reserved name. Use something else.\n"
      if $reserved_section_names->{$id} or
         $id =~ /^_/;
}

sub _make_block {
    my $hunk = shift;
    my $cd = $self->block_delim;
    my $dd = $self->data_delim;
    my $block = $self->block_class->new;
    $hunk =~ s/\A\Q${cd}\E[ \t]*(.*)\s+// or die;
    my $name = $1;
    my @parts = split /^\Q${dd}\E +\(?(\w+)\)? *(.*)?\n/m, $hunk;
    my $description = shift @parts;
    $description ||= '';
    unless ($description =~ /\S/) {
        $description = $name;
    }
    $description =~ s/\s*\z//;
    $block->set_value(description => $description);
    
    my $section_map = {};
    my $section_order = [];
    while (@parts) {
        my ($type, $filters, $value) = splice(@parts, 0, 3);
        $self->_check_reserved($type);
        $value = '' unless defined $value;
        $filters = '' unless defined $filters;
        if ($filters =~ /:(\s|\z)/) {
            croak "Extra lines not allowed in '$type' section"
              if $value =~ /\S/;
            ($filters, $value) = split /\s*:(?:\s+|\z)/, $filters, 2;
            $value = '' unless defined $value;
            $value =~ s/^\s*(.*?)\s*$/$1/;
        }
        $section_map->{$type} = {
            filters => $filters,
        };
        push @$section_order, $type;
        $block->set_value($type, $value);
    }
    $block->set_value(name => $name);
    $block->set_value(_section_map => $section_map);
    $block->set_value(_section_order => $section_order);
    return $block;
}

sub _spec_init {
    return $self->_spec_string
      if $self->_spec_string;
    local $/;
    my $spec;
    if (my $spec_file = $self->_spec_file) {
        open FILE, $spec_file or die $!;
        $spec = <FILE>;
        close FILE;
    }
    else {    
        $spec = do { 
            package main; 
            no warnings 'once';
            <DATA>;
        };
    }
    return $spec;
}

sub _strict_warnings() {
    require Filter::Util::Call;
    my $done = 0;
    Filter::Util::Call::filter_add(
        sub {
            return 0 if $done;
            my ($data, $end) = ('', '');
            while (my $status = Filter::Util::Call::filter_read()) {
                return $status if $status < 0;
                if (/^__(?:END|DATA)__\r?$/) {
                    $end = $_;
                    last;
                }
                $data .= $_;
                $_ = '';
            }
            $_ = "use strict;use warnings;$data$end";
            $done = 1;
        }
    );
}

sub tie_output() {
    my $handle = shift;
    die "No buffer to tie" unless @_;
    tie $handle, 'Test::Base::Handle', $_[0];
}

sub no_diff {
    $ENV{TEST_SHOW_NO_DIFFS} = 1;
}

package Test::Base::Handle;

sub TIEHANDLE() {
    my $class = shift;
    bless \ $_[0], $class;
}

sub PRINT {
    $$self .= $_ for @_;
}

#===============================================================================
# Test::Base::Block
#
# This is the default class for accessing a Test::Base block object.
#===============================================================================
package Test::Base::Block;
our @ISA = qw(Spiffy);

our @EXPORT = qw(block_accessor);

sub AUTOLOAD {
    return;
}

sub block_accessor() {
    my $accessor = shift;
    no strict 'refs';
    return if defined &$accessor;
    *$accessor = sub {
        my $self = shift;
        if (@_) {
            Carp::croak "Not allowed to set values for '$accessor'";
        }
        my @list = @{$self->{$accessor} || []};
        return wantarray
        ? (@list)
        : $list[0];
    };
}

block_accessor 'name';
block_accessor 'description';
Spiffy::field 'seq_num';
Spiffy::field 'is_filtered';
Spiffy::field 'blocks_object';
Spiffy::field 'original_values' => {};

sub set_value {
    no strict 'refs';
    my $accessor = shift;
    block_accessor $accessor
      unless defined &$accessor;
    $self->{$accessor} = [@_];
}

sub run_filters {
    my $map = $self->_section_map;
    my $order = $self->_section_order;
    Carp::croak "Attempt to filter a block twice"
      if $self->is_filtered;
    for my $type (@$order) {
        my $filters = $map->{$type}{filters};
        my @value = $self->$type;
        $self->original_values->{$type} = $value[0];
        for my $filter ($self->_get_filters($type, $filters)) {
            $Test::Base::Filter::arguments =
              $filter =~ s/=(.*)$// ? $1 : undef;
            my $function = "main::$filter";
            no strict 'refs';
            if (defined &$function) {
                local $_ = join '', @value;
                my $old = $_;
                @value = &$function(@value);
                if (not(@value) or 
                    @value == 1 and $value[0] =~ /\A(\d+|)\z/
                ) {
                    if ($value[0] && $_ eq $old) {
                        Test::Base::diag("Filters returning numbers are supposed to do munging \$_: your filter '$function' apparently doesn't.");
                    }
                    @value = ($_);
                }
            }
            else {
                my $filter_object = $self->blocks_object->filter_class->new;
                die "Can't find a function or method for '$filter' filter\n"
                  unless $filter_object->can($filter);
                $filter_object->current_block($self);
                @value = $filter_object->$filter(@value);
            }
            # Set the value after each filter since other filters may be
            # introspecting.
            $self->set_value($type, @value);
        }
    }
    $self->is_filtered(1);
}

sub _get_filters {
    my $type = shift;
    my $string = shift || '';
    $string =~ s/\s*(.*?)\s*/$1/;
    my @filters = ();
    my $map_filters = $self->blocks_object->_filters_map->{$type} || [];
    $map_filters = [ $map_filters ] unless ref $map_filters;
    my @append = ();
    for (
        @{$self->blocks_object->_filters}, 
        @$map_filters,
        split(/\s+/, $string),
    ) {
        my $filter = $_;
        last unless length $filter;
        if ($filter =~ s/^-//) {
            @filters = grep { $_ ne $filter } @filters;
        }
        elsif ($filter =~ s/^\+//) {
            push @append, $filter;
        }
        else {
            push @filters, $filter;
        }
    }
    return @filters, @append;
}

{
    %$reserved_section_names = map {
        ($_, 1);
    } keys(%Test::Base::Block::), qw( new DESTROY );
}

__DATA__

=head1 NAME

Test::Base - A Data Driven Testing Framework

=head1 SYNOPSIS

A new test module:

    # lib/MyProject/Test.pm
    package MyProject::Test;
    use Test::Base -Base;
    
    use MyProject;
    
    package MyProject::Test::Filter;
    use Test::Base::Filter -base;

    sub my_filter {
        return MyProject->do_something(shift);
    }

A sample test:    
    
    # t/sample.t
    use MyProject::Test;
    
    plan tests => 1 * blocks;
    
    run_is input => 'expected';

    sub local_filter {
        s/my/your/;
    }
    
    __END__
    
    === Test one (the name of the test)
    --- input my_filter local_filter
    my
    input
    lines
    --- expected
    expected
    output
    
    === Test two
    This is an optional description
    of this particular test.
    --- input my_filter
    other
    input
    lines
    --- expected
    other expected
    output

=head1 DESCRIPTION

Testing is usually the ugly part of Perl module authoring. Perl gives
you a standard way to run tests with Test::Harness, and basic testing
primitives with Test::More. After that you are pretty much on your own
to develop a testing framework and philosophy. Test::More encourages
you to make your own framework by subclassing Test::Builder, but that is
not trivial.

Test::Base gives you a way to write your own test framework base
class that I<is> trivial. In fact it is as simple as two lines:

    package MyTestFramework;
    use Test::Base -Base;

A module called C<MyTestFramework.pm> containing those two lines, will
give all the power of Test::More and all the power of Test::Base to
every test file that uses it. As you build up the capabilities of
C<MyTestFramework>, your tests will have all of that power as well.

C<MyTestFramework> becomes a place for you to put all of your reusable
testing bits. As you write tests, you will see patterns and duplication,
and you can "upstream" them into C<MyTestFramework>. Of course, you
don't have to subclass Test::Base at all. You can use it directly in
many applications, including everywhere you would use Test::More.

Test::Base concentrates on offering reusable data driven patterns, so
that you can write tests with a minimum of code. At the heart of all
testing you have inputs, processes and expected outputs. Test::Base
provides some clean ways for you to express your input and expected
output data, so you can spend your time focusing on that rather than
your code scaffolding.

=head1 EXPORTED FUNCTIONS

Test::Base extends Test::More and exports all of its functions. So you
can basically write your tests the same as Test::More. Test::Base
also exports many functions of its own:

=head2 is(actual, expected, [test-name])

This is the equivalent of Test::More's C<is> function with one
interesting twist. If your actual and expected results differ and the
output is multi-line, this function will show you a unified diff format
of output. Consider the benefit when looking for the one character that
is different in hundreds of lines of output!

Diff output requires the optional C<Text::Diff> CPAN module. If you
don't have this module, the C<is()> function will simply give you normal
Test::More output. To disable diffing altogether, set the
C<TEST_SHOW_NO_DIFFS> environment variable (or C<$ENV{TEST_SHOW_NO_DIFFS}>)
to a true value. You can also call the C<no_diff> function as a shortcut.

=head2 blocks( [data-section-name] )

The most important function is C<blocks>. In list context it returns a
list of C<Test::Base::Block> objects that are generated from the test
specification in the C<DATA> section of your test file. In scalar
context it returns the number of objects. This is useful to calculate
your Test::More plan.

Each Test::Base::Block object has methods that correspond to the names
of that object's data sections. There is also a C<name> and a
C<description> method for accessing those parts of the block if they
were specified.

The C<blocks> function can take an optional single argument, that
indicates to only return the blocks that contain a particular named data
section. Otherwise C<blocks> returns all blocks.

    my @all_of_my_blocks = blocks;

    my @just_the_foo_blocks = blocks('foo');

=head2 next_block()

You can use the next_block function to iterate over all the blocks.

    while (my $block = next_block) {
        ...
    }

It returns undef after all blocks have been iterated over. It can then
be called again to reiterate.

=head2 first_block()

Returns the first block or undef if there are none. It resets the iterator to
the C<next_block> function.

=head2 run(&subroutine)

There are many ways to write your tests. You can reference each block
individually or you can loop over all the blocks and perform a common
operation. The C<run> function does the looping for you, so all you need
to do is pass it a code block to execute for each block.

The C<run> function takes a subroutine as an argument, and calls the sub
one time for each block in the specification. It passes the current
block object to the subroutine.

    run {
        my $block = shift;
        is(process($block->foo), $block->bar, $block->name);
    };

=head2 run_is([data_name1, data_name2])

Many times you simply want to see if two data sections are equivalent in
every block, probably after having been run through one or more filters.
With the C<run_is> function, you can just pass the names of any two data
sections that exist in every block, and it will loop over every block
comparing the two sections.

    run_is 'foo', 'bar';

If no data sections are given C<run_is> will try to detect them
automatically.

NOTE: Test::Base will silently ignore any blocks that don't contain
both sections.

=head2 run_is_deeply([data_name1, data_name2])

Like C<run_is> but uses C<is_deeply> for complex data structure comparison.

=head2 run_like([data_name, regexp | data_name]);

The C<run_like> function is similar to C<run_is> except the second
argument is a regular expression. The regexp can either be a C<qr{}>
object or a data section that has been filtered into a regular
expression.

    run_like 'foo', qr{<html.*};
    run_like 'foo', 'match';

=head2 run_unlike([data_name, regexp | data_name]);

The C<run_unlike> function is similar to C<run_like>, except the opposite.

    run_unlike 'foo', qr{<html.*};
    run_unlike 'foo', 'no_match';

=head2 run_compare(data_name1, data_name2)

The C<run_compare> function is like the C<run_is>, C<run_is_deeply> and
the C<run_like> functions all rolled into one. It loops over each
relevant block and determines what type of comparison to do.

NOTE: If you do not specify either a plan, or run any tests, the
C<run_compare> function will automatically be run.

=head2 delimiters($block_delimiter, $data_delimiter)

Override the default delimiters of C<===> and C<--->.

=head2 spec_file($file_name)

By default, Test::Base reads its input from the DATA section. This
function tells it to get the spec from a file instead.

=head2 spec_string($test_data)

By default, Test::Base reads its input from the DATA section. This
function tells it to get the spec from a string that has been
prepared somehow.

=head2 filters( @filters_list or $filters_hashref )

Specify a list of additional filters to be applied to all blocks. See
L<FILTERS> below.

You can also specify a hash ref that maps data section names to an array
ref of filters for that data type.

    filters {
        xxx => [qw(chomp lines)],
        yyy => ['yaml'],
        zzz => 'eval',
    };

If a filters list has only one element, the array ref is optional.

=head2 filters_delay( [1 | 0] );

By default Test::Base::Block objects are have all their filters run
ahead of time. There are testing situations in which it is advantageous
to delay the filtering. Calling this function with no arguments or a
true value, causes the filtering to be delayed.

    use Test::Base;
    filters_delay;
    plan tests => 1 * blocks;
    for my $block (blocks) {
        ...
        $block->run_filters;
        ok($block->is_filtered);
        ...
    }

In the code above, the filters are called manually, using the
C<run_filters> method of Test::Base::Block. In functions like
C<run_is>, where the tests are run automatically, filtering is delayed
until right before the test.

=head2 filter_arguments()

Return the arguments after the equals sign on a filter.

    sub my_filter {
        my $args = filter_arguments;
        # is($args, 'whazzup');
        ...
    }

    __DATA__
    === A test
    --- data my_filter=whazzup

=head2 tie_output()

You can capture STDOUT and STDERR for operations with this function:

    my $out = '';
    tie_output(*STDOUT, $buffer);
    print "Hey!\n";
    print "Che!\n";
    untie *STDOUT;
    is($out, "Hey!\nChe!\n");

=head2 no_diff()

Turn off diff support for is() in a test file.

=head2 default_object()

Returns the default Test::Base object. This is useful if you feel
the need to do an OO operation in otherwise functional test code. See
L<OO> below.

=head2 WWW() XXX() YYY() ZZZ()

These debugging functions are exported from the Spiffy.pm module. See
L<Spiffy> for more info.

=head2 croak() carp() cluck() confess()

You can use the functions from the Carp module without needing to import
them. Test::Base does it for you by default.

=head1 TEST SPECIFICATION

Test::Base allows you to specify your test data in an external file,
the DATA section of your program or from a scalar variable containing
all the text input.

A I<test specification> is a series of text lines. Each test (or block)
is separated by a line containing the block delimiter and an optional
test C<name>. Each block is further subdivided into named sections with
a line containing the data delimiter and the data section name. A
C<description> of the test can go on lines after the block delimiter but
before the first data section.

Here is the basic layout of a specification:

    === <block name 1>
    <optional block description lines>
    --- <data section name 1> <filter-1> <filter-2> <filter-n>
    <test data lines>
    --- <data section name 2> <filter-1> <filter-2> <filter-n>
    <test data lines>
    --- <data section name n> <filter-1> <filter-2> <filter-n>
    <test data lines>

    === <block name 2>
    <optional block description lines>
    --- <data section name 1> <filter-1> <filter-2> <filter-n>
    <test data lines>
    --- <data section name 2> <filter-1> <filter-2> <filter-n>
    <test data lines>
    --- <data section name n> <filter-1> <filter-2> <filter-n>
    <test data lines>

Here is a code example:

    use Test::Base;
    
    delimiters qw(### :::);

    # test code here

    __END__
    
    ### Test One
    We want to see if foo and bar
    are really the same... 
    ::: foo
    a foo line
    another foo line

    ::: bar
    a bar line
    another bar line

    ### Test Two
    
    ::: foo
    some foo line
    some other foo line
    
    ::: bar
    some bar line
    some other bar line

    ::: baz
    some baz line
    some other baz line

This example specifies two blocks. They both have foo and bar data
sections. The second block has a baz component. The block delimiter is
C<###> and the data delimiter is C<:::>.

The default block delimiter is C<===> and the default data delimiter
is C<--->.

There are some special data section names used for control purposes:

    --- SKIP
    --- ONLY
    --- LAST

A block with a SKIP section causes that test to be ignored. This is
useful to disable a test temporarily.

A block with an ONLY section causes only that block to be used. This is
useful when you are concentrating on getting a single test to pass. If
there is more than one block with ONLY, the first one will be chosen.

Because ONLY is very useful for debugging and sometimes you forgot to
remove the ONLY flag before commiting to the VCS or uploading to CPAN,
Test::Base by default gives you a diag message saying I<I found ONLY
... maybe you're debugging?>. If you don't like it, use
C<no_diag_on_only>.

A block with a LAST section makes that block the last one in the
specification. All following blocks will be ignored.

=head1 FILTERS

The real power in writing tests with Test::Base comes from its
filtering capabilities. Test::Base comes with an ever growing set
of useful generic filters than you can sequence and apply to various
test blocks. That means you can specify the block serialization in
the most readable format you can find, and let the filters translate
it into what you really need for a test. It is easy to write your own
filters as well.

Test::Base allows you to specify a list of filters to each data
section of each block. The default filters are C<norm> and C<trim>.
These filters will be applied (in order) to the data after it has been
parsed from the specification and before it is set into its
Test::Base::Block object.

You can add to the default filter list with the C<filters> function. You
can specify additional filters to a specific block by listing them after
the section name on a data section delimiter line.

Example:

    use Test::Base;

    filters qw(foo bar);
    filters { perl => 'strict' };

    sub upper { uc(shift) }

    __END__

    === Test one
    --- foo trim chomp upper
    ...

    --- bar -norm
    ...

    --- perl eval dumper
    my @foo = map {
        - $_;
    } 1..10;
    \ @foo;

Putting a C<-> before a filter on a delimiter line, disables that
filter.

=head2 Scalar vs List

Each filter can take either a scalar or a list as input, and will return
either a scalar or a list. Since filters are chained together, it is
important to learn which filters expect which kind of input and return
which kind of output.

For example, consider the following filter list:

    norm trim lines chomp array dumper eval

The data always starts out as a single scalar string. C<norm> takes a
scalar and returns a scalar. C<trim> takes a list and returns a list,
but a scalar is a valid list. C<lines> takes a scalar and returns a
list. C<chomp> takes a list and returns a list. C<array> takes a list
and returns a scalar (an anonymous array reference containing the list
elements). C<dumper> takes a list and returns a scalar. C<eval> takes a
scalar and creates a list.

A list of exactly one element works fine as input to a filter requiring
a scalar, but any other list will cause an exception. A scalar in list
context is considered a list of one element.

Data accessor methods for blocks will return a list of values when used
in list context, and the first element of the list in scalar context.
This is usually "the right thing", but be aware.

=head2 The Stock Filters

Test::Base comes with large set of stock filters. They are in the
C<Test::Base::Filter> module. See L<Test::Base::Filter> for a listing and
description of these filters.

=head2 Rolling Your Own Filters

Creating filter extensions is very simple. You can either write a
I<function> in the C<main> namespace, or a I<method> in the
C<Test::Base::Filter> namespace or a subclass of it. In either case the
text and any extra arguments are passed in and you return whatever you
want the new value to be.

Here is a self explanatory example:

    use Test::Base;

    filters 'foo', 'bar=xyz';

    sub foo {
        transform(shift);
    }
        
    sub Test::Base::Filter::bar {
        my $self = shift;       # The Test::Base::Filter object
        my $data = shift;
        my $args = $self->current_arguments;
        my $current_block_object = $self->block;
        # transform $data in a barish manner
        return $data;
    }

If you use the method interface for a filter, you can access the block
internals by calling the C<block> method on the filter object.

Normally you'll probably just use the functional interface, although all
the builtin filters are methods.

Note that filters defined in the C<main> namespace can look like:

  sub filter9 {
      s/foo/bar/;
  }

since Test::Base automatically munges the input string into $_
variable and checks the return value of the function to see if it
looks like a number. If you must define a filter that returns just a
single number, do it in a different namespace as a method. These
filters don't allow the simplistic $_ munging.

=head1 OO

Test::Base has a nice functional interface for simple usage. Under the
hood everything is object oriented. A default Test::Base object is
created and all the functions are really just method calls on it.

This means if you need to get fancy, you can use all the object
oriented stuff too. Just create new Test::Base objects and use the
functions as methods.

    use Test::Base;
    my $blocks1 = Test::Base->new;
    my $blocks2 = Test::Base->new;

    $blocks1->delimiters(qw(!!! @@@))->spec_file('test1.txt');
    $blocks2->delimiters(qw(### $$$))->spec_string($test_data);

    plan tests => $blocks1->blocks + $blocks2->blocks;

    # ... etc

=head1 THE C<Test::Base::Block> CLASS

In Test::Base, blocks are exposed as Test::Base::Block objects. This
section lists the methods that can be called on a Test::Base::Block
object. Of course, each data section name is also available as a method.

=head2 name()

This is the optional short description of a block, that is specified on the
block separator line.

=head2 description()

This is an optional long description of the block. It is the text taken from
between the block separator and the first data section.

=head2 seq_num()

Returns a sequence number for this block. Sequence numbers begin with 1. 

=head2 blocks_object()

Returns the Test::Base object that owns this block.

=head2 run_filters()

Run the filters on the data sections of the blocks. You don't need to
use this method unless you also used the C<filters_delay> function.

=head2 is_filtered()

Returns true if filters have already been run for this block.

=head2 original_values()

Returns a hash of the original, unfiltered values of each data section.

=head1 SUBCLASSING

One of the nicest things about Test::Base is that it is easy to
subclass. This is very important, because in your personal project, you
will likely want to extend Test::Base with your own filters and other
reusable pieces of your test framework.

Here is an example of a subclass:

    package MyTestStuff;
    use Test::Base -Base;

    our @EXPORT = qw(some_func);

    sub some_func {
        (my ($self), @_) = find_my_self(@_);
        ...
    }

    package MyTestStuff::Block;
    use base 'Test::Base::Block';

    sub desc {
        $self->description(@_);
    }

    package MyTestStuff::Filter;
    use base 'Test::Base::Filter';

    sub upper {
        $self->assert_scalar(@_);
        uc(shift);
    }

Note that you don't have to re-Export all the functions from
Test::Base. That happens automatically, due to the powers of Spiffy.

The first line in C<some_func> allows it to be called as either a
function or a method in the test code.

=head1 DISTRIBUTION SUPPORT

You might be thinking that you do not want to use Test::Base in you
modules, because it adds an installation dependency. Fear not.
Module::Install takes care of that.

Just write a Makefile.PL that looks something like this:

    use inc::Module::Install;

    name            'Foo';
    all_from        'lib/Foo.pm';

    use_test_base;

    WriteAll;

The line with C<use_test_base> will automatically bundle all the code
the user needs to run Test::Base based tests.

=head1 OTHER COOL FEATURES

Test::Base automatically adds:

    use strict;
    use warnings;

to all of your test scripts and Test::Base subclasses. A Spiffy
feature indeed.

=head1 HISTORY

This module started its life with the horrible and ridicule inducing
name C<Test::Chunks>. It was renamed to C<Test::Base> with the hope
that it would be seen for the very useful module that it has become. If
you are switching from C<Test::Chunks> to C<Test::Base>, simply
substitute the concept and usage of C<chunks> to C<blocks>.

=head1 AUTHOR

Ingy döt Net <ingy@cpan.org>

=head1 COPYRIGHT

Copyright (c) 2006. Ingy döt Net. All rights reserved.
Copyright (c) 2005. Brian Ingerson. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
