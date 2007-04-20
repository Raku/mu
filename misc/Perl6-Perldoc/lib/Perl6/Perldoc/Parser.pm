package Perl6::Perldoc::Parser;
#use Smart::Comments;

use warnings;
use strict;

use version; our $VERSION = qv('0.0.1');

# Regexes the parser needs...
my $FAIL          = q{(?!)};

my $BLANK_LINE    = q{^ \\s* $};

my $IDENT         = qr{ [^\W\d]\w* }xms;
my $QUAL_IDENT    = qr{ (?: $IDENT ::)* $IDENT }xms;
my $P6_QUAL_IDENT = qr{ $QUAL_IDENT (?: -\S+ )}xms;

my $DIR_NC   # DIRective but Not a Comment
    = qr{ (?= ^ = (?! \s | (?:(?:begin|end|for) [^\S\n]+)? comment\b ))}xms;

our $BALANCED_BRACKETS;
    $BALANCED_BRACKETS = qr{  <   (?: (??{$BALANCED_BRACKETS}) | . )*?  >
                           | \[   (?: (??{$BALANCED_BRACKETS}) | . )*? \]
                           | \{   (?: (??{$BALANCED_BRACKETS}) | . )*? \}
                           | \(   (?: (??{$BALANCED_BRACKETS}) | . )*? \)
                           | \xAB (?: (??{$BALANCED_BRACKETS}) | . )*? \xBB
                           }xms;

my $OPTION         = qr{ :   $IDENT  $BALANCED_BRACKETS?  | : !  $IDENT    }xms;
my $OPTION_EXTRACT = qr{ :()($IDENT)($BALANCED_BRACKETS?) | :(!)($IDENT)() }xms;

my $OPTIONS        = qr{ (?: \s* $OPTION)+ }xms;

my $FORMATTING_CODE = q{[BCDEIKLMNPRSTUVXZ]};

my $PERMITS_IMPLICIT
    = qr{\A (?: pod | (?! DATA ) [[:upper:]]+ ) \z}xms;

my $PERMITS_IMPLICIT_IF_DELIMITED
    = qr{ $PERMITS_IMPLICIT | \A (?: item | nested ) \z}xms;

# Error handlers (push error messages to the correct queue: warnings or errors)

# Format location nicely...
sub _loc {
    my ($range_ref) = @_;
    return defined $range_ref->{file}
                ? "$range_ref->{file} line $range_ref->{from}"
                : "line $range_ref->{from}"
                ;
}

# Missing block terminators must be (at least) warned about... 
sub _err_no_closing_delim {
    my ($top, $errors_ref, $warnings_ref) = @_;

    # Paragraphed, parser-introduced, and magic blocks don't need terminators...
    return if $top->{is_blank_terminated}
           || $top->{is_implicit}
           || $top->{typename} =~ m{\A (?: \(ambient\) | list | DATA | END ) }xms;

    my $location = _loc( $top->{range} );
    my $msg = 'No closing delimiter for '
            . ( $top->{style} eq 'formatting' 
                    ? "$top->{typename}$top->{left_delim}..."
                    : "'$top->{typename}' block"
              )
            . " opened at $location";

    # Only a fatal problem if the block is missing an =end...
    if ($top->{style} eq 'delimited') {
        push @{$errors_ref}, $msg;
    }
    else {
        push @{$warnings_ref}, $msg;
    }
}

# Nothing except options allowed after the block declarator...
sub _err_trailing_junk {
    my ($block_type, $range_ref, $junk, $errors_ref) = @_;
    my $location = _loc($range_ref);

    push @{$errors_ref}, 'Trailing junk after '
                       . ( $block_type =~ /[ ]/
                            ? $block_type
                            : "'$block_type' block specifier"
                         )
                       . " at $location: $junk"
                       ;
}

# All lowercase and uppercase block names are reserved...
sub _err_unknown_reserved_block {
    my ($block, $errors_ref) = @_;
    my $location = _loc($block->{range});

    push @{$errors_ref}, "Unknown reserved block type ('$block->{typename}') "
                       . " at $location"
                       ;
}

# No extra option lines after the content starts...
sub _err_options_too_late {
    my ($top, $range_ref, $warnings_ref) = @_;
    my $location = _loc($range_ref);
    
    push @{$warnings_ref}, 'Possible attempt to specify extra options '
                         . "too late in '$top->{typename}' block at $location";
}

# Unexpected block terminators often mean an typo on the opener...
sub _err_closed_unopened_block {
    my ($type, $range_ref, $errors_ref) = @_;
    my $location = _loc($range_ref);

    push @{$errors_ref}, "Invalid '=end $type' (not in '$type' block) "
                       . "at $location";
}

# A terminator on an END block is meaningless; report it...
sub _err_closed_END_block {
    my ($range_ref, $warnings_ref) = @_;
    my $location = _loc($range_ref);

    push @{$warnings_ref},  q{Ignored explicit '=end END' }
                         .  q{(END blocks run to end-of-file) }
                         . qq{at $location}
                         ;
}

# No =itemN unless there's a preceding =item(N-1)...
sub _err_missing_list_level {
    my ($range_ref, $item_level, $errors_ref) = @_;
    my $location = _loc($range_ref);

    push @{$errors_ref},  q{No =item} . ($item_level-1)
                       . qq{ before =item$item_level at $location};
}


# User-defined M<> code do have to be defined...
sub _err_missing_M_scheme {
    my ($range_ref, $errors_ref) = @_;
    my $location = _loc($range_ref);

    push @{$errors_ref},  q{Missing scheme specifier in M<> formatting code }
                       . qq{at $location};
}

# Link and =use targets actually have to point somewhere real...
sub _err_bad_use_uri {
    my ($uri, $range_ref, $errors_ref) = @_;
    my $location = _loc($range_ref);

    push @{$errors_ref}, qq{Unable to open URI in '=use $uri' }
                       . qq{at $location};
}


# Various places need to add content to the top of the stack...
sub _add_content {
    my ($top, $content) = @_;

    if (!$top->{content} || ref $top->{content}[-1]) {
        push @{ $top->{content} }, $content;
    }
    else {
        $top->{content}[-1] .= $content;
    }
}


# Extract options in Perl 6 format...

# Handled embedded quotes in <<>>
sub _shell_split {
    my ($content) = @_;
    my @components;
    pos $content = 0;
    while (pos $content < length $content) {
        $content =~ m{ \G \s*
                          (?: " ( [^"\\]* (?: \\. [^"\\]* )* ) "
                          |   ' ( [^'\\]* (?: \\. [^'\\]* )* ) '
                          |     ( \S+                        )
                          )
                          \s*
                     }gcxms;
        push @components, $+;
    }
    return @components;
}

# Parse and convert Perl 6 style :key(value) and key=>value pairs...
sub _extract_options {
    my ($options) = @_;

    return {} if $options !~ /\S/;

    my @components = grep { defined } $options =~ m{ $OPTION_EXTRACT }ogxms;

    my %options;
    while (my ($neg, $key, $val) = splice @components, 0, 3) {
        $options{$key} = $neg                             ? 0 
                       : !length $val                     ? 1
                       : $val =~ /^ \((.*)\) $/xms        ? eval($1)
                       : $val =~ /^(\[ .* \])$/xms        ? eval($1)
                       : $val =~ /^(\{ .* \})$/xms        ? eval($1)
                       : $val =~ /^ \<\s*(.*?)\s*\> $/xms ? [split /\s+/, $1]
                       : $val =~ /^ \xAB\s*(.*?)\s*\xBB $/xms
                                                          ? [_shell_split($1)]
                       :                              die "Internal error"
                       ;
    }

    return \%options;
}


# Track hierarchical counters for numbered blocks...
sub _next_number_for {
    my ($type, $depth, $disjoint, $state_ref) = @_;

    # Retrieve (or create) counter for this block type...
    my $numbers = $state_ref->{numberer_for}{$type} ||= [];

    # Reset top-level item if disjoint from previous items...
    if ($disjoint && $depth == 1 && $type eq 'item') {
        @{$numbers} = (0);
    }

    # Update according to relative depth...
    if ($depth > @{$numbers}) {         # nesting increased -> extend
        push @{$numbers}, (1) x ($depth-@{$numbers});
    }
    elsif ($depth == @{$numbers}) {     # nesting stays at same level
        $numbers->[-1]++;
    }  
    else {                              # nesting decreased -> truncate
        @{$numbers} = @{$numbers}[0..$depth-1];
        $numbers->[-1]++;
    }
    
    return join '.', @{$numbers};
}

# Handle block numbering and formatting...
sub _resolve_numbering {
    my ($data, $state_ref) = @_;

    # Handle :numbered set-up
    if ($data->{typename} =~ m{\A (\w+?)(\d*) \Z}xms) {
        my ($type, $depth) = ($1, $2||1);
        my $content_ref = \($data->{content}[0]);

        # Is this block explicitly numbered?
        my $explicitly_numbered = $data->{options}{numbered};
        if (!defined $explicitly_numbered) {
            $explicitly_numbered = $data->{config}{numbered};
        }
        
        # Otherwise, two types of block support implicit (#) numbering...
        my $implicitly_numbered
            =  ($type eq 'head' || $type eq 'item') 
            && !defined($explicitly_numbered)
            && !ref(${$content_ref})
            && defined ${$content_ref}
            && ${$content_ref} =~ m{ \A \s* [#] [^\S\n]+ }xms
            ;

        # Number if necessary...
        if ($explicitly_numbered || $implicitly_numbered) {
            # Clean up the magic leading # if present...
            if ($implicitly_numbered) {
                ${$content_ref} =~ s{\A \s* [#] [^\S\n]+ }{}xms
            }
            my $disjoint =  $data->{disjoint}
                         && !$data->{options}{continued}
                         && !$data->{config}{continued};
            $data->{number}
                = _next_number_for($type, $depth, $disjoint, $state_ref);
        }
    }

    # Recurse to process contents...
    for my $subtree ( @{ $data->{content} } ) {
        next if !ref $subtree;
        _resolve_numbering($subtree, $state_ref);
    }
}


# Create object tree from hash tree...
my $head_max = 4;   # Maximum predefined =headN block

sub _create_objects {
    my ($tree, $state_ref) = @_;    # $state_ref tracks global numbering, etc.

    # Recursively create subtrees first...
    for my $subtree ( @{ $tree->{content} || [] }) {
        if (ref $subtree) {
            $subtree = _create_objects($subtree, $state_ref);
        }
    }

    # Translate block names to class names, tracking reserved blocks...
    my $is_reserved;
    my $classname = 'Perl6::Perldoc::';
    my $typename = $tree->{typename};

    # Parenthesized name -> parser introduced (so its given its own name)
    if ($typename =~ m{^\( (\w+) \)}xms) {
        $classname .= "\u$1";
    }
    # Formatting codes are all subclasses of FormattingCode...
    elsif ($tree->{style} eq 'formatting') {
        $classname .= "FormattingCode::$typename";
    }
    # Directives are all subclasses of Directive...
    elsif ($tree->{style} eq 'directive') {
        $classname .= "Directive::$typename";
    }
    # Mixed-class block names -> user-defined subclass of Named...
    elsif ($typename =~ m{[[:upper:]]}xms && $typename =~ m{[[:lower:]]}xms) {
        $classname .= "Block::Named::$tree->{typename}";
        no strict 'refs';
        push @{$classname.'::ISA'}, 'Perl6::Perldoc::Block::Named';
    }
    # All upper or all lower case -> reserved block
    else {
        $tree->{is_semantic} = $typename =~ m{[[:upper:]]}xms;
#        $is_reserved = !$tree->{is_semantic}
#                       && $typename !~ m{\A (?:head|item) \d+ \z }xms;
        $is_reserved = $typename !~ m{\A (?:head|item) \d+ \z }xms;
        $classname .= "Block::$tree->{typename}";

        # Any non-existent headN classes inherit last defined headN class...
        if ($classname =~ m{:: head (\d+) \z}xms) {
            my $head_level = $1;
            $tree->{level} = $head_level;
            if ($head_level > $head_max) {
                no strict 'refs';
                @{ 'Perl6::Perldoc::Block::head'.$head_level.'::ISA' }
                    = 'Perl6::Perldoc::Block::head'.$head_max;
            }
        }

        # Any non-existent itemN classes act like existent itemN classes...
        elsif ($classname =~ m{:: item (\d+) \z}xms) {
            my $item_level = $1 || 1;
            $tree->{level} = $item_level;

            no strict 'refs';
            @{ 'Perl6::Perldoc::Block::item'.$item_level.'::ISA' }
                = 'Perl6::Perldoc::Block::item';
        }
    }

    # Construct corresponding object if possible...
    return $classname->new($tree, { errors=>$state_ref->{errors}, })
        if $classname->can('new');

    # If a built-in but no constructor, must be unknown...
    if ($is_reserved) {
        _err_unknown_reserved_block($tree, $state_ref->{errors});
    }

    # Otherwise, bless the raw data itself as an object...
    return bless $tree, $classname;
}

# Create a new config frame for a =config directive...
sub _extend_config {
    my ($curr_config_ref, $target_block, $new_opts_ref) = @_;

    # Default new config to copy of old config...
    my %config = %{$curr_config_ref};

    # Default new target block in config to copy of old target block...
    $config{$target_block}
        = $config{$target_block} ? {%{$config{$target_block}}} : {};

    # Change basis of target if :like specified
    my $likeness = $new_opts_ref->{like};
    if ($likeness) {
        for my $alike (ref $likeness eq 'ARRAY' ?  reverse @{$likeness} : $likeness) {
            my $like_config_ref = $curr_config_ref->{$alike};
            for my $option (keys %{$like_config_ref}) {
                unshift @{ $config{$target_block}{$option} }, 
                        @{$curr_config_ref->{$alike}{$option}||[]};
            }
        }
    }

    # Update all keys of target that appear in new options... 
    for my $opt (keys %{$new_opts_ref}) {
        next if $opt eq 'like';

        my $old_type = ref $config{$target_block}{$opt};
        my $new_type = ref $new_opts_ref->{$opt};

        if (!$old_type) {
            $config{$target_block}{$opt} = $new_opts_ref->{$opt};
        }
        elsif ($old_type ne $new_type) {
            die "Internal error: type mismatch on :$opt ($old_type vs $new_type)";
        }
        elsif ($old_type eq 'ARRAY') {
            $config{$target_block}{$opt}
                = [@{$config{$target_block}{$opt}}, @{$new_opts_ref->{$opt}}];
        }
        elsif ($old_type eq 'HASH') {
            $config{$target_block}{$opt}
                = { %{$config{$target_block}{$opt}}, %{$new_opts_ref->{$opt}}};
        }
        else {
            die "Internal error: bad :$opt of type $new_type";
        }
    }

    return \%config;
}

# Open or close implicit list blocks around item blocks

sub _adjust_lists {
    my ($stack_ref, $line, $is_item, $is_comment,
        $item_level, $range_ref, $warnings_ref)      = @_;

    my $parent_ref = $stack_ref->[-1];

    # Ignore blank lines and comments...
    return $parent_ref if $line !~ m{\S}xms || $is_comment;

    # Are we there yet?
    my $is_in_list = $parent_ref->{typename} eq 'list';
    my $list_level = $parent_ref->{level} || 0;

    # Ignore non-transition points...
    return $parent_ref
        if !$is_in_list && !$is_item
        || $is_in_list && $is_item && $list_level == $item_level;

    # Detect missing items...
    if ($list_level < $item_level-1) {
        _err_missing_list_level($range_ref, $item_level, $warnings_ref);
    }

    # Add required number of additional implicit lists...
    my %range = %{ $range_ref };
    while ($list_level < $item_level) {
        $list_level++;
        push @{$stack_ref}, {
            typename   => 'list',
            style      => 'implicit',
            range      => \%range,
            level      => $list_level,
            # terminator => $FAIL,
            terminator => qr{ (?= $parent_ref->{terminator} ) }xms,
            allow      => $parent_ref->{allow},
        };
    }

    # Alternatively, close required number of nested lists...
    while ($list_level > $item_level) {
        my $list_block = pop @{$stack_ref};
        $list_block->{range}{to} = $range{from}-1;
        push @{ $stack_ref->[-1]{content} }, $list_block;
        $list_level--;
    }

    return $stack_ref->[-1];
}

# Handle :like option (effectively prepending other defn to options)...
sub _handle_likeness {
    my ($top_ref, $config_ref) = @_;

    my %options = %{ $top_ref->{options} || {} };
    if ( my $like = $options{like} ) {
        my @likenesses = (ref($like)||q{}) eq 'ARRAY' ? @{ $like } : $like;

        for my $likeness (reverse @likenesses) {
            %options = (
                %{ $config_ref->{$likeness} || {} },
                %options,
            );
        }
    }

    return \%options;
}


# Handle any :formatted() options by imposing extra levels on stack...
sub _handle_formatted {
    my ($top, $range_ref, $config_ref, $errors_ref) = @_;
    my $location = _loc($range_ref);

    # Locate formatted options (on block itself or in block type's config)...
    my $formatted = $top->{options}{formatted}
                 || $config_ref->{$top->{typename}}{formatted};
    return if !$formatted;

    # Bad option! No biscuit!
    if ((ref($formatted)||q{}) ne 'ARRAY') {
        push @{$errors_ref},
            qq{Value of :formatted option not an array at $location};
        return;
    }

    my $terminator = "(?=$top->{terminator})";
    my $verbatim   = $top->{is_verbatim};

    my $permits_implicit_blocks = $top->{permits_implicit_blocks};

    # Work through specified formatting codes, adding nested block for each...
    my @blocks;
    my %range = %{$range_ref};
    FCODE:
    for my $fcode (@{ $formatted }) {
        if ($fcode !~ $FORMATTING_CODE) {
            push @{$errors_ref},
                qq{Unknown formatting code ($fcode) in :formatted option at $location};
            next FCODE;
        }
        $verbatim ||= $fcode =~ m{[VCMP]}xms;
        push @blocks, {
            typename    => $fcode,
            style       => 'formatting',
            config      => $config_ref->{"$fcode<>"},
            range       => \%range,
            initiator   => '(?!)',
            terminator  => $terminator,
            left_delim  => q{},
            right_delim => q{},
            is_verbatim => $verbatim,
            is_implicit => 1,
            permits_implicit_blocks => $permits_implicit_blocks,
        };
    }
    return @blocks;
}

# Track which nested formatting codes are allowed for a given block...
sub _update_allow {
    my ($top, $config, $options) = @_;
    my %new_allow = %{ $top->{allow} || {} };

    # If not explicit on block, try config...
    if (my $src_ref = $options->{allow} || $config->{allow}) {
        if (ref $src_ref eq 'ARRAY') {
            @new_allow{ @{$src_ref} } = ();
        }
        else {
            $new_allow{ $src_ref } = undef;
        }
    }

    return \%new_allow;
}


# Parse input from a filehandle or filename, extracting the Pod...
sub parse {
    my ($classname, $filehandle, $opt_ref) = @_;
    my $filename = undef;

    # If filename passed, open it...
    if (!ref $filehandle) {
        $filename = $filehandle;
        undef $filehandle;
        open $filehandle, '<', $filename
            or require Carp
            and Carp::croak("parse() can't open file $filename ($!)");

        if (!exists $opt_ref->{all_pod} || $opt_ref->{all_pod} =~ m{\A auto \z}ixms) {
            $opt_ref->{all_pod} = $filename =~ m{ [.] pod6? }xms;
        }
    }

    # Remember where we found this data...
    my %range = ( file=>$filename, from => 0 );

    # Initialize stack representation of Pod...
    my @stack = {
        typename   => '(document)',
        terminator => $FAIL,
        range      => {%range},
    };

    # Initialize configuration stack to track lexical =config directives
    my @config_stack = {};

    # Track P<toc:...> requests...
    my @toc_placements;

    # Add implicit =pod block if caller indicates it's all pod...
    if ($opt_ref->{all_pod}) {
        push @stack, {
            typename    => 'pod',
            style       => 'implicit',
            config      => {},
            range       => { %range },
            terminator  => $FAIL,
            is_implicit => 1,
            permits_implicit_blocks => 1,
        };
    }

    # Initialize look-up table of allowed formatting codes...
    $stack[-1]{allow} = _update_allow($opt_ref||{},{},{});

    # Storage for reporting problems...
    my (@errors, @warnings);

    # Is a block with options waiting for possible extra options?
    my $has_options_pending;


    # Parse input line-by-line...
    LINE:
    while (my $line = <$filehandle>) {
        pos $line = 0;
        $range{from} = $.;
        my $is_directive = substr($line, 0, 1) eq '=';

        # Within line, parse out each token...
        TOKEN:
        while (pos $line < length $line) {
            ### AT: substr($line, pos $line)
            my $top = $stack[-1];

            # Check and process options pending...
            if ($has_options_pending) {
                # Extra options on a line immediately after a block specifier...
                if ($line =~ m{ \G ^ = (\s+ $OPTIONS) \s* $ }ogcxms) {
                    my $options = $1;

                    $top->{options} =
                        { %{ $top->{options} },
                          %{ _extract_options($options) },
                        };

                    ### Adding more options: $stack[-1]
                    next LINE;
                }

                # No extra options, then handle :like, :formatted options...
                else {
                    my $config_ref = $config_stack[-1];
                    $top->{options} = _handle_likeness($top, $config_ref);

                    if ($top->{style} =~ m{\A (?:delimited|paragraph) \z}xms) {
                        push @stack, _handle_formatted(
                            $top, \%range, $config_ref, \@errors
                        );
                    }
                    $has_options_pending = 0;

                    $top->{allow}
                        = _update_allow( $stack[-2],
                                         $config_ref->{$top->{typename}},
                                         $top->{options}
                                       );
                }
            }

            # A close marker for the innermost block or formatting code...
            if ($line =~ m{ \G ($top->{terminator}) }gcxms) {
                my $terminator = $1;

                # Is this an implicit close (i.e. an outer block closing)?
                if (length($terminator) == 0) {
                    _err_no_closing_delim($top, \@errors, \@warnings);
                    pos $line = pos $line;  # Workaround for bug in /gc :-(
                }

                # Is this a nested close marker in a formatting code?
                if ($top->{style} eq 'formatting' && $top->{delim_nesting}) {
                    # If so, decrease the nesting and treat as plain content...
                    $top->{delim_nesting}--;
                    _add_content($top, $terminator);
                    ### Added nested formatting code terminator
                }
                # If not nested formatting code delimiter, close the block...
                else {
                    my $block = pop @stack;

                    # Execute any use statement...
                    if ($block->{typename} eq '(use)') {
                        my $source = $block->{source};
                        if (eval "require $source") {
                            my %options = (
                                %{ $block->{config}{use}||{} },
                                %{ $block->{options} || {} },
                            );
                            $source->import(\%options);
                        }
                        else {
                            _err_use_cant_load($source, \%range, \@errors);
                            next TOKEN;
                        }
                    }

                    # Syncronize config stack...
                    for (1..$block->{has_config}||0) {
                        pop @config_stack;
                    }
        
                    # Incorporate closed block into representation...
                    if ($block->{style} ne 'implicit' || $block->{content}) {
                        # Complete line range...
                        $block->{range}{to} = $range{from};

                        # Remove parser-specific internal data...
                        delete @{$block}{qw<terminator initiator allow>};

                        # Add block to parent...
                        push @{ $stack[-1]{content} }, $block;
                        
                        ### Terminated block: $block
                    }
                    else {
                        ### Threw away empty implicit block: $block
                    }
                }
                next TOKEN;
            }

            # Content of comments is appended raw...
            if ($top->{typename} eq 'comment') {
                $top->{content}[0] .= $line;
                next LINE;
            }

            # All directives start with '=' on the line...
            if ($is_directive) {
                # Unexpected close marker for unopened block...
                if ($line =~ m{\G ^ =end \s+ (\S+) }gcxms) {
                    my $type = $1;
                    if ($type eq 'END') {
                        _err_closed_END_block(\%range, \@warnings);
                    }
                    else {
                        _err_closed_unopened_block($type, \%range, \@errors);
                    }

                    ### Unexpected =end $type: $line
                    next LINE;
                }

                # Open marker for delimited block...
                if ($line =~ m{\G ^ =begin \s+ ($IDENT) ($OPTIONS?) \s* (.*) $}ogcxms) {
                    my ($type, $options, $junk) = ($1, $2, $3);

                    # Anything after last option is junk...
                    if ($junk) {
                        _err_trailing_junk($type, \%range, $junk, \@errors);
                    }

                    # Track level of =item blocks...
                    my ($is_item, $item_level) 
                        = $type =~ m{\A (item)(\d+)? \Z}xms;
                    $item_level ||= $is_item ? 1 : 0;

                    my $is_comment = $type eq 'comment';

                    # Insert or close implicit list block if required...
                    $top = _adjust_lists(\@stack,     $line,       $is_item,
                                         $is_comment, $item_level,
                                         \%range,     \@warnings
                                        );
                    my $disjoint_item1
                        =  $is_item && $item_level==1 && !$top->{content};

                    my $permits_implicit_blocks
                        = $type =~ m{$PERMITS_IMPLICIT_IF_DELIMITED}xms;

                    $has_options_pending = 1;

                    # Terminator is corresponding =end or parent's terminator...
                    my $terminator
                        = $type eq 'END' ? '(?!)'
                        :                  qr{^ =end \s+ \Q$type\E [^\n]* \n? $
                                             | (?= $top->{terminator} )
                                             }xms
                        ;

                    $options = _extract_options($options);
                    my $config = $config_stack[-1]{$type};

                    my $verbatim = $type eq 'code';

                    # Add to parsing stack (not yet in tree)...
                    push @stack, {
                        typename   => $type,
                        style      => 'delimited',
                        range      => { %range },
                        options    => $options,
                        config     => $config,
                        terminator => $terminator,
                        is_verbatim   => $verbatim || $top->{is_verbatim},
                        disjoint      => $disjoint_item1,
                        permits_implicit_blocks => $permits_implicit_blocks,
                    };

                    ### Opened delimited block: $stack[-1]
                    next TOKEN;
                }

                # Open marker for paragraph block...
                if ($line =~ m{ \G ^ =for \s+ ($IDENT) ($OPTIONS?) \s* (.*) $ }ogcxms) {
                    my ($type, $options, $junk) = ($1, $2, $3);

                    # Anything after last option is junk...
                    if ($junk) {
                        _err_trailing_junk($type, \%range, $junk, \@errors);
                    }

                    my $permits_implicit_blocks
                        = $type =~ m{$PERMITS_IMPLICIT}xms;

                    # Track level of =item blocks...
                    my ($is_item, $item_level) 
                        = $type =~ m{\A (item)(\d+)? \Z}xms;
                    $item_level ||= $is_item ? 1 : 0;
                    
                    $has_options_pending = 1;

                    my $is_comment = $type eq 'comment';

                    # Insert or close implicit list block if required...
                    $top = _adjust_lists(\@stack,     $line,      $is_item,
                                         $is_comment, $item_level,
                                         \%range,     \@warnings
                                        );

                    my $disjoint_item1
                        =  $is_item && $item_level==1 && !$top->{content};

                    my $verbatim = $type eq 'code';

                    $options = _extract_options($options);
                    my $config = $config_stack[-1]{$type};

                    # Add to parsing stack (not yet in tree)...
                    push @stack, {
                        typename   => $type,
                        style      => 'paragraph',
                        range      => { %range },
                        options    => $options,
                        config     => $config,
                        terminator => qr{ ^ \s* $
                                        | $DIR_NC
                                        | (?= $top->{terminator} )
                                        }xms,
                        is_verbatim   => $verbatim || $top->{is_verbatim},
                        is_blank_terminated => 1,
                        disjoint      => $disjoint_item1,
                        permits_implicit_blocks => $permits_implicit_blocks,
                    };

                    ### Opened paragraph block: $stack[-1]
                    next TOKEN;
                }

                # =use URI directive
                if ($line =~ m{ \G ^ =use \s+ (\S+) ($OPTIONS?) \s* ([^\n]*) \n }ogcxms) {
                    my ($source, $options, $junk) = ($1, $2, $3);
                    my $orig_source = $source;

                    # Anything after last option is junk...
                    if ($junk) {
                        _err_trailing_junk('=use directive', \%range, $junk, \@errors);
                    }

                    $has_options_pending = 1;

                    # Insert or close implicit list block if required...
                    $top = _adjust_lists(\@stack, $line, 0, 0, 0, \%range, \@warnings);

                    # Can use Perl 5 modules...
                    if ($source =~ m{\A (?:perl5:)? $QUAL_IDENT \Z}xms) {
                        push @stack, {
                            typename   => 'use',
                            style      => 'directive',
                            source     => $source,
                            range      => { %range },
                            terminator => qr{ ^ \s* $
                                            | $DIR_NC
                                            | (?= [^=] )
                                            | (?= $top->{terminator} )
                                            }xms,
                            options    => _extract_options($options),
                            config     => $config_stack[-1]{use},
                            is_blank_terminated => 1,
                        };

                        ### =use directive: $stack[-1]
                        next TOKEN;
                    }

                    # Otherwise, no options allowed (on direct inclusions)...
                    if ($options) {
                        _err_trailing_junk('=use directive', \%range,
                                           $options,         \@errors
                                          );
                    }

                    # Assume it's a Pod file; open it...
                    $source =~ s{\A file:}{}xms;
                    if (!-r $source) {
                        _err_bad_use_uri($orig_source, \%range, \@errors);
                        next TOKEN
                    }

                    my %opts;
                    if ($source =~ m{ [.]pod6 \Z }xms) {
                        $opts{all_pod} = 1;
                    }

                    # Then read, parse, and add in the result (recursively)...
                    my $result_ref
                        = Perl6::Perldoc::Parser->parse($source, \%opts);
                    if ($result_ref->{tree}) {
                        push @{$stack[-1]{content}}, {
                            typename   => 'use',
                            style      => 'directive',
                            uri        => $orig_source,
                            range      => { %range, to => $range{from} },
                            content    => $result_ref->{tree}{content},
                        };
                    }

                    # Propagate any warnings or errors...
                    if ($result_ref->{errors}) {
                        push @errors, @{ $result_ref->{errors} };
                    }
                    if ($result_ref->{warnings}) {
                        push @warnings, @{ $result_ref->{warnings} };
                    }

                    ### =use directive: $stack[-1]
                    next TOKEN;
                }

                # =encoding directive
                if ($line =~ m{ \G ^ =encoding \s+ (\S+) \s* ([^\n]*) \n }ogcxms) {
                    my ($encoding, $junk) = ($1, $2);

                    # =encoding takes no options...
                    if ($junk) {
                        _err_trailing_junk('=encoding directive', \%range, $junk, \@errors);
                    }

                    # It also terminates any surrounding list...
                    $top = _adjust_lists(\@stack, $line, 0, 0, 0, \%range, \@warnings);

                    # Add it to the stack (not yet in the representation)...
                    push @stack, {
                        typename   => 'encoding',
                        style      => 'directive',
                        encoding   => $encoding,
                        range      => { %range },
                        terminator => qr{ ^ \s* $
                                        | $DIR_NC
                                        | (?= [^=] )
                                        | (?= $top->{terminator} )
                                        }xms,
                        is_blank_terminated => 1,
                    };

                    ### =encoding directive: $stack[-1]
                    next TOKEN;
                }

                # =config directive
                if ($line =~ m{ \G ^ =config \s+ ($FORMATTING_CODE<>|$IDENT) ($OPTIONS?) \s* ([^\n]*) \n }ogcxms) {
                    my ($config_type, $options, $junk) = ($1, $2, $3);

                    # Anything after last option is junk...
                    if ($junk) {
                        _err_trailing_junk("=config directive", \%range, $junk, \@errors);
                    }

                    $has_options_pending = 1;

                    my $parsed_opts_ref
                        = _extract_options($options);

                    # Record added config scope in parent...
                    $stack[-1]{has_config}++;

                    # Add new lexical config frame...
                    push @config_stack, _extend_config(
                                            $config_stack[-1],
                                            $config_type,
                                            $parsed_opts_ref,
                                        );

                    # Directive closes any surrounding list...
                    $top = _adjust_lists(\@stack, $line, 0, 0, 0, \%range, \@warnings);

                    # Save representation of =config directive...
                    push @stack, {
                        typename   => 'config',
                        style      => 'directive',
                        target     => $config_type,
                        options    => $parsed_opts_ref,
                        range      => { %range },
                        terminator => qr{ ^ \s* $
                                        | $DIR_NC
                                        | (?= [^=] )
                                        | (?= $top->{terminator} )
                                        }xms,
                        is_blank_terminated => 1,
                    };

                    ### =config directive: $stack[-1]
                    ### Config now: $config_stack[-1]
                    next TOKEN;
                }

                # Open marker for abbreviated block...
                if ($line =~ m{ \G ^ = ($IDENT) \s* }ogcxms) {
                    my $type = $1;

                    # Work out its nesting level if it's an item block...
                    my ($is_item, $item_level) 
                        = $type =~ m{\A (item)(\d+)? \Z}xms;
                    $item_level ||= $is_item ? 1 : 0;

                    my $is_comment = $type eq 'comment';

                    # Open or close implicit list if necessary...
                    $top = _adjust_lists(\@stack, $line, $is_item, $is_comment,
                                         $item_level, \%range, \@warnings
                                        );

                    my $disjoint_item1
                        =  $is_item && $item_level==1 && !$top->{content};

                    my $permits_implicit_blocks
                        = $type =~ m{$PERMITS_IMPLICIT}xms;

                    my $verbatim = $type eq 'code';

                    my $config = $config_stack[-1]{$type};

                    # Copy allowed fcodes...
                    my $allow_ref = _update_allow($top, $config, {});

                    # Add it to the stack (not yet in the representation)...
                    push @stack, {
                        typename   => $type,
                        style      => 'abbreviated',
                        config     => $config,
                        range      => { %range },
                        terminator => qr{ ^ \s* $
                                        | $DIR_NC
                                        | (?= $top->{terminator} )
                                        }xms,
                        is_verbatim   => $verbatim || $top->{is_verbatim},
                        is_blank_terminated => 1,
                        allow         => $allow_ref,
                        disjoint      => $disjoint_item1,
                        permits_implicit_blocks => $permits_implicit_blocks,
                    };

                    # Handling configured implicit formatting, if any...
                    $stack[-1]->{options}
                        = _handle_likeness($stack[-1], $config_stack[-1]);
                    push @stack, _handle_formatted(
                        $stack[-1], \%range, $config_stack[-1], \@errors
                    );

                    # Finished with directive (may be trailing data)
                    $is_directive = 0;

                    ### Opened abbreviated block: $stack[-1]
                    next TOKEN;
                }

                # Treat "late" option lines as content (with warning)...
                if ($line =~ m{ \G ^ = (\s+ $OPTIONS) \s* $ }ogcxms) {
                    _err_options_too_late($top, \%range, \@warnings);

                    _add_content($top, $line);

                    ### Added dubious raw content: $stack[-1]
                    next LINE;
                }
            }

            # If not directive, must be ambient text, raw para or code block...

            # Close implicit item list if necessary...
            $top = _adjust_lists(\@stack, $line, 0, 0, 0, \%range, \@warnings);

            # Deal with ambient text (i.e. non-Pod) and unprocessed blocks...
            if (@stack == 1) {
                $top = {
                    typename   => '(ambient)',
                    style      => 'implicit',
                    range      => { %range },
                    terminator => qr{(?= ^ = $IDENT) }xms,
                };
                push @stack, $top;
            }
            if ($top->{typename} =~ m{^(?:\(ambient\)|table)$}xms
                || $top->{typename} =~ m{[[:upper:]]}
                   && $top->{typename} =~ m{[[:lower:]]}
            ) {
                if ($line =~ m{ \G (.*) }gcxms) {
                    _add_content($top, $1);
                    ### Unprocessed text: $1
                }

                next LINE;
            }

            # Implicit code/para Pod block depends on indenting...
            if (pos($line) == 0 && $top->{permits_implicit_blocks}) {
                my $terminator 
                    = $top->{style} eq 'delimited' ? '^ \s* $' : '(?= ^ \s* $)';

                # Indented block is code block...
                if ($line =~ m{ \G ^ (?= [^\S\n]+ \S [^\n]* $ ) }gcxms) {

                    my $config = $config_stack[-1]{'code'};
                    my $allow_ref = _update_allow($top, $config, {});

                    push @stack, {
                        typename   => 'code',
                        style      => 'implicit',
                        config     => $config,
                        allow      => $allow_ref,
                        range      => { %range },
                        terminator => qr{ $terminator
                                        | $DIR_NC
                                        | (?= $top->{terminator} )
                                        }xms,
                        is_verbatim   => 1,
                        is_blank_terminated => 1,
                    };

                    _add_content($stack[-1], $line);

                    ### Opened implicit code block: $stack[-1]
                    next LINE;
                }
                # Unindented block is para block...
                elsif ($line =~ m{ \G (?= \S .* $ ) }gcxms) {

                    my $config = $config_stack[-1]{'para'};
                    my $allow_ref = _update_allow($top, $config, {});

                    push @stack, {
                        typename   => 'para',
                        style      => 'implicit',
                        config     => $config,
                        allow      => $allow_ref,
                        range      => { %range },
                        terminator => qr{ $terminator
                                        | $DIR_NC
                                        | (?= $top->{terminator} )
                                        }xms,
                        is_verbatim=>  $top->{is_verbatim},
                        is_blank_terminated => 1,
                    };

                    ### Opened implicit para block: $stack[-1]
                    next TOKEN;
                }
                else { # Meaningless empty line
                    next LINE;
                }
            }

            # Open marker for formatting code (only outside V<> and C<>)...
            if ( ( !$top->{is_verbatim}
                 || exists $top->{allow}{substr($line,pos $line,1)}
                 )
                && $line =~ m{ \G ($FORMATTING_CODE) ((?><+)|\xAB) }ogcxms
            ) {
                    my ($type, $delim) = ($1, $2);

                    # Generate right delimiter (and nested matcher) from left...
                    my $rdelim = $delim;
                    $rdelim =~ tr/<\xAB/>\xBB/;
                    my $initiator = $delim . ($delim =~ /</ ? '(?!<)' : q{});
                    my $terminator = length($delim) == 1 ? $rdelim
                                   :                       "$rdelim(?!>)"
                                   ;

                    # Don't look up terminator stack if partial matches might
                    # occur...
                    if ($rdelim =~ />/ && $rdelim !~ $top->{terminator}) {
                        $terminator .= "|(?=$top->{terminator})"
                    }

                    my $config_ref = $config_stack[-1]{"$type<>"};
                    my $allow_ref = _update_allow($top, $config_ref, {});

                    push @stack, {
                        typename    => $type,
                        style       => 'formatting',
                        config      => $config_ref,
                        allow       => $allow_ref,
                        range       => { %range },
                        initiator   => $initiator,
                        terminator  => $terminator,
                        left_delim  => $delim,
                        right_delim => $rdelim,
                        is_verbatim => ($type =~ m{[VCMP]}xms ? 1 : 0)
                                    || $top->{is_verbatim},
                        permits_implicit_blocks
                                    => $top->{permits_implicit_blocks},
                    };

                    # Track placement requests for table-of-contents...
                    if ($type eq 'P') {
                        push @toc_placements, $stack[-1];
                    }

                    ### Opened formatting code: $stack[-1]
                    next TOKEN;
            }

            # Balance nested delimiters inside a formatting code...
            if (   $top->{style} eq 'formatting'
                && $line =~ m{ \G ($top->{initiator}) }gcxms
            ) {
                my $delim = $1;
                $top->{delim_nesting}++;
                _add_content($top, $delim);

                ### Nested left delimiter in formatting code: $stack[-1]
                next TOKEN;
            }

            # Is there a separator in one of the "separable" codes?
            if (   $top->{style} eq 'formatting'
                && $top->{typename} =~ m{\A [DLX] \Z}xms
                && $line =~ m{ \G [|] }gcxms) {
                $top->{target} = "";
            }

            # Otherwise, it's raw content or target (eat *all* angles, if any)..
            if ($line =~ m{ \G ( [\xAB<]+ | [^A-Z|\n<>\xAB\xBB]+ | . ) }gcxms) {
                # Are we in the "target" section yet?
                if (exists $top->{target}) {
                    $top->{target} .= $1;
                    ### Added target: $stack[-1]
                }
                # Otherwise, still in the "appearance" section
                else {
                    _add_content($top, $1);
                    ### Added raw content: $stack[-1]
                }

                next TOKEN;
            }

            # Should be impossible to get to here...
            die "Internal error near: ", substr($line, pos $line);
        }
    }

    # Close and nest any unclosed blocks at the end of the file...
    while (@stack > 1) {
        my $top = $stack[-1];
        my $line_num = $.;

        # Record the missing closing delimiter...
        _err_no_closing_delim($top, \@errors, \@warnings);

        # Finish line range and remove internal parsing data...
        my $block = pop @stack;
        $block->{range}{to} = $line_num;
        delete @{$block}{qw< terminator initiator has_config allow>};

        # Execute any use statement...
        if ($block->{typename} eq '(use)') {
            my $source = $block->{source};
            if (eval "require $source") {
                my %options = (
                    %{ $block->{config}{use}||{} },
                    %{ $block->{options} || {} },
                );
                $source->import(\%options);
            }
            else {
                _err_use_cant_load($source, $line_num, \@errors);
                next TOKEN;
            }
        }

        push @{ $stack[-1]{content} }, $block;

        ### Implicitly terminated block: $block
    }

    # Apply global processing to root of data structure...
    my $root = pop(@stack);

    # Number all numbered blocks...
    my $state_ref = { errors => \@errors };
    _resolve_numbering($root, $state_ref);

    # Convert internal hash-based representation to objects...
    my $tree = _create_objects($root, $state_ref);

    # Build and install any tables-of-content for P<toc:...> codes...
    TOC:
    for my $toc_placement_obj (@toc_placements) {
        next TOC if $toc_placement_obj->{content}[0] !~ m{\A \s* toc:}xms;

        # Replace P<toc:...>'s contents with TOC...
        $toc_placement_obj->{content}
            = [ _build_toc($tree, $toc_placement_obj) ];

        # Set flag to ignore this node on subsequent TOC-building passes...
        $toc_placement_obj->{ignore_toc} = 1;
    }

    # Aggregrate and return information in an object...
    return bless {
        tree     => $tree,
        errors   => \@errors,
        warnings => \@warnings,
    }, 'Perl6::Perldoc::Parser::ReturnVal';

}

# Build the table of contents for a given P<toc:> request...
sub _build_toc {
    my ($data_structure, $placement_obj) = @_;

    # Work out what's in the TOC (including the =item/=item1 alias)...
    my $requested_types = $placement_obj->{target};
       $requested_types =~ s{\A \s* toc: \s*}{}xms;
    my %toc_wants; 
       @toc_wants{ split m/\s+/, $requested_types } = ();
    if (exists $toc_wants{item} || exists $toc_wants{item1}) {
       @toc_wants{qw< item item1 >} = ();
    }

    # Build flat list of tocitems into nested toclists...
    my @toc_stack = [];
    for my $toc_entry ( _walk_toc($data_structure, \%toc_wants) ) {
        my $level = $toc_entry->{level};

        # Increase nesting for higher numbered items...
        while ($level > @toc_stack) {
            push @toc_stack, [];
        }
        # Decrease nesting for lower numbered items...
        while ($level < @toc_stack) {
            my $content = pop @toc_stack;
            push @{ $toc_stack[-1] }, Perl6::Perldoc::Block::toclist->new({
                typename => 'toclist',
                style    => 'implicit',
                content  => $content,
                range    => {},
            });
        }
        # Insert the item into the hierarchy...
        push @{ $toc_stack[-1] }, $toc_entry;
    }

    # Nest any unclosed lists...
    while (@toc_stack > 1) {
        my $content = pop @toc_stack;
        push @{ $toc_stack[-1] }, Perl6::Perldoc::Block::toclist->new({
            typename => 'toclist',
            style    => 'implicit',
            content  => $content,
            range    => {},
        });
    }

    # Retrieve a flat list of tocitem blocks representing the TOC...
    return @{ $toc_stack[-1] };
}

# Blocks without an inherent nesting level default to this nesting...
my $DEFAULT_LEVEL = 5;

# Walk DOM tree extracting blocks specified to be part of TOC...
use Scalar::Util qw< reftype >;
sub _walk_toc {
    my ($node, $wanted_ref) = @_;

    my $node_type = reftype($node) || q{};

    # Hashes are nodes: check if this one (and its subnodes) should be included
    if ($node_type eq 'HASH') {
        return if $node->{ignore_toc};

        my $node_class = $node->{typename};
        my @this_node;

        # Is this node part of the TOC?
        my $wanted = exists $wanted_ref->{$node_class}
                     || $node->{is_semantic} && exists $wanted_ref->{'head1'};
        if ($wanted) {
            my $level
                = $node->{is_semantic}                  ? 1
                : $node_class =~ m{\A head (\d+) \z}xms ? $1
                :                                         $DEFAULT_LEVEL
                ;

            my $target = $node+0;

            # Create a TOC entry (a list item with a link inside it)...
            @this_node = bless {
                typename => "tocitem$level",
                style    => 'implicit',
                level    => $level,
                target   => "#$target",
                content  => [$node],
                range    => {},
            }, "Perl6::Perldoc::Block::tocitem$level";

            # Install the TOC entry's class in the DOM...
            no strict 'refs';
            @{"Perl6::Perldoc::Block::tocitem${level}::ISA"}
                = 'Perl6::Perldoc::Block::tocitem';
        }

        # Does it have subnodes that are part of the TOC?
        my @sub_nodes = _walk_toc($node->{content}, $wanted_ref);

        # Return node's TOC entry (if any) and those of its contents...
        return @this_node, @sub_nodes;
    }

    # Arrays may contain nodes: check each element...
    elsif ($node_type eq 'ARRAY') {
        return map { _walk_toc($_, $wanted_ref) }  @{$node};
    }

    # Ignore everything else...
    else {
        return;
    }
}


# Standard classes for Perldoc DOM...

package Perl6::Perldoc::Parser::ReturnVal;

sub report_errors {
    my $self = shift;

    # Report warnings...
    if (@{$self->{warnings}}) {
        print {*STDERR} join "\n", @{$self->{warnings}}, "";
    }

    # Report errors and die...
    if (@{$self->{errors}}) {
        print {*STDERR} join "\n", @{$self->{errors}}, "";

        # Die in context if a fatality message was specified...
        if (@_) {
            require Carp and Carp::croak(@_);
        }
        # Otherwise die silently...
        else {
            die "\n";
        }
    }

    # On success, return self to allow for chaining...
    return $self;
}

package Perl6::Perldoc::Root;
use strict;
use warnings;

# Root ctor just blesses the data structure...
sub new {
    my ($classname, $data_ref) = @_;

    return bless $data_ref, $classname;
}

# Standard read-only accessor methods shared by all DOM components...
sub typename         { my ($self) = @_; return $self->{typename};           }
sub style            { my ($self) = @_; return $self->{style};              }
sub target           { my ($self) = @_; return $self->{target};             }
sub range            { my ($self) = @_; return $self->{range};              }
sub config           { my ($self) = @_; return $self->{config};             }
sub number           { my ($self) = @_; return $self->{number};             }
sub title            { my ($self) = @_; return '[' . $self->typename . ']'; }
sub is_semantic      { my ($self) = @_; return $self->{is_semantic};        }
sub is_numbered      { my ($self) = @_; return exists $self->{number};      }
sub is_post_numbered { 0 }

sub content {
    my ($self) = @_;
    my $vals_ref = $self->{content};
    if (!wantarray) {
        if (@{ $vals_ref } > 1) {
            require Carp and Carp::carp(
                "Multivalued accessor content() called in scalar context"
            );
        }
        return $vals_ref->[0];
    }
    return @{ $vals_ref };
}

# Asking for an option falls back to the config if necessary...
sub option {
    my ($self, $opt_name) = @_;
    return $self->{options}{$opt_name}
        if defined $self->{options}{$opt_name};
    return $self->{config}{$opt_name}
        if defined $self->{config}{$opt_name};
    return;
}

# Return an object's term or caption...

sub _flatten_or_convert_option {
    my ($self, $opt_ref, $option_name) = @_;
    my $value = $self->option($option_name);

    # Flatten if value specified as a list...
    if (ref($value) eq 'ARRAY') {
        $value = "@{$value}";
    }

    # Return raw if not requested as an object...
    return $value if !$opt_ref->{as_objects};

    my $cache_slot = "parsed_$option_name";
    # Otherwise, convert to Pod object and cache for reuse...
    if (!$self->{$cache_slot}) {
        open my $fh, '<', \$value
            or die "Internal error: can't parse :$option_name";
        $self->{$cache_slot}
            = Perl6::Perldoc::Parser->parse( $fh, { all_pod => 1 })->{tree};
    }

    return $self->{$cache_slot};
}

sub term {
    my ($self, $opt_ref) = @_;
    return _flatten_or_convert_option($self, $opt_ref, 'term');
}

sub caption {
    my ($self, $opt_ref) = @_;
    return _flatten_or_convert_option($self, $opt_ref, 'caption');
}



# Representation of file itself...
package Perl6::Perldoc::File;  
    use base 'Perl6::Perldoc::Root';

# Representation of document...
package Perl6::Perldoc::Document;  
    use base 'Perl6::Perldoc::Root';

# Ambient text around the Pod...
package Perl6::Perldoc::Ambient;  
    use base 'Perl6::Perldoc::Root';


# Pod directives...
package Perl6::Perldoc::Directive;    
    use base 'Perl6::Perldoc::Root';

# Standard =use directive...
package Perl6::Perldoc::Directive::use; 
    use base 'Perl6::Perldoc::Directive';

# Standard =config directive...
package Perl6::Perldoc::Directive::config; 
    use base 'Perl6::Perldoc::Directive';

# Standard =encoding directive...
package Perl6::Perldoc::Directive::encoding; 
    use base 'Perl6::Perldoc::Directive';


# Pod blocks...
package Perl6::Perldoc::Block;    
    use base 'Perl6::Perldoc::Root';

# Base class for user-defined blocks...
package Perl6::Perldoc::Block::Named;    
    use base 'Perl6::Perldoc::Block';

# Standard =pod block...
package Perl6::Perldoc::Block::pod;    
    use base 'Perl6::Perldoc::Block';

# Standard =para block (may be implicit)...
package Perl6::Perldoc::Block::para;   
    use base 'Perl6::Perldoc::Block';

# Standard =code block (may be implicit)...
package Perl6::Perldoc::Block::code;   
    use base 'Perl6::Perldoc::Block';

# Standard =input block
package Perl6::Perldoc::Block::input;   
    use base 'Perl6::Perldoc::Block';

# Standard =output block
package Perl6::Perldoc::Block::output;   
    use base 'Perl6::Perldoc::Block';

# Base class for =headN classes
package Perl6::Perldoc::Heading;
    use base 'Perl6::Perldoc::Block';

# All headings have a title (which is just their contents)...
sub title {
    my ($self) = @_;

    my $vals_ref = $self->{content};

    if (exists $self->{number}) {
        unshift @{$vals_ref}, "$self->{number}. ";
    }

    if (!wantarray) {
        if (@{ $vals_ref } > 1) {
            require Carp and Carp::carp(
                "Multivalued accessor title() called in scalar context"
            );
        }
        return $vals_ref->[0];
    }
    return @{ $vals_ref };
}

# Standard =head1 block...
package Perl6::Perldoc::Block::head1;  
    use base 'Perl6::Perldoc::Heading';

# Standard =head2 block...
package Perl6::Perldoc::Block::head2;  
    use base 'Perl6::Perldoc::Heading';

# Standard =head3 block...
package Perl6::Perldoc::Block::head3;  
    use base 'Perl6::Perldoc::Heading';

# Standard =head4 block...
package Perl6::Perldoc::Block::head4;  
    use base 'Perl6::Perldoc::Heading';

# Standard =item block...
package Perl6::Perldoc::Block::item;   
    use base 'Perl6::Perldoc::Block';

# Implicit =list block...
package Perl6::Perldoc::Block::list;   
    use base 'Perl6::Perldoc::Block';

# Implicit =tocitem block...
package Perl6::Perldoc::Block::tocitem;   
    use base 'Perl6::Perldoc::Block';

sub title {
    my ($self) = @_;
    my $content = $self->{content}[0];
    return $content->title();
}

# Implicit =toclist block...
package Perl6::Perldoc::Block::toclist;   
    use base 'Perl6::Perldoc::Block';

# Standard =nested block...
package Perl6::Perldoc::Block::nested;   
    use base 'Perl6::Perldoc::Block';

sub new {
    my ($self, $data) = splice @_, 0, 2;
    $data->{nested} ||= 1;
    return $self->SUPER::new($data, @_);
}

# Standard =comment block...
package Perl6::Perldoc::Block::comment;   
    use base 'Perl6::Perldoc::Block';

# Standard =END block...
package Perl6::Perldoc::Block::END;   
    use base 'Perl6::Perldoc::Block';

# Standard SEMANTIC blocks...
package Perl6::Perldoc::Semantic;
    use base 'Perl6::Perldoc::Block';
    
# For most semantic blocks, their title is their name, suitably de-shouted...
sub title {
    my ($self) = @_;
    my $title = ucfirst lc $self->{typename};
    
    if (exists $self->{number}) {
        $title = $self->is_post_numbered  ?  "$title $self->{number}"
               :                             "$self->{number}. $title"
               ;
    }

    return $title;
};

package Perl6::Perldoc::Block::ACKNOWLEDGEMENT;
                                            use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::ACKNOWLEDGEMENTS;
                                            use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::APPENDICES;  use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::APPENDIX;    use base 'Perl6::Perldoc::Semantic';
sub is_post_numbered {1}

package Perl6::Perldoc::Block::APPENDIXES;  use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::AUTHOR;      use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::AUTHORS;     use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::BUG;         use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::BUGS;        use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::CHAPTER;     use base 'Perl6::Perldoc::Semantic';
sub is_post_numbered {1}

package Perl6::Perldoc::Block::CHAPTERS;    use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::COPYRIGHT;   use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::COPYRIGHTS;  use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::DEPENDENCIES;
                                            use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::DEPENDENCY;  use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::DESCRIPTION; use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::DESCRIPTIONS;
                                            use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::DIAGNOSTIC;  use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::DIAGNOSTICS; use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::DISCLAIMER;  use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::DISCLAIMERS; use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::ERROR;       use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::ERRORS;      use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::FOREWORD;    use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::FOREWORDS;   use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::INDEX;       use base 'Perl6::Perldoc::Semantic';
sub is_post_numbered {1}

package Perl6::Perldoc::Block::INDEXES;     use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::INDICES;     use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::INTERFACE;   use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::INTERFACES;  use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::LICENCE;     use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::LICENCES;    use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::LICENSE;     use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::LICENSES;    use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::METHOD;      use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::METHODS;     use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::NAME;        use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::NAMES;       use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::OPTION;      use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::OPTIONS;     use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::SECTION;     use base 'Perl6::Perldoc::Semantic';
sub is_post_numbered {1}

package Perl6::Perldoc::Block::SECTIONS;    use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::SUBROUTINE;  use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::SUBROUTINES; use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::SUMMARIES;   use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::SUMMARY;     use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::SYNOPSES;    use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::SYNOPSIS;    use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::TITLE;       use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::TITLES;      use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::USAGE;       use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::USAGES;      use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::VERSION;     use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::VERSIONS;    use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::WARNING;     use base 'Perl6::Perldoc::Semantic';
package Perl6::Perldoc::Block::WARNINGS;    use base 'Perl6::Perldoc::Semantic';

# A few semantic classes need to translate their titles...
package Perl6::Perldoc::Block::TOC;         use base 'Perl6::Perldoc::Semantic';
sub title { return 'Table of Contents'; }

package Perl6::Perldoc::Block::TOCS;        use base 'Perl6::Perldoc::Semantic';
sub title { return 'Tables of Contents'; }

package Perl6::Perldoc::Block::SEEALSO;     use base 'Perl6::Perldoc::Semantic';
sub title { return 'See Also'; }

package Perl6::Perldoc::Block::SEEALSOS;    use base 'Perl6::Perldoc::Semantic';
sub title { return 'See Also'; }

# Base class for formatting codes...
package Perl6::Perldoc::FormattingCode; 
    use base 'Perl6::Perldoc::Root';

# Basis text formatter...
package Perl6::Perldoc::FormattingCode::B;
    use base 'Perl6::Perldoc::FormattingCode';

# Code formatter...
package Perl6::Perldoc::FormattingCode::C;
    use base 'Perl6::Perldoc::FormattingCode';

# Definition formatter...
package Perl6::Perldoc::FormattingCode::D;
    use base 'Perl6::Perldoc::FormattingCode';

# The "definition" formatting code must extract its synonyms...
sub new {
    my ($classname, $data_ref) = @_;

    if (my $synonyms = delete $data_ref->{target}) {
        $data_ref->{synonyms} = [split /;/, $synonyms];
    }

    return $classname->SUPER::new($data_ref);
}

sub synonyms {
    my ($self) = @_;
    my $vals_ref = $self->{synonyms};
    if (!wantarray) {
        if (@{ $vals_ref } > 1) {
            require Carp and Carp::carp(
                "Multivalued accessor synonyms() called in scalar context"
            );
        }
        return $vals_ref->[0];
    }
    return @{ $vals_ref };
}


# Entity formatter...
package Perl6::Perldoc::FormattingCode::E;
    use base 'Perl6::Perldoc::FormattingCode';

# Important text formatter...
package Perl6::Perldoc::FormattingCode::I;
    use base 'Perl6::Perldoc::FormattingCode';

# Keyboard input formatter...
package Perl6::Perldoc::FormattingCode::K;
    use base 'Perl6::Perldoc::FormattingCode';

# Link formatter...
package Perl6::Perldoc::FormattingCode::L;
    use base 'Perl6::Perldoc::FormattingCode';

# Flatten a hierarchical data structure into a suitable link target...
use Scalar::Util qw< reftype >;
sub _flatten {
    my $flat_version = _flatten_recursive(@_);
    $flat_version =~ s{\n}{ }gxms;
    $flat_version =~ s{\A \s+ | \s+ \z}{}gxms;
    return $flat_version;
}

sub _flatten_recursive {
    my ($data) = @_;
    my $class = ref($data) || q{};
    $class =~ s{.*::}{}xms;
    my $type = reftype($data) || q{};
    if ($type eq 'HASH') {
        return q{} if $data->{ignore_toc};
        return "$class<" . _flatten_recursive($data->{content}) . '>';
    }
    elsif ($type eq 'ARRAY') {
        return join q{}, map {_flatten_recursive($_)} @{$data};
    }
    else {
        return $data || q{};
    }
}

# The content of a link is its default target...
sub new {
    my ($classname, $data_ref) = @_;
    my $self = $classname->SUPER::new($data_ref);

    if (!$self->{target}) {
        $self->{target} = _flatten($self->{content});
        $self->{has_no_text} = 1;
    }

    return $self;
}

sub has_distinct_text {
    my ($self) = @_;
    return !$self->{has_no_text};
}

# Meta-formatter...
package Perl6::Perldoc::FormattingCode::Named;
    use base 'Perl6::Perldoc::FormattingCode';

package Perl6::Perldoc::FormattingCode::M;
    use base 'Perl6::Perldoc::FormattingCode';

# The user-defined formatting code is open ended...
sub new {
    my ($classname, $data_ref, $opt_ref) = @_;
    
    my $content_ref
        = defined $data_ref->{content} ? $data_ref->{content} : q{};
    
    # Install the necessary class to support this user-defined code...
    if ($content_ref->[0] =~ s{\A \s* ($QUAL_IDENT) \s* :}{}xms) {
        $classname = "Perl6::Perldoc::FormattingCode::Named::$1";
        no strict qw< refs >;
        push @{"${classname}::ISA"}, "Perl6::Perldoc::FormattingCode::Named";
    }
    # If there's no class specified, it's an error...
    else {
        Perl6::Perldoc::Parser::_err_missing_M_scheme(
            $data_ref->{range}, $opt_ref->{errors}
        );
    }
    return $classname->SUPER::new($data_ref);
}

# Note formatter...
package Perl6::Perldoc::FormattingCode::N;
    use base 'Perl6::Perldoc::FormattingCode';

# Placement link formatter...
package Perl6::Perldoc::FormattingCode::P;
    use base 'Perl6::Perldoc::FormattingCode';

# The content of a link is always its target...
sub new {
    my ($classname, $data_ref) = @_;
    my $self = $classname->SUPER::new($data_ref);

    $self->{target} = join q{}, @{$self->{content}};

    return $self;
}


# Replacable item formatter...
package Perl6::Perldoc::FormattingCode::R;
    use base 'Perl6::Perldoc::FormattingCode';

# Space-preserving formatter...
package Perl6::Perldoc::FormattingCode::S;
    use base 'Perl6::Perldoc::FormattingCode';

# Terminal output formatter...
package Perl6::Perldoc::FormattingCode::T;
    use base 'Perl6::Perldoc::FormattingCode';

# Unusual text formatter...
package Perl6::Perldoc::FormattingCode::U;
    use base 'Perl6::Perldoc::FormattingCode';

# Verbatim formatter...
package Perl6::Perldoc::FormattingCode::V;
    use base 'Perl6::Perldoc::FormattingCode';

# indeX formatter...
package Perl6::Perldoc::FormattingCode::X;
    use base 'Perl6::Perldoc::FormattingCode';

# Index entries have to be extracted from the index target...
sub new {
    my ($classname, $data_ref) = @_;

    if (my $entries = delete $data_ref->{target}) {
        $data_ref->{entries} = [split /;/, $entries];
    }

    return $classname->SUPER::new($data_ref);
}

sub entries {
    my ($self) = @_;
    my $vals_ref = $self->{entries};
    if (!wantarray) {
        if (@{ $vals_ref } > 1) {
            require Carp and Carp::carp(
                "Multivalued accessor entries() called in scalar context"
            );
        }
        return $vals_ref->[0];
    }
    return @{ $vals_ref };
}


# Zero-width formatter...
package Perl6::Perldoc::FormattingCode::Z;
    use base 'Perl6::Perldoc::FormattingCode';


# Standard =table block...
package Perl6::Perldoc::Block::table;   
    use base 'Perl6::Perldoc::Block';

# A table's caption is its title...
sub title {
    my ($self) = @_;
    if (my $title = $self->caption({ as_objects => 1 })) {
        return $title;
    }
    return;
}

# Rows accessor...
sub rows {
    my ($self) = @_;
    my $vals_ref = $self->{rows};
    if (!wantarray) {
        if (@{ $vals_ref } > 1) {
            require Carp and Carp::carp(
                "Multivalued accessor rows() called in scalar context"
            );
        }
        return $vals_ref->[0];
    }
    return @{ $vals_ref };
}

# Ctor needs to build table by parsing raw contents of block...
sub new {
    my ($classname, $data, $opt_ref) = @_;

    $data->{rows} = _build_table(
                        $data->{content}[0],
                        $data->{allow},
                    );

    return $classname->SUPER::new($data);
}

# Regexes to help with table parsing...
my $HWS            = qr{ [ \t] }xms;

my $COL_SEP        = qr{ $HWS* [|+]{1,2} | $HWS{2,}     }xms;
my $ROW_SEP_LINE   = qr{ ^ [-=_ \t|+]*  \n }xms;

# Utility maximum routine:

sub _max {
    my $max = shift;

    for my $next (@_) {
        if ($next > $max) {
            $max = $next;
        }
    }

    return $max;
}

# Build an unpack template of the table column layout...
sub _column_template {
    my @lines = split /\n/, shift;

    my $max_width = _max(map {length} @lines);

    # Detect rivers...
    my %rivers;
    my %is_visible;
    for my $line (@lines) {
        # Hide single and double spaces...
        $line =~ s/[^\s|+][ ][^\s|+]/***/g;
        $line .= q{ } x ($max_width - length $line);

        # Check each position for a column boundary character...
        my @char = split(//, $line);
        for my $pos (0..$#char) {
            my $char = $char[$pos];
            if ($char =~ m{[-=_ ]}) {
                $rivers{$pos}++;
            }
            elsif ($char =~ m{[|+]}) {
                $rivers{$pos}++;
                $is_visible{$pos} = 1;
            }
        }
    }

    # Remove partial rivers...
    delete @rivers{grep { ($rivers{$_}||0) < @lines } keys %rivers};

    # Fill river positions with '1' (or '2' if a visible boundary)...
    my $template = '0' x $max_width;
    for my $pos (keys %rivers) {
        substr($template,$pos,1,$is_visible{$pos} ? 2 : 1);
    }

    # Rivers with visible boundaries are only rivers in the visible bits...
    $template =~ s{ (1*)(2+)(1*) }{ 0 x length($1) . 1 x length($2) . 0 x length($3) }egxms;

    # Add any missing external boundaries...
    my $prefix  = $template =~ /^0/ ? 'A0' : q{};
    my $postfix = $template =~ /0$/ ? 'A0' : q{};

    # Convert bitmap to an 'unpack' extractor...
    $template =~ s{ (1+ | 0+) }{ 'A'.length($1) }egxms;

    # Return extractor...
    return $prefix.$template.$postfix;
}

# Build list of individual table rows for given separators...
sub _build_table_rows {
    my ($text, $has_head, $cells_ref, $seps_ref, $allow_ref) = @_;

    # Get extract template and subdivide cells:
    my $extractor = _column_template($text);

    # Parse rows and build representations...
    my @rows;
    for my $row_index (0..$#{$cells_ref}) {
        # Extract top and bottom row separators...
        my ($pre_sep, $post_sep)
            = map { [ unpack $extractor, $_ ] }
                @{$seps_ref}[$row_index, $row_index+1];

        # Extract cells themselves...
        my @cells;
        for my $line (split /\n/, $cells_ref->[$row_index]) {
            my @cols = unpack $extractor, $line;
            for my $col_index (0..$#cols) {
                push @{$cells[$col_index]}, $cols[$col_index];
            }
        }

        # Recombine the cells...
        my @cell_objs;
        my $left_sep = shift @cells;
        shift @{$pre_sep};
        shift @{$post_sep};

        CELL:
        while (@cells) {
            my ($cell, $right_sep) = splice(@cells, 0, 2);

            next CELL if @{$cell} == grep /\A (\s* \|) \s* \Z/xms, @{$cell};

            my ($top)    = splice(@{$pre_sep}, 0, 2);
            my ($bottom) = splice(@{$post_sep}, 0, 2);

            my $content = join("\n", @{$cell});

            # Remove common horizontal whitespace prefix...
            if ($content =~ m{\A ([^\S\n]+)}xms) {
                my $prefix = $1;
                $content =~ s{^$prefix}{}gms;  # No /x so whitespace significant
            }

            open my $fh, '<', \$content
                or die "Internal error: could not parse table content";

            # Recursively parse content as Pod...
            $content
                = Perl6::Perldoc::Parser->parse($fh,
                    {all_pod=>1, allow=>$allow_ref}
                  )->{tree}->{content};

            # Add cell to list for row...
            push @cell_objs, bless {
                content => $content,
                left    => join("\n", @{$left_sep}),
                right   => join("\n", @{$right_sep}),
                top     => $top,
                bottom  => $bottom,
                header  => $has_head && $row_index == 0,
            }, 'Perl6::Perldoc::Block::table::Cell';

            # Move left (right separator becomes left separator)
            $left_sep = $right_sep;
        }

        # Add the new row object...
        push @rows, bless {
             cells => \@cell_objs,
        }, 'Perl6::Perldoc::Block::table::Row';

        # Move downwards...
        $pre_sep = $post_sep;
    }

    return \@rows;
}

# Build entire table...
sub _build_table {
    my ($text, $allow_ref) = @_;

    # Remove surrounding blank lines...
    $text =~ s{\A ($HWS* \n)+ | (^ $HWS* \n?)+ \z}{}gxms;

    # Remove top/bottom border...
    $text =~ s{\A ($ROW_SEP_LINE)}{}xms;     my $top_sep    = $1 || q{};
    $text =~ s{\n ($ROW_SEP_LINE) \Z}{}xms;  my $bottom_sep = $1 || q{};

    # Decompose into separated rows...
    my ($first_row, $first_sep, @rest) = split m{($ROW_SEP_LINE)}xms, $text;
    my $has_head = @rest != 0;

    my @rows = @rest == 0 ? (split m{(\n)}xms, $text)
             : @rest == 1 ? ($first_row, $first_sep, split m{(\n)}xms, $rest[0])
             :              ($first_row, $first_sep, @rest)
             ;

    my @separators = ($top_sep, @rows[grep {$_%2!=0} 0..$#rows], $bottom_sep);
    my @cells      = @rows[grep {$_%2==0} 0..$#rows];

    return _build_table_rows($text, $has_head, \@cells, \@separators, $allow_ref);
}

# Class to represent individual table row...
package Perl6::Perldoc::Block::table::Row;

# Read-only accessor for individual cells...
sub cells {
    my ($self) = @_; 
    my $vals_ref = $self->{cells};
    if (!wantarray) {
        if (@{ $vals_ref } > 1) {
            require Carp and Carp::carp(
                "Multivalued accessor cells() called in scalar context"
            );
        }
        return $vals_ref->[0];
    }
    return @{ $vals_ref };
}

# Class to represent individual table cell...
package Perl6::Perldoc::Block::table::Cell;

# Read-only content accessor...
sub content {
    my ($self) = @_; 
    my $vals_ref = $self->{content};
    if (!wantarray) {
        if (@{ $vals_ref } > 1) {
            require Carp and Carp::carp(
                "Multivalued accessor content() called in scalar context"
            );
        }
        return $vals_ref->[0];
    }
    return @{ $vals_ref };
}

# Is this a header row?
sub is_header {
    my ($self) = @_; 
    return $self->{header};
}


1;

__END__

=head1 NAME

Perl6::Perldoc::Parser - Parse Perl 6's documentation mark-up language


=head1 VERSION

This document describes Perl6::Perldoc::Parser version 0.0.1


=head1 SYNOPSIS

    use Perl6::Perldoc::Parser;

    $representation = Perl6::Perldoc::Parser->parse($file, \%options);

    $errors   = $representation->{errors};
    $warnings = $representation->{warnings};

    $obj_tree = $representation->{tree};

  
=head1 DESCRIPTION

This module parses text marked up with the Perl 6 Pod notation and
converts it to a hierarchical object-based representation.


=head1 MODULE INTERFACE 

=head2 C<< $rep = Perl6::Perldoc::Parser->parse($file, \%options) >>

The C<parse()> method expects a filename or input filehandle as its first
argument, and (optionally) a reference to a hash of options as its second.
The options that can be passed in this second argument are:

=over

=item C<< all_pod => $status >>

If $status is true, specifies that the entire text should be considered
to be Pod. Any text not inside a Pod block will be treated as a plain
paragraph or code block, rather than as ambient source code. Specifying
this option is the same as placing a C<=begin pod>/C<=end pod> block
around the entire text.

If $status is false, specifies that the text should be considered to be
heterogeneous: a mixture of Pod and source code. Any text not
inside a Pod block will be treated as ambient source code.

As a specical if $status is the string C<'auto'>, the option will be
automatically set by looking at the filename passed to C<parse()>. If that
filename ends in '.pod6' or '.pod', the option will be set true.

Defaults to false when C<parse()> is passed a filehandle and C<'auto'> when
C<parse()> is passed a filename.

=item C<< allow => \%allowed >>

Specifies that the formatting codes whose names appear as keys of the hash
value are to be allowed within verbatim blocks.
For example, to universally allow the C<< EZ<><> >> and C<< LZ<><> >> codes with
otherwise verbatim text:

    Perl6::Perldoc::Parser->parse($file, { allow => {E=>1, L=>1} });

Defaults to no allowed codes.

=back

Text is read from the file and parsed as Perl 6 Pod. The method call
returns a hash containing three entries:

=over

=item C<< $rep->{tree} >>

The root object of a hierarchical representation of the tree (see L<"DOM
INTERFACE>).

=item C<< $rep->{errors} >>

A reference to an array of error messages generated during the parse. If
this array is non-empty then the parse failed and the resulting object
tree is not guaranteed to be correct. It is suggested that if C<parse()>
returns a non-empty C<< $rep->{errors} >> the application calling it
should report the errors and abort.

=item C<< $rep->{warnings} >>

A reference to an array of warning messages generated during the parse. If
this array is non-empty the parse probably succeeded and the resulting object
tree is very likely to be correct. It is suggested that if C<parse()> returns
a non-empty C<< $rep->{warnings} >> the application calling it should report the
warnings before continuing.

=back

=head2 C<< $rep->report_errors(@optional_message) >>

This method can be called on the hash returned by C<parse()>. It prints
to STDERR any errors or warnings returned from the parse and then throws
an exception (containing the optional message) if there were any errors.

If there are no errors, it returns its own invocant, so it can be chained
directly to the end of an actual parse:

    $rep = Perl6::Perldoc::Parser->parse($file, \%options)
                                 ->report_errors('Bad pod');

=head1 DOM INTERFACE 

The class hierarchy of the objects returned by C<parse()> is as follows:
 
    (All classes prefixed with Perl6::Perldoc::)

    Root
        File
        Ambient
        Directive
            Directive::config
            Directive::use
            Directive::encoding
        Block
            Block::pod
            Block::para
            Block::code
            Block::input
            Block::output
            Block::Named
                Block::Named::Whatever
                Block::Named::WhateverElse
                etc.
            Block::head
                Block::head1
                Block::head2
                Block::head3
                Block::head4
                    Block::head5
                    Block::head6
                    etc.
            Block::list
            Block::item
                Block::item1
                Block::item2
                Block::item3
                etc.
            Block::nested
            Block::comment
            Block::table
            Block::table::Row
            Block::table::Cell
            Block::toclist
            Block::tocitem
                Block::tocitem1
                Block::tocitem2
                Block::tocitem3
                Block::tocitem4
                Block::tocitem5
            Block::Semantic
                Block::NAME
                Block::VERSION
                Block::SYNOPSIS
                Block::DESCRIPTION
                etc.
        FormattingCode
            FormattingCode::B
            FormattingCode::C
            FormattingCode::D
            FormattingCode::E
            FormattingCode::I
            FormattingCode::K
            FormattingCode::L
            FormattingCode::M
            FormattingCode::Named
                FormattingCode::Named::Whatever
                FormattingCode::Named::WhateverElse
                etc.
            FormattingCode::N
            FormattingCode::P
            FormattingCode::R
            FormattingCode::S
            FormattingCode::T
            FormattingCode::U
            FormattingCode::V
            FormattingCode::X
            FormattingCode::Z


Every class has a C<new()> constructor, which expects its first argument to be
a reference to a hash containing the parsed information for the block. The
second, optional argument is a reference to a hash containing any of the
global options that may be passed to C<Perl6::Perldoc::Parser::parse()>.

The C<Perl6::Perldoc::Root> class (and hence every other class in the
DOM hierrachy) has the following methods available, all of which are
currently read-only accessors:

=over

=item C<typename()>

Returns the name of the block type, typically the same as the last
component of the object's classname. Handy for text (re)generation, but
consider using polymorphic methods instead of switching on this value.

=item C<style()>

Returns the style of block that the object was generated from. The
possibilities are: 

=over

=item C<'delimited'>

The object was derived from a block that was specified in
C<=begin>/C<=end> markers

=item C<'paragraph'>

The object was derived from a block that was specified
with a C<=for> marker

=item C<'abbreviated'>

The object was derived from a block that was specified
using the short-form C<=I<typename>> syntax

=item C<'directive'>

The object was derived from a C<=use>, C<=config>, or C<=encoding> directive

=item C<'formatting'>

The object was derived from a formatting code.

=item C<'implicit'>

The object was created internally by the parser. Such objects are
typically list containers, top-level pod blocks, or representations of
raw code or text paragraph blocks.

=back

=item C<content()>

Returns a list of objects and/or strings representing the content of the
block. Objects always represent nested blocks; strings are always unformatted
text.

=item C<range()>

Returns a reference to a hash specifying the range of lines in which the
corresponding block was defined. The entries of the hash are:

    $obj->range->{from}     # Line at which block opened
    $obj->range->{to}       # Line at which block closed
    $obj->range->{file}     # File in which block opened

=item C<number()>

Returns the hierarchical number of the block within its block type. Will be
undefined if the block was not numbered, so typically only meaningful for
headers and list items.

=item C<is_numbered()>

Returns true if the block has a C<:numbered> option specified (either
explicitly, or by preconfiguration).

=item C<is_post_numbered()>

Returns true if the block is special in that its number should appear at the
end of its content, not at the start. Typically this is true for certain types
of semantic block (for example: C<=CHAPTER>) where a rendering such as:

    Chapter 1

makes more sense than:

    1. Chapter

User-defined block are often defined to have their C<is_post_numbered()>
methods return true as well. For example:

    for Image :numbered :caption<Our mascot> :source<file:images/camel.jpg>

is better captioned with post-numbering:

    Image 7: Our mascot
    

=item C<config()>

Returns a reference to a nested hash containing the configuration
(i.e. C<=config>) environment in effect for the block. Each top-level 
key of the hash is the name of a block type being configured,
each second-level hash contains the configuration options for that block type.

=item C<option( $opt_name )>

Returns the value of the named option for the specific block object. This
value may be derived from an explicit option on the declaration, or implicitly
from the configuration for the block.

=item C<term( \%options )>

Returns the value of the "term" option of the block. Typically this will
be C<undef> unless the block is an C<=item>.

The "term" value is normally returned as a raw string, but you can
have the method return a fully parsed Pod subtree by specifying an
option on the call:

    $pod_tree = $item->term({ as_objects => 1 })


=item C<caption( \%options )>

Returns the value of the "caption" option of the block. This is most often
used for C<=table> blocks, but any block may be given a caption.

The "caption" value is normally returned as a raw string, but you can
have the method return a fully parsed Pod subtree by specifying an
option on the call:

    $pod_tree = $item->caption({ as_objects => 1 })

=back

Some DOM classes offer additional methods, as follows:

=head2 Perl6::Perldoc::Directive::config

=over

=item C<target()>

Returns the typename of the block type that the corresponding C<=config>
directive configures.

=back


=head2 Perl6::Perldoc::Block::table

=over

=item C<rows()>

Returns a list of C<Perl6::Perldoc::Block::table::Row> objects, representing
the rows of the table.

=back


=head2 Perl6::Perldoc::Block::table::Row

=over

=item C<cells()>

Returns a list of C<Perl6::Perldoc::Block::table::Cell> objects, representing
the cells of the table row.

=back


=head2 Perl6::Perldoc::Block::table::Cell

=over

=item C<is_header()>

Returns true if the corresponding table cell is in the header row.

=back


=head2 Perl6::Perldoc::FormattingCode::D

=over

=item C<synonyms()>

Returns a list of strings containing the specified synonyms for the
correspondinging C<< DZ<><> >> definition.

=back


=head2 Perl6::Perldoc::FormattingCode::L

=over

=item C<has_distinct_text()>

Returns true if the formatting code was specified with both a display
text and a seperate target URI. For example, the method would return
true for an object representing:

    L<The Perl development page|http://dev.perl.org>

but would return false for an object representing:

    L<http://dev.perl.org>

=back


=head2 Perl6::Perldoc::FormattingCode::L and Perl6::Perldoc::FormattingCode::P

=over

=item C<target()>

Returns a string containing the target URI of the C<< LZ<><> >> or C<< PZ<><>
>> formatting code represented by the object.

=back


=head2 Perl6::Perldoc::FormattingCode::X

=over

=item C<entries()>

Returns a list of strings containing the index entries for the
corresponding C<< XZ<><> >> formatting code.

=back


=head1 DIAGNOSTICS

=over

=item parse() can't open file %s

The C<parse()> method expects as its first argument either an open
filehandle or else a string containing a filename. If the argument isn't
a filehandle, it's assumed to be a filename. This error indicates that
assumption proved to be wrong and that something unexpected was passed
instead.


=item Unable to open URI in '=use %s'

This parser only handles C<file:I<path>> and C<perl5:I<module>> style URIs in
an C<=use> directive. The Pod being parsed had something else.


=item Missing scheme specifier in MZ<><> formatting code

An C<< MZ<><> >> formatting code must start with a scheme/class name,
followed by a colon. For example:
    
    M<Image: logo.gif>

That initial identifier was missing. For example:

    M<logo.gif>


=item No =item%d before =item%d

An C<=item2> can only appear after an C<=item1>; an C<=item3>, only after
an C<=item2>; etc. 

A common mistake that produces this error is to physically nest
C<=item> markers:

    =begin item1
    The choices are:
    =item2 Tom Swift
    =item2 Dick Wittington
    =item2 Harry Houdini
    =end item1

Items are not physically nested in Pod; they are logically nested. The
workaround is to rewrite the Pod without nested items:

    =item1 The choices are:
    =item2 Tom Swift
    =item2 Dick Wittington
    =item2 Harry Houdini


=item Ignored explicit '=end END'

C<END> blocks, no matter how they're specified, run from the line at which
they're opened to the very end of the file. An explicit C<=end END> is always
ignored (and should be removed, because it's misleading).


=item Invalid '=end %s' (not in %s block)

The parser came across an C<=end> marker for a block that isn't open at that
point. This is usually caused by either misspelling the block name, or
accidentally closing an outer block before an inner one.


=item Possible attempt to specify extra options too late in %s block

Extra options on a block are specified by lines immediately after the block
declarator that start with an C<=>, followed by whitespace:

    =begin SomeBlock :option(1)
    =                :extra_option
    =                :yet_another<here>

As soon as a line that doesn't start with an C<=> is encountered, the
rest of the block is considered to be content. So any line that begins
with an C<=> after that point is content, not configuration:

    =begin SomeBlock :option(1)

    = :this<content> :!config

Such lines are reported as possible mistakes.


=item Unknown reserved block type (%s)

Block names that consist of entirely uppercase or entirely lowercase
identifiers are reserved for Pod itself. User-defined block types must be
mixed-case. The Pod that was parsed contained an reserved identifier that the
parser did not recognize. This is reported as a possible future-compatibility
problem.


=item Trailing junk after %s

Any options on a block must be specified in the Perl 6 C<:name(value)>
option syntax (or any of its variations). Anything else on an option line is
invalid, and reported as "trailing junk".


=item No closing delimiter for %s block opened at line %s

There was an unbalanced C<=begin> in the Pod. This is often caused by typos
in the (supposedly) matching C<=end> directive.


=item Multivalued accessor %s called in scalar context

Some DOM object accessor methods (for example:
C<Perl6::Perldoc::Root::content()>) return a list of values in list
context. If these accessors are called in scalar context, only the first
value in the list is returned. However, if there is more than one value in the
list, a scalar-context call is a source of potential errors, so this
warning is issued.


=item Internal error: %s

The module's internal diagnostics detected a problem in the implementation
itself.  There is nothing you can do about this error, except report it.

=back

=head1 CONFIGURATION AND ENVIRONMENT

Perl6::Perldoc::Parser requires no configuration files or environment
variables.


=head1 DEPENDENCIES

version.pm


=head1 INCOMPATIBILITIES

None reported.


=head1 LIMITATIONS

=over 

=item *

This parser does not currently fully support C<=use> directives. In
particular, only the forms:

    =use file:path/to/file
    =use      path/to/file

and:

    =use perl5:Module::Name  :options(here)
    =use       Module::Name  :options(here)

are supported.

=item * 

The C<=encoding> directive is parsed and internally represented, but ignored.


=back


=head1 BUGS

=over 

=item * 

The parser does not assume a default encoding of UTF-8 (as per the
specification in Synopsis 26).

=back 

Please report any bugs or feature requests to
C<bug-perldoc-parser@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.


=head1 AUTHOR

Damian Conway  C<< <DCONWAY@CPAN.org> >>


=head1 LICENCE AND COPYRIGHT

Copyright (c) 2006, Damian Conway C<< <DCONWAY@CPAN.org> >>. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.


=head1 DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE
ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH
YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL
NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL,
OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE
THE SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.
