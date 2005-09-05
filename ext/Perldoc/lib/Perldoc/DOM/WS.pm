class Perldoc::DOM::WS is Perldoc::DOM::Text;

# the mutator for $.content should disallow storing of spaces...

#sub content {
    #if ( @_ ) {
        ## FIXME - unicode :)
        #my $content = shift;
        #$content =~ /\S/
            #&& croak "tried to put non-whitespace in a whitespace node";
        #$self->{content} = $content;
    #} else {
        #$self->{content};
    #}
#}

sub event_type {
    "ignorable_whitespace"
}

=head1 NAME

Perldoc::DOM::WS - ignorable whitespace in a Perldoc::DOM tree

=head1 SYNOPSIS

See L<Perldoc::DOM::Node>.

=head1 DESCRIPTION

Sometimes you need to put in a little whitespace to fill an XML
document.  This node type is for that.

=head2 SUB-CLASS PROPERTIES

This node type keeps the C<source> property, and adds C<content>,
which is the whitespace to be represented in the normative XML.

=cut

