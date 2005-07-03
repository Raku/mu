
use v6;

use Tree::Visitor;

class Tree::Visitor::FindByPath-0.0.1 is Tree::Visitor;

has @:search_path;
has $:success;

sub set_search_path ($self: @path) {
    (@path) || die "Insufficient Arguments : You must specify a path";
    @:search_path = @path;
}

sub visit ($self: $tree) {

    # reset our success flag
    $:success = 0;

    # get our filter function
	my $func;
    if $self.has_node_filter() {
        $func = -> ($tree, $test) { (($self.get_node_filter().($tree) ~ "") eq $test) };    
    }
    else {
        $func = -> ($tree, $test) { (($tree.node() ~ "") eq $test) };  
    }

    # get ready with our results
    my @results;

    # get our path
    my @path = @:search_path;    

    # get our variables ready
    my $current_path;
    my $current_tree = $tree;

    # check to see if we have been 
    # asked to include the trunk
    if $self.include_trunk() {
        # if we dont match the root of the path
        # then we have failed already and so return
        return unless $func($current_tree, @path[0]);
        # if we do match, then store the tree in our results
        @results.push($current_tree);
    }

    TOP: {
        # if we have no more @path we have found it
        unless @path {
            # store the current tree as
            # our last result
            @results.push($current_tree);
            $self.set_results(@results);
            # and set the sucess flag
            $:success = 1;
            return;
        }
        # otherwise we need to keep looking ...
        # get the next element in the path
        $current_path = @path.shift;
        # now check all the current tree's children
        # for a match
        for $current_tree.get_all_children() -> $child {
            if $func($child, $current_path) {
                # if we find a match, then
                # we store the current tree 
                # in our results, and 
                @results.push($current_tree);
                # we change our current tree
                $current_tree = $child;
                # and go back to the TOP                
                goto TOP;
            }
        }
        # if we do not find a match, then we can fall off 
        # this block and the whole subroutine for that matter
        # since we know the match has failed.
    }
    # we do however, store the 
    # results as far as we got,
    # so that the user can maybe 
    # do something else to recover
    $self.set_results(@results);
}

sub get_result ($self:) {
    # if we did not succeed, then 
    # we return undef, ...
    return undef unless $:success;
    # otherwise we return the 
    # last in the results
    return ($self.get_results())[-1];
}
