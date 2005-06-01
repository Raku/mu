
role Date;

# return floating point seconds relative to 1st Jan 2000, UTC
method epoch($self:) returns Float {
    ...
}

# return a unix time_t
method time_t($self:) returns Int {
    ...
}

method add($self: Duration $duration) returns Date {
    ...
}

method subtract($self: Duration $duration) returns Date {
    $self.add(-$duration);
}

method difference($self: Date $other) returns Duration {
    ...
}

method infix:<->($self: Date $other) returns Duration {
    $self.difference($other);
}

method infix:<->($self: Duration $duration) returns Date {
    $self.subtract($duration);
}

method infix:<+>($self: Duration $duration) returns Date {
    $self.add($duration);
}

