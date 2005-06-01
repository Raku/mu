
role Duration;

has Date $.start;
has Date $.end;
has Num $.length;

method add($self: Duration $duration) returns Duration {
    $self.new($self.length + $duration.length);
}

method subtract($self: Duration $duration) returns Duration {
    $self.new($self.length - $duration.length);
}

method infix:<->($self: Duration $duration) returns Duration {
    $self.subtract($duration);
}

method infix:<+>($self: Duration $duration) returns Duration {
    $self.add($duration);
}
