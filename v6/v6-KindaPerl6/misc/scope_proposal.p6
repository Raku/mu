sub foo {
    my $a = 42;
    return sub {
        $a++;
    };
}

print foo()->(),"\n";
print foo()->(),"\n";
print foo()->(),"\n";
print foo()->(),"\n";

__END__
#is sort of

Closure.new(scope=>Scope.new(outer=>$scope),code=>sub($scope) {
    $scope<a> = 1;
    return Closure.new(scope=>Scope.new(outer=>$scope,code=>sub($scope) {
        $scope<a>++;
    }));
})
