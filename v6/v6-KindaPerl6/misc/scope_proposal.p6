sub foo {
    my $a = 1;
    return sub {
        $a++;
    };
}

#is sort of

Closure.new(scope=>Scope.new(outer=>$scope),code=>sub($scope) {
    $scope<a> = 1;
    return Closure.new(scope=>Scope.new(outer=>$scope,code=>sub($scope) {
        $scope<a>++;
    }));
})
