:: usage:  perl6 1   (for trace mode)   perl6   (for non-trace mode)
@set TRACE=%1
@set DEBUG=%2
@if not %DEBUG%a == a set DEBUG=-d
:: I use perl 5.10.0 (strawberry), but 5.8.x should work.
:: pipe input version
@if not %TRACE%a == a cmd /c type foo|perl %DEBUG% perl6 2>foo1
@if %TRACE%a == a cmd /c type foo|perl %DEBUG% perl6
:: file reading input version
:: @perl perl6 d:\source\pugs\misc\yap6\src\t\foo
