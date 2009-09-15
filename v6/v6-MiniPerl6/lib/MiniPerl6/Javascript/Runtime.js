// lib/MiniPerl6/Javascript/Runtime.js

// class Main
if (typeof Main != 'function') {
  Main = function() {};
  Main = new Main;
}
(function () {
  // method newline
  Main.f_newline = function () {
    return "\n";
  }
  Main.f_newline;  // v8 bug workaround
})();

