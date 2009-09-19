// lib/MiniPerl6/Javascript/Runtime.js

// class Main
if (typeof Main != 'object') {
  Main = function() {};
  Main = new Main;
}
(function () {
  // method newline
  Main.f_newline = function () {
    return "\n";
  }
  Main.f_lisp_escape_string = function (s) {
    var o = s;
    o.replace( /\\/g, "\\\\");
    o.replace( /"/g, "\\\"");
    return(o);
  }

})();

if (typeof MiniPerl6$Match != 'object') {
  MiniPerl6$Match = function() {};
  MiniPerl6$Match = new MiniPerl6$Match;
}
v_MATCH = { __proto__:MiniPerl6$Match };

MiniPerl6$Match.f_hash = function () { return this }
f_scalar = function (o) { return o.f_scalar() }


// regex primitives
if (typeof MiniPerl6$Grammar != 'object') {
  MiniPerl6$Grammar = function() {};
  MiniPerl6$Grammar = new MiniPerl6$Grammar;
}
MiniPerl6$Grammar.f_word = function (v_str, v_pos) { 
    return {           
            __proto__:MiniPerl6$Match, 
            v_str:  v_str,
            v_from: v_pos, 
            v_to:   v_pos + 1,
            v_bool: v_str.substr(v_pos, 1).match(/\w/)
        };
} 
MiniPerl6$Grammar.f_digit = function (v_str, v_pos) { 
    return {           
            __proto__:MiniPerl6$Match, 
            v_str:  v_str,
            v_from: v_pos, 
            v_to:   v_pos + 1,
            v_bool: v_str.substr(v_pos, 1).match(/\d/)
        };
} 
MiniPerl6$Grammar.f_space = function (v_str, v_pos) { 
    return {           
            __proto__:MiniPerl6$Match, 
            v_str:  v_str,
            v_from: v_pos, 
            v_to:   v_pos + 1,
            v_bool: v_str.substr(v_pos, 1).match(/\s/)
        };
} 
MiniPerl6$Grammar.f_is_newline = function (v_str, v_pos) { 
    var m_ = v_str.substr(v_pos).match(/^(\r\n?|\n\r?)/);
    return {           
            __proto__:MiniPerl6$Match, 
            v_str:  v_str,
            v_from: v_pos, 
            v_to:   m_ != null ? v_pos + m_[0].length : v_pos,
            v_bool: m_ != null,
        };
} 
MiniPerl6$Grammar.f_not_newline = function (v_str, v_pos) { 
    return {           
            __proto__:MiniPerl6$Match, 
            v_str:  v_str,
            v_from: v_pos, 
            v_to:   v_pos + 1,
            v_bool: v_str.substr(v_pos, 1).match(/[^\r\n]/)
        };
} 
