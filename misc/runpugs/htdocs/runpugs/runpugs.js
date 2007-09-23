//debugging flag: 1 = enable, 0 = disable
var debug=0;

var histlist=new Array();
var histentry=0;
var sessionid=0;
var reldev=0;
var curpos=0;
var cmd = "";
var prompt = "pugs> ";
var cmds = new Array();
var theme = "wb_theme";
var fixedCharWidth;

//for tutorials & school ;-)
var tutorialMode = 0;
var slide = 0;
var FIRST_TUTORIAL_SLIDE = 2;
var slides = new Array(
    'default_info.html',
    '../p6_syntax_hilite/syntax_hilite.html',
    'tutorial/01_helloworld.html',
    'tutorial/02_vars.html',
    'tutorial/03_iteration_array.html',
    'tutorial/04_iteration_hash.html',
    'tutorial/05_iteration_many.html',
    'tutorial/06_conditionals.html',
    'tutorial/07_junctions.html',
    'tutorial/08_junctions_array.html',
    'tutorial/09_chained_comparison.html',
    'tutorial/10_io.html',
    'tutorial/11_io_iteration.html',
    'tutorial/12_io_reading.html',
    'tutorial/13_lists_reduction.html',
    'tutorial/14_lists_hyper.html',
    'tutorial/15_lists_cross.html',
    'tutorial/16_grammar.html',
    'tutorial/17_final.html'
);

 
//show the cursor
function showCursor() {
    var cmdLen = cmds.length;
    var cursorEl = (cmdLen == 0) ? "#d0" : "#d" + (cmdLen - 1);
    $(cursorEl).toggleClass('cursorOff');
    $(cursorEl).toggleClass('cursorOn');
    setTimeout('showCursor()',1000);
}

//move the cursor by css
function moveCursor() {
    var left = -(cmd.length-curpos) * fixedCharWidth;
    var cursorEl = "#d" + (cmds.length - 1);
    $(cursorEl).css('left',left + "px");
}

//show last command on console
function showCmd() {
    var cmdEl = "#c" + (cmds.length - 1);
    $(cmdEl).text(prompt + cmd);
}

//update the console
function updateConsole() {
    $.each(cmds,function(i,n) {
        if(i == 0) {
            //clear only on first usage...
            $("#tt").empty();
        }
        var l = (n == "") ? "&nbsp;" : n;
        var tr = "<tr><td><pre id='c" + i + 
            "' class='"+ theme +"'>" + l + "</pre>";
        if(i == cmds.length - 1) {
            tr += "<span id='d" + i + 
            "' class='cursorOff'>&nbsp;</span>";
        } 
        $("#tt").append(tr + "</td></tr>");
    });
    var scrollHeight = $("#termwin")[0].scrollHeight;
    $("#termwin").animate({ scrollTop: scrollHeight }, "fast")
}

//wait for when the document is ready
$(document).ready( function() {
    //display waiting msg and try to calculate width of 
    //a single fixed character...
    var msg = "Please wait while Pugs starts up...";
    $("#tt").empty();
    $("#tt").append("<tr><td><pre id='c0' class='wb_theme'>" + msg +
    "</pre><span id='d0' class='cursorOff'>&nbsp;</span></td></tr>");
    fixedCharWidth = ($("#c0")[0]) ? 
        $("#c0")[0].offsetWidth / msg.length : 8;

    //apply theme and animate logo
    $("#theme").change(function() {
        $("pre").toggleClass(theme);
        theme = $("#theme").val();
        $("pre").toggleClass(theme);
    });
    $("#logo").slideDown(2000);

    //repaint & start showing the cursor...
    showCursor();

    //attach keyboard listeners...
    $(document).keydown(function(event) {
        return onKeyDown(event);
    });
    $(document).keypress(function(event) {    
        return onKeyPress(event);
    });

    //attach pugs cleanup listener...
    $(window).unload(function() {
        //send :q to pugs on onload
        $.ajax({
            url: "/perl/runpugs.pl?" + "sessionid=" + sessionid + 
            "&reldev=1&ia=1&cmd=%3Aq",
            async: true
        });
    });

    //load default text into element id 'info'
    loadSlide();

    //start loading pugs session after page has loaded...
    $("#hidden_iframe").append(
        '<iframe src="/perl/runpugs.pl" id="scratch" name="scratch" ' +
        'style="visibility:hidden" width="700px" height="1px" ' +
        'onLoad="getreply()"></iframe>');
});

//load a slide into #info with an slideup/slidedown effect
function loadSlide() {
    if(debug) {
        alert("slide url to be loaded = '" + slides[slide] + "'");
    }
    $("#info").slideUp("fast");
    $.ajax({
        url: slides[slide],
        async: true,
        success: function(data) {
            if(debug) {
                alert("slide data received: " + data);
            }
            $("#info").html(data);
            $("#info").slideDown("slow");
        }
    });
}

//insert character 'ch' at index 'pos' in string str 
//and return the result
function insert(str,ch,pos) {
    var s = str.substring(0,pos);
    var t = str.substring(pos,str.length);
    return s + ch + t;
}

//focus on last command very hard ;-)
function focusOnCmd(e) {
    $(e).focus();
    var scrollHeight = $("#termwin")[0].scrollHeight;
    $("#termwin").animate({ scrollTop: scrollHeight }, "fast");
}

//handle ENTER key
function handleEnter() {
    var sessCmds=document.terminal.cmd.value + cmd;
    var tmpCmds=sessCmds.split(prompt);
    var tmpCmd=tmpCmds[tmpCmds.length-1];
    var trimTmpCmd = $.trim(tmpCmd);
    if(trimTmpCmd != "") {
        histlist.push(tmpCmd);
    }

    //for tutorial, back, next support
    if(trimTmpCmd == ":tutorial") {
        tutorialMode = !tutorialMode;
        slide = (tutorialMode) ? FIRST_TUTORIAL_SLIDE : 0;
        loadSlide();
    } else if (trimTmpCmd == ":back" || trimTmpCmd == ":next") {
        if(tutorialMode) {
            if(trimTmpCmd == ":back") {
                slide--;
                if(slide < FIRST_TUTORIAL_SLIDE) {
                    slide = FIRST_TUTORIAL_SLIDE;
                }
                loadSlide();
            } else {
                //next command
                slide++;
                if(slide >= slides.length) {
                    slide--;
                }
                loadSlide();
            }
        }
    } else if(trimTmpCmd == ":show") {
        //alert('showing examples....');
        slide = 1;
        loadSlide();
    }
    if(debug) {
        alert("data to be sent: " + sessCmds);
    }
    frames['scratch'].document.getElementById("cmd").value=sessCmds;
    frames['scratch'].document.terminal.submit(); 
    cmd = "";
}

//$.keydown
function onKeyDown(event) {
    if(event.ctrlKey || event.altKey || event.shiftKey) {
        //ignore ctrl and alt modifiers
        return true;
    }

    var keyCode = event.keyCode;
    
    focusOnCmd("#status");
    
    if(keyCode == 13) {
        //enter
        handleEnter();
        return false;
        
    } else if(keyCode == 8) {
        //backspace
        if(curpos > 0) {
            curpos -=1
            var newCmd = "";
            for(var i = 0; i < cmd.length; i++) {
                if(i != curpos) {
                    newCmd += cmd.charAt(i);
                }
            }
            cmd = newCmd;
            showCmd();
            
            debugKeys();
        }
        return false;
        
    } else if(keyCode == 38) {
        //up
        hist_next();
        return false;
        
    } else if(keyCode == 40) {
        //down
        hist_prev();
        return false;
        
    } else if(keyCode == 37) {
        //left
        if(curpos > 0) {
            curpos--;
            moveCursor();
        }
        return false;
        
    } else if(keyCode == 39) {
        //right
        if(curpos < cmd.length) {
            curpos++;
            moveCursor();
        }
        return false;
    } else if(keyCode == 36) {
        //home
        curpos = 0;
        moveCursor();
        return false;
    
    } else if(keyCode == 35) {
        //end
        curpos = cmd.length;
        moveCursor();
        return false;
  
    } else if(keyCode == 46) {
        //del
        if(curpos >= 0){
            var newCmd = "";
            for(var i = 0; i < cmd.length; i++) {
                if(i != curpos) {
                    newCmd += cmd.charAt(i);
                }
            }
            cmd = newCmd;
            showCmd();
            moveCursor();
            
            debugKeys();
        }
        return false;
    }
    return true;
}

//Convert a string character to its ascii integer value
//return the ascii integer if string 'ch' is in the range 32-126
//otherwise, will return a zero
function toAscii(ch)  {
    var loAZ = "abcdefghijklmnopqrstuvwxyz";
    var symbols = " !\"#$%&'()*+'-./0123456789:;<=>?@" + 
        loAZ.toUpperCase() + "[\\]^_`" + loAZ + "{|}~";
    var loc = symbols.indexOf(ch);
    var ascii = (loc > -1) ? (32 + loc) : 0;
    return ascii;  
}

//$.keypress
function onKeyPress(event) {

    if(event.ctrlKey || event.altKey) {
        //ignore ctrl and alt modifiers
        return;
    }

    var keyCode = event.keyCode;
    
    focusOnCmd("#status");
    
    
    if($.browser.msie || $.browser.opera || $.browser.safari) {
        var key = String.fromCharCode(keyCode);
        if(key >= ' ') {
            if(($.browser.opera && (keyCode < 35 || keyCode > 40)) || $.browser.msie || $.browser.safari) {
                //insert key at curpos
                cmd = insert(cmd,key,curpos);
                showCmd();
                curpos++;
                
                debugKeys(key);                
            }
        }
        return false;
    } else if($.browser.mozilla && keyCode == 0) {
        var key = String.fromCharCode(event.charCode ? event.charCode : event.keyCode);
        //insert key at curpos
        if(key >= ' ') {
            cmd = insert(cmd,key,curpos);
            showCmd();
            curpos++;
            
            debugKeys(key);
        }
        
        return false;
    }
    return true;
}

//debug commands...
function debugKeys(key) {
    if(debug) {
        var cmdBytes = "";
        for(var i = 0; i < cmd.length; i++) {
            var asciiCode = toAscii(cmd.charAt(i));
            cmdBytes += asciiCode + ",";
        }
        $("#status").text(((key) ? ("key = '" + key) : "") + 
        "', cmd = '" + cmd + 
        "', cmd.length=" + cmd.length +
        ", ascii=(" + cmdBytes + ")");
    }
}

//called by textarea: TODO should be removed when textarea 
//is replaced by async $.ajax 
function getreply () {
    scratchpad=frames['scratch'].document;
    var reply=scratchpad.getElementById("cmd").value;
    histentry=histlist.length;
    sessionid=scratchpad.terminal.sessionid.value;
    document.terminal.cmd.value=reply;

    //$("#toolbar").slideDown(2000);
    
    if(scratchpad.terminal.prompt) {
        //safely assign prompt...
        var val = scratchpad.terminal.prompt.value;
        if(val && val.length == 'pugs> '.length) {
            prompt = val;
        }
    }
    
    //escape html from whitespace and html entities
    //and then split lines
    var escapedReply = reply.replace(/&/g,'&amp;')
        .replace(/ /g,'&nbsp;')
        .replace(/</g,'&lt;')
        .replace(/>/g,'&gt;')
        .replace(/"/g,'&quot;');
	cmds = escapedReply.split(/\r\n|\n|\r/g);
        
    if(debug) {
        alert("reply (unescaped): " + reply);
        //alert("reply (escaped): " + escapedReply);
    }
    
    updateConsole();
    cmd = "";
    curpos=0;
    showCmd();
}

//next in history (triggered by DOWN)
function hist_next () {
    if (histentry>=1) {
        histentry--;
        cmd=histlist[histentry];
        curpos=cmd.length;
        showCmd();
        moveCursor();
    }
    return false;
}

//previous in history (triggered by UP)
function hist_prev () {
    if (histentry<histlist.length-1) {
        histentry++;
        cmd=histlist[histentry];
        curpos=cmd.length;
        showCmd();
        moveCursor();
    }
}

//triggered by user selecting release/development version
function set_version () {
    var reldev=document.terminal.reldev[0].checked;
    if (reldev==true) {
        frames['scratch'].document.terminal.reldev[0].checked=true;
        frames['scratch'].document.terminal.reldev[1].checked=false;
    } else {
        frames['scratch'].document.terminal.reldev[0].checked=false;
        frames['scratch'].document.terminal.reldev[1].checked=true;
    }
    frames['scratch'].document.terminal.submit();

    $("#rel").blur();
    $("#dev").blur();
    focusOnCmd("#tt");
}

