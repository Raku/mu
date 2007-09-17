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

    //start loading pugs session after page has loaded...
    $("#hidden_iframe").append(
        'im there!<iframe src="/perl/runpugs.pl" id="scratch" name="scratch" ' +
        'style="visibility:hidden" width="700px" height="1px" ' +
        'onLoad="getreply()"></iframe>');
});

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
        var sessCmds=document.terminal.cmd.value + cmd;
        var tmpCmds=sessCmds.split(prompt);
        var tmpCmd=tmpCmds[tmpCmds.length-1];
        if($.trim(tmpCmd) != "") {
            histlist.push(tmpCmd);
        }
       	frames['scratch'].document.getElementById("cmd").value=sessCmds;
        frames['scratch'].document.terminal.submit(); 
        cmd = "";
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
        }
        return false;
    }
    return true;
}

//$.keypress
function onKeyPress(event) {

    if(event.ctrlKey || event.altKey) {
        //ignore ctrl and alt modifiers
        return;
    }

    focusOnCmd("#status");

    var keyCode = event.keyCode;
    if($.browser.msie || $.browser.opera) {
        var key = String.fromCharCode(keyCode);
        if(key >= ' ') {
            if(($.browser.opera && (keyCode < 35 || keyCode > 40)) || $.browser.msie) {
                //insert key at curpos
                cmd = insert(cmd,key,curpos);
                showCmd();
                curpos++;
            }
        }
        return false;
    } else if($.browser.mozilla && keyCode == 0) {
        var key = String.fromCharCode(event.charCode ? event.charCode : event.keyCode);
        //insert key at curpos
        cmd = insert(cmd,key,curpos);
        showCmd();
        curpos++;
        return false;
    }
    return true;
}

//called by textarea: TODO should be removed when textarea 
//is replaced by async $.ajax 
function getreply () {
    scratchpad=frames['scratch'].document;
    var reply=scratchpad.getElementById("cmd").value;
    histentry=histlist.length;
    sessionid=scratchpad.terminal.sessionid.value;
    document.terminal.cmd.value=reply;
    
    if(scratchpad.terminal.prompt) {
        //safely assign prompt...
        var val = scratchpad.terminal.prompt.value;
        if(val && val.length == 'pugs> '.length) {
            prompt = val;
        }
    }
    
    //escape html from whitespace and html entities
    //and then split lines
	cmds = 
        reply.replace(/&/g,'&amp;')
        .replace(/ /g,'&nbsp;')
        .replace(/</g,'&lt;')
        .replace(/>/g,'&gt;')
        .replace(/"/g,'&quot;')
        .split(/\r\n|\n|\r/g);
    updateConsole();
    cmd = "";
    curpos=0;
    //moveCursor();
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

    focusOnCmd("#tt");
}

