var histlist=new Array();
var histentry=0;
var reply="";
var sessionid=0;
var reldev=0;

var curpos=0;
var cmd = "";
var prompt = "pugs> ";
var cmds = new Array();
var theme = "wb_theme";

$(window).unload(function() {
	//send :q to pugs on onload
	$.ajax({
		url: "/perl/runpugs.pl?" + "sessionid=" + sessionid + 
        "&reldev=1&ia=1&cmd=%3Aq",
		async: true
	});
});

function showCursor() {
    var cursorEl = "#d" + (cmds.length - 1);
    $(cursorEl).toggleClass('cursorOff');
    $(cursorEl).toggleClass('cursorOn');
    setTimeout('showCursor()',1000);
}

function updateConsole() {
    $("#tt").empty();
    $.each(cmds,function(i,n) {
        var l = (n == "") ? "&nbsp;" : n;
        $("#tt").append(
        "<tr><td><pre id='c" + i + "' class='"+ theme +"'>" + 
        l + "</pre><span id='d" + i + 
        "' class='cursorOff'>&nbsp;</span></td></tr>"
        );
    });
    var lastId = (cmds.length - 1);
    var cmdEl = "#c" + lastId;
    var cursorEl = "#d" + lastId;
    
    var scrollHeight = $("#termwin")[0].scrollHeight;
    $("#termwin").animate({ scrollTop: scrollHeight }, "fast")
}

$(document).ready( function() {
	//document is ready now...
    $("#tt").empty();
    $("#tt").append("<tr><td><pre id='c0' class='wb_theme'>" +
    "Please wait while Pugs starts up...</pre>"+
    "<span class='cursor'>&nbsp;</span></td></tr>");
    
    $("#theme").change(function() {
        $("pre").toggleClass(theme);
        theme = $("#theme").val();
        $("pre").toggleClass(theme);    
    });
    $("#logo").slideDown(2000);
    
    showCursor();
});





$(document).keydown(function(event) {
    var keyCode = event.keyCode;
    var cmdEl = "#c" + (cmds.length - 1);

    var scrollHeight = $("#termwin")[0].scrollHeight;
    $("#termwin").animate({ scrollTop: scrollHeight }, "fast");

    if(keyCode == 13) {
        //enter
        var sessCmds=document.terminal.cmd.value + "\n" + prompt + cmd;
        var tmpCmds=sessCmds.split('pugs> ');
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
            cmd = cmd.substring(0,cmd.length-1);
            $(cmdEl).text(prompt + cmd);
        }
        return false;
    } else if(keyCode == 38) {
        //up
        hist_next();
        
        return false;
    } else if(keyCode == 40) {
        //down
        hist_prev();
        $(cmdEl).text(prompt + cmd);
        return false;
    } /*else if(keyCode == 37) {
        //left
        $("#status").text("left not implemented");
        return false;
    } else if(keyCode == 39) {
        //right
        $("#status").text("right not implemented");
        return false;
    } else if(keyCode == 36) {
        //home
        //curpos = 0;
        $("#status").text("home not implemented");
        return false;
    
    } else if(keyCode == 35) {
        //end
        $("#status").text("end not implemented");
        return false;
  
    } */
      
      return true;
});
$(document).keypress(function(event) {

    var scrollHeight = $("#termwin")[0].scrollHeight;
    $("#termwin").animate({ scrollTop: scrollHeight }, "fast");

    var keyCode = event.keyCode;
    var cmdEl = "#c" + (cmds.length - 1);
    if($.browser.msie || $.browser.opera) {
        var key = String.fromCharCode(keyCode);
        if(key >= ' ') {
            if(($.browser.opera && (keyCode < 35 || keyCode > 40)) || $.browser.msie) {
                cmd += key;   
                $(cmdEl).text(prompt + cmd);
                curpos++;
            }
        }
        return false;
    }else if($.browser.mozilla && keyCode == 0) {
        var key = String.fromCharCode(event.charCode ? event.charCode : event.keyCode);
        cmd += key;   
        $(cmdEl).text(prompt + cmd);
        curpos++;
        return false;
    }
    return true;
});


function getreply () {
    scratchpad=frames['scratch'].document;
    reply=scratchpad.getElementById("cmd").value;
    histentry=histlist.length;
    sessionid=scratchpad.terminal.sessionid.value;
    document.terminal.cmd.value=reply;

	cmds = reply.replace(/ /g,'&nbsp;').split(/\r\n|\n|\r/g);
    updateConsole();
}

function hist_next () {
    if (histentry>=1) {
        histentry--;
        cmd=histlist[histentry];
        curpos=cmd.length;
        var cmdEl = "#c" + (cmds.length - 1);
        $(cmdEl).text(prompt + cmd);
    }
    return false;
}

function hist_prev () {
    if (histentry<histlist.length-1) {
        histentry++;
        cmd=histlist[histentry];
        curpos=cmd.length;
        var cmdEl = "#c" + (cmds.length - 1);
        $(cmdEl).text(prompt + cmd);
    }
}

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
}

