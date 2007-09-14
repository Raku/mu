var nchars=0;
var histlist=new Array();
var histentry=0;
var reply="";
var sessionid=0;
var reldev=1;

$(window).unload(function() {
	//send :q to pugs on onload
	$.ajax({
		url: '/perl/runpugs.pl?sessionid='+sessionid+'&reldev=1&ia=1&cmd=%3Aq',
		async: false
	});                
});
$(document).ready( function() {
	//document is ready now...
        $('#cmd').keydown(function(event) {
                return process_events(event.keyCode);
        });
});




function getnchars() {
    return document.terminal.cmd.value.length
}

function getcursorpos() {
	var obj=document.terminal.cmd;
	if(document.selection) {
    	obj.focus();			
		var rng=document.selection.createRange();			
        rng.moveStart('textedit',-1);
//        rng.moveEnd('character',getnchars());
		return rng.text.length;
	} else if(obj.selectionStart) { // FireFox
    	var start = obj.selectionStart;
    	var end   = obj.selectionEnd;
   		if (start<=end) {
   			return start;
		} else {
   			return end;
           }
	}
}

function setcursorpos() {
	var obj=document.terminal.cmd;
	if(document.selection) {
    	obj.focus();			
		var rng=document.selection.createRange();			
        rng.moveStart('character',0);
        rng.moveEnd('textedit');
//		rng.select();
	} else if(obj.selectionStart>=0) { // FireFox
        	obj.selectionStart=nchars;
    	   obj.selectionEnd=nchars;
	}
}

function process_events (keycode) {
    if (keycode == 13) {
        var cmd=document.terminal.cmd.value;
       //cmd.replace(/^.*pugs\>\ /, "");
        var cmds=cmd.split('pugs> ');
        var lastCmd=cmds[cmds.length-1];
	if($.trim(lastCmd) != "") {
		histlist.push(lastCmd);
	}
       	frames['scratch'].document.getElementById("cmd").value=cmd;
        frames['scratch'].document.terminal.submit(); 
        return false;
    } else if (keycode==38) {
        hist_next();
        setcursorpos();
        return false;
    } else if (keycode==40) {
        hist_prev();
        return false;
    } else if ((keycode==8)||(keycode==37)||(keycode==46)) {
        if ((getnchars()>nchars) && (getcursorpos() > nchars)) {
            return true;
        } else {
        setcursorpos();
            return false;
        }
    } else {
    if(keycode) {
        return true;
        } else { 
        return false;
        }
    }
}

function getreply () {
    scratchpad=frames['scratch'].document;//.contentDocument;
    reply=scratchpad.getElementById("cmd").value;
    histentry=histlist.length;
    sessionid=scratchpad.terminal.sessionid.value;
    document.terminal.cmd.value=reply;
    document.terminal.cmd.focus() 
    document.terminal.cmd.scrollTop =document.terminal.cmd.scrollHeight;
    nchars=document.terminal.cmd.value.length;
}

function hist_next () {
    if (histentry>=1) {
        histentry-=1;
        document.terminal.cmd.value=reply+histlist[histentry];
        document.terminal.cmd.scrollTop =document.terminal.cmd.scrollHeight; 
    }
    return false;
}

function hist_prev () {
    if (histentry<histlist.length-1) {
        histentry+=1;
        document.terminal.cmd.value=reply+histlist[histentry];
        document.terminal.cmd.scrollTop =document.terminal.cmd.scrollHeight; 
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

