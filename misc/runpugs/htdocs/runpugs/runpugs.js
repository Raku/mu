var nchars=0;
var histlist;
var histentry=0;
var reply="";

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

function catch_events(myfield,e) {
    var keycode;
    if (e) {
        keycode = e.which;
        if(keycode) {
        return process_events(keycode);
        } else {
          if (window.event) {     
            keycode = window.event.keyCode;
            if(keycode) {
                return process_events(keycode);
            } else {
                return true;
            }
        }
    }
}
}


function process_events (keycode) {
//    document.terminal.mon.value=keycode;
    if (keycode == 13) {
        var cmd=document.terminal.cmd.value;
        //cmd.replace(/^.*pugs\>\ /, "");
        //cmds=cmd.split('pugs> ');
        //cmd=cmds[cmds.length-1];
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
    //getElementById("scratch")
    scratchpad=frames['scratch'].document;//.contentDocument;
    reply=scratchpad.getElementById("cmd").value;
    histlist=scratchpad.terminal.history.options;
    histentry=histlist.length;
    //histlist.reverse;
    document.terminal.cmd.value=reply;
    document.terminal.cmd.focus() 
    document.terminal.cmd.scrollTop =document.terminal.cmd.scrollHeight;
    nchars=document.terminal.cmd.value.length;
}

function hist_next () {
    if (histentry>1) {
        histentry-=1;
        document.terminal.cmd.value=reply+histlist[histentry].value;
        document.terminal.cmd.scrollTop =document.terminal.cmd.scrollHeight; 
    }
    return false;
}

function hist_prev () {
    if (histentry<histlist.length-1) {
        histentry+=1;
        document.terminal.cmd.value=reply+histlist[histentry].value;
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

/*
<textarea id="cmd" name="cmd" rows="20" cols="80" wrap="virtual" onkeydown="return catch_events(this,event)" >
Please wait while Pugs starts up...
</textarea>
*/
