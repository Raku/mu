var nchars=0;
var histlist;
var histentry=0;
var reply="";
var sessionid=0;
var reldev=1;
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
    sessionid=scratchpad.terminal.sessionid.value;
    //reldev=scratchpad.terminal.reldev.value;
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

/* Courtesy of diakopter */
//WV01012007: But it does not send a ':q' on close. 
//in fact, it looks like the handler is completely ignored.
function HandleOnUnload(evt)
{
	if ( (navigator.userAgent.toUpperCase().indexOf( "SAFARI" ) == -1) &&  // Exclude Safari.
	( (document.all && window.screenLeft >= 10004)   // IE moves the window 10000 pixels to the right while closing it
		|| (!document.all && evt.target==null) ) )  // Firefox sets the target of the passed event to null.
	{ // The window has been closed.  Submit a logout request to the server.
    var expireSessionUrl='/perl/runpugs3.pl?sessionid='+sessionid+'&reldev=1&ia=1&cmd=%3Aq';
		var objXMLCloser = null;
		if ( !document.all && !XMLHttpRequest ) return;
		if ( document.all ) { objXMLCloser = new ActiveXObject( "Microsoft.XMLHTTP" ) }
		else if ( XMLHttpRequest ) { objXMLCloser = new XMLHttpRequest(); }
		if (objXMLCloser) { objXMLCloser.open( 'GET', expireSessionUrl, false ); objXMLCloser.send( null ); }
	}
}

/* usage: 

This will work in any server-language environment that maintains session state on the server with timeouts.  This will help prevent large accumulation of sessions in memory on the servers.

You need to include the javascript file above.

Usage: on the page where you want to trap the window close event, add the following to the body tag as follows: 
<body onunload="HandleOnUnload(event)">

*/
