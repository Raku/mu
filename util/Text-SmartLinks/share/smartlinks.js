// EVENT HANDLING

// http://therealcrisp.xs4all.nl/upload/addEvent_dean.html
// written by Dean Edwards, 2005
// with input from Tino Zijdel - crisp@xs4all.nl
// http://dean.edwards.name/weblog/2005/10/add-event/
// modified by Aankhen

var addEvent;

if (window.addEventListener) {
    addEvent = function (element, type, handler) { element.addEventListener(type, handler, (typeof arguments[3] != 'undefined') ? arguments[3] : false); };
} else {
  addEvent = function (element, type, handler) {
    // assign each event handler a unique ID
    if (!handler.$$guid) handler.$$guid = addEvent.guid++;
    // create a hash table of event types for the element
    if (!element.events) element.events = {};
    // create a hash table of event handlers for each element/event pair
    var handlers = element.events[type];
    if (!handlers) {
      handlers = element.events[type] = {};
      // store the existing event handler (if there is one)
      if (element['on' + type]) {
        handlers[0] = element['on' + type];
      }
      // assign a global event handler to do all the work
      element['on' + type] = handleEvent;
    }

    // store the event handler in the hash table
    handlers[handler.$$guid] = handler;
  }
}

// a counter used to create unique IDs
addEvent.guid = 1;

function removeEvent(element, type, handler) {
  if (element.removeEventListener)
    element.removeEventListener(type, handler, false);
  // delete the event handler from the hash table
  else if (element.events && element.events[type] && handler.$$guid)
    delete element.events[type][handler.$$guid];
}

function handleEvent(event) {
  // grab the event object (IE uses a global event object)
  event = event || fixEvent(window.event);

  var returnValue = true;

  // get a reference to the hash table of event handlers
  var handlers = this.events[event.type];

  // execute each event handler
  for (var i in handlers) {
    // don't copy object properties
    if (!Object.prototype[i]) {
      this.$$handler = handlers[i];
      if (this.$$handler(event) === false) {
        returnValue = false;
        // in accordance with DOM2-Events, all remaining event handlers on the object will be triggered, hence the absence of a `break`
      }
    }
  }

  // clean up
  if (this.$$handler) this.$$handler = null;

  return returnValue;
}

function fixEvent(event) {
  // add W3C standard event methods
  event.preventDefault = fixEvent.preventDefault;
  event.stopPropagation = fixEvent.stopPropagation;
  return event;
}

fixEvent.preventDefault = function() {
  this.returnValue = false;
};

fixEvent.stopPropagation = function() {
  this.cancelBubble = true;
};


// VISIBILITY TOGGLE

function toggle_snippet (e) {
  var matches = this.id.match(/smartlink_toggle(\d+)/);
  var num = matches[1];

  var id = 'smartlink_' + num;
  var div = document.getElementById(id);
  div.style.display = (div.style.display == 'none') ? '' : 'none';

  var text = this.firstChild;
  text.nodeValue = text.nodeValue.replace(/^- (Show|Hide)/, function (full, p1) { return "- " + ((p1 == 'Show') ? 'Hide' : 'Show') }); // this may be unnecessarily complicated, or it may not.  you get to decide. :-)

  e.stopPropagation();
  e.preventDefault();

  return false;
}

function toggle_hilite(id,url) {
    var el = document.getElementById(id);
    if(el) {
        if(el.style.display == "none") {
            el.src = url;
            el.style.display = "block";
        } else {
            el.style.display = "none";
        }
    }
    return false;
}


// LINK GENERATION
// this would be simpler if we used a library like YUI to simplify retrieval and creation of elements, but oh well

function collectionToArray(col) {
  a = new Array();
  for (i = 0; i < col.length; i++)
    a[a.length] = col[i];
  return a;
}

addEvent(window, 'load', function () {
  var divs = collectionToArray(document.getElementsByTagName('div'));

  for (var i = 0, j = divs.length; i < j; i++) {
    var curr = divs[i];
    if (curr.id && curr.id.match(/smartlink_(\d+)/)) {
      var num = RegExp.$1;

      var toBeRemoved = [ "smartlink_skip_", "smartlink_skipto_" ]; // let it be reusable since this list could conceivably grow :-)
      for (var k = 0, l = toBeRemoved.length; k < l; k++) {
        var id = toBeRemoved[k] + num;
        var elm = document.getElementById(id);
        elm.parentNode.removeChild(elm);
      }

      var p = curr.previousSibling;

      while (p.nodeType != 1) { p = p.previousSibling; } // ignore any whitespace-only nodes

      var text = p.firstChild;
      text.nodeValue = text.nodeValue.replace(/^From/, '- Show');

      var link = document.createElement('a');

      var child;
      while (child = p.firstChild) {
        link.appendChild(child);
      }

      var end = link.lastChild;
      if ((end.nodeType == 3) && (end.nodeValue.search(/:$/) > -1)) {
        end.nodeValue = end.nodeValue.replace(/:$/, ' -');
      }

      link.href = '#';
      link.id = 'smartlink_toggle' + num;
      addEvent(link, 'click', toggle_snippet);

      p.appendChild(link);
      curr.parentNode.insertBefore(p, curr);
      curr.style.display = 'none';
    }

    // explicitly jump to the page anchor (if any) since the code above messes it up
    if (location.hash && location.hash.match(/#.+/)) location.hash = RegExp.lastMatch;
  }
});
