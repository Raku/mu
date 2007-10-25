var gmrDoc, strDoc;
var curStep = 0;
var regexFrom = [];
var regexTo = [];
var curStrPos = 0;
var lastFrom, lastTo;

$(document).ready(function () {
    //alert("hey!");
    //alert(\$("#$id").text());
    //alert(Ops.length);
    setTimeout(function () {
        //alert("hey!");
        gmrDoc = $("#gmr")[0].contentDocument;
        strDoc = $("#str")[0].contentDocument;
        //alert(gmr_doc);
    }, 500);

    $("#reset").click(function () {
        curStep = 0;
        while (regexFrom.length > 0) {
            var from = regexFrom.pop();
            var to = regexTo.pop();
            //alert("From: " + from);
            renderRegex(from, to, setNodeNormal);
        }
        if (lastFrom || lastTo) {
            renderRegex(lastFrom, lastTo, setNodeNormal);
        }
        renderStr(curStrPos, setNodeNormal);
    });
    $("#next").click(function () {
        displayStep();
    });
});

function displayStep () {
    gmrDoc = $("#gmr")[0].contentDocument;
    strDoc = $("#str")[0].contentDocument;
    //alert("Click!");
    //if (curStep == 0 && direction == -1) { alert("Boundary!"); }
    if (regexFrom.length && regexTo.length) {
        //var lastIndex = regexFrom.length - 1;
        //renderRegex(regexFrom[lastIndex], regexTo[lastIndex], setNodeNormal);
        for (var i = 0; i < regexFrom.length; i++) {
            var from = regexFrom[i];
            var to = regexTo[i];
            //alert("From: " + from);
            renderRegex(from, to, setNodeNormal);
        }
        if (lastFrom || lastTo) {
            renderRegex(lastFrom, lastTo, setNodeNormal);
        }
        renderStrFwd(curStrPos, setNodeNormal);
    }
    if (curStrPos) {
        renderStr(curStrPos, setNodeNormal);
    }
    while (true) {
        var op = Ops[curStep++];
        //curStep += direction;
        if (!op) {
            alert("End!");
            curStep = 0;
            return;
        }
        var cmd = op[0];
        $("#label").html("Step " + curStep + ": " + op.join(" "));
        if (cmd == 'begin') {
            var from = op[2];
            var to = op[3];
            var pos = op[4];
            if (from == regexFrom && to == regexTo) {
                alert("Duplicate!");
                continue;
            }
            regexFrom.push(from);
            regexTo.push(to);
            renderRegex(from, to, setNodePending);

            curStrPos = pos;
            renderStr(curStrPos, setNodeSuccess);
            renderStrFwd(curStrPos, setNodePending);
        } else if (cmd == 'end') {
            var result = op[2];
            var pos = op[3];
            curStrPos = pos;
            //alert(result);
            lastFrom = regexFrom.pop();
            lastTo = regexTo.pop();
            if (result == 'success') {
                renderRegex(lastFrom, lastTo, setNodeSuccess);
                renderStr(pos, setNodeSuccess);
            } else if (result == 'fail') {
                renderRegex(lastFrom, lastTo, setNodeFail);
                renderStr(pos, setNodeSuccess);
            }
        }
        break;
    }
    //setSuccess(node);
    //setYello(node);
}

function renderRegex (from, to, setter) {
    var started = false;
    for (var i = 0; i < regexPos.length; i++) {
        if (regexPos[i] == to) {
            break;
        } else if (regexPos[i] == from) {
            started = true;
        }
        if (started) {
            var id = 'R' + regexPos[i] + '-' + regexPos[i+1];
            //alert("Id: " + id);
            var node = $("#" + id, gmrDoc)[0];
            if (!node) {
                alert("Sorry, no node found in renderRegex");
                return;
            }
            setter(node);
        }
    }
}

function renderStr (pos, setter) {
    //alert(strPos.length);
    for (var i = 0; i < strPos.length; i++) {
        if (strPos[i] == pos) {
            break;
        }
        var id = 'R' + strPos[i] + '-' + strPos[i+1];
        //alert("Id: " + id);
        //alert(strDoc);
        var node = $("#" + id, strDoc)[0];
        if (!node) {
            alert("Sorry, no node found in renderStr");
            return;
        }
        setter(node);
    }
}

function renderStrFwd (pos, setter) {
    for (var i = 0; i < strPos.length; i++) {
        if (strPos[i] == pos) {
            var id = 'R' + strPos[i] + '-' + strPos[i+1];
            //alert("Id: " + id);
            //alert(strDoc);
            var node = $("#" + id, strDoc)[0];
            if (!node) {
                //alert("Sorry, no node found in renderStr");
                return;
            }
            setter(node);
            return;
        } else if (strPos[i] > pos) {
            break;
        }
    }
}

function setNodeNormal (node) {
    $(node).css('background-color', 'white');
    $(node).css('color', 'black');
    $(node).css('font-weight', 'normal');
}

function setNodeSuccess (node) {
    $(node).css('background-color', 'green');
    $(node).css('color', 'white');
    $(node).css('font-weight', 'bold');
}

function setNodeFail (node) {
    $(node).css('background-color', 'red');
    $(node).css('color', 'white');
    $(node).css('font-weight', 'bold');
}

function setNodePending (node) {
    $(node).css('background-color', 'yellow');
    $(node).css('color', 'black');
    $(node).css('font-weight', 'bold');
}

