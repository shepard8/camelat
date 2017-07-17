// Preconditions: 
// - {text} starts with before;
// - {text} ends with after.
//
// Removes the {before} and {after} values around {text} in {e.value};
// sets the {e} selection around {text}.
function remove(e, before, after, textBefore, text, textAfter) {
  text = text.substring(before.length, text.length - after.length);
  e.value = textBefore + text + textAfter;
  
  e.selectionStart = textBefore.length;
  e.selectionEnd = e.selectionStart + text.length;
}

// Adds {before} and {after} around {text} in {e.value};
// sets the {e} selection around {before + text + after}.
function insert(e, before, after, textBefore, text, textAfter) {
  text = before + text + after;
  e.value = textBefore + text + textAfter;
  
  e.selectionStart = textBefore.length;
  e.selectionEnd = e.selectionStart + text.length;
}

function switchgen(id, before, after, handletabs) {
  var e = document.getElementById(id);
  
  var selStart = e.selectionStart;
  var selEnd = e.selectionEnd;
  
  var textBefore = e.value.substring(0, selStart);
  var text = e.value.substring(selStart, selEnd);
  var textAfter = e.value.substring(selEnd);

  if (text.startsWith(before) && text.endsWith(after)) {
    if (handletabs) {
      text = text.split("\n" + handletabs).join("\n");
    }

    remove(e, before, after, textBefore, text, textAfter);
  }
  else {
    if (handletabs) {
      text = handletabs + text.split("\n").join("\n" + handletabs);
    }

    insert(e, before, after, textBefore, text, textAfter);
  }
}

function surround(id, before, after) {
  var e = document.getElementById(id);

  var selStart = e.selectionStart;
  var selEnd = e.selectionEnd;

  var textBefore = e.value.substring(0, selStart);
  var text = e.value.substring(selStart, selEnd);
  var textAfter = e.value.substring(selEnd);

  insert(e, before, after, textBefore, text, textAfter);
}

function switchcmd(id, before, after) {
  switchgen(id, before, after, false);
}

function switchenv(id, before, after) {
  switchgen(id, before, after, "\t");
}

