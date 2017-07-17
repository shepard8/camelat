// Camelat - Parsing LaTeX-like text in OCaml
// Copyright (C) year  name of author
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

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

