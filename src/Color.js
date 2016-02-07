// module Color
// jshint node: true

"use strict";

// Parse a string which guaranteed to be of the form `[0-9a-f]{1,2}`.
exports.parseHex = function(str) {
  return parseInt(str, 16);
};

// Convert a number between 0 and 255 to a hex value between 00 and ff.
exports.toHex = function(n) {
  var str = n.toString(16);
  if (str.length == 1) {
    return "0" + str;
  }
  return str;
};
