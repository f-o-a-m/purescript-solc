"use strict";

var solc = require('solc');

exports.callbackSuccess = function (contents) {
  return { "contents": contents }
};

exports.callbackFailure = function (error) {
  return { "error": error }
};

exports._compile = function (input, readCallback) {
  return function() {
    return solc.compile(input, function(requestedFile) {
      return readCallback(requestedFile)();
    });
  }
};