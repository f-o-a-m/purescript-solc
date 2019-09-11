"use strict";

const solc = require('solc');

function stringify(input) {
  if (typeof input !== 'string') {
    return JSON.stringify(input);
  }
  return input;
}

function objectify(input) {
  if (typeof input === 'object') {
    return input;
  }
  return JSON.parse(input);
}

exports.defaultCompiler = solc;

exports.version = function(solc) {
  return solc.version;
}

exports.callbackSuccess = function (contents) {
  return { "contents": contents }
};

exports.callbackFailure = function (error) {
  return { "error": error }
};

exports._loadRemoteVersion = function(version) {
  return function (onError, onSuccess) {
    var cancel = solc.loadRemoteVersion(version, function(err, solcSnapshot) {
      if (err) {
        onError(err);
      } else {
        onSuccess(solcSnapshot);
      }
    });
    return function(cancelError, onCancelerError, onCancelerSuccess) {
      cancel();
      onCancelerSuccess();
    };
  }
};

exports._compile = function (solc, input, readCallback) {
  return function() {
    return objectify(solc.compile(stringify(input), function(requestedFile) {
      return readCallback(requestedFile)();
    }));
  }
};
