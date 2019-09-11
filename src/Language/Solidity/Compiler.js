"use strict";

const solc = require('solc');

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

exports._compile = function (input, readCallback) {
  return function() {
    return JSON.parse(solc.compile(JSON.stringify(input), function(requestedFile) {
      return readCallback(requestedFile)();
    }));
  }
};