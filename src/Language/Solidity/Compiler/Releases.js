"use strict";

// TODO: replace with purescript-affjax

const http = require('http');
const https = require('https');
const MemoryStream = require('memorystream'); // TODO: should be in deps?

exports._getURL = function(url) {
  const httpImpl = url.startsWith("https:") ? https : http;
  return function(onError, onSuccess) {
    var cancel = httpImpl.get(url, function (res) {
      var error;
      if (res.statusCode != 200) {
        error = new Error("Request failed, status code " + statusCode);
      }

      if (error) {
        res.resume();
        onError(error);
      };
      var mem = new MemoryStream(null, { readable: false });

      res.pipe(mem);
      res.on('end', function () { onSuccess(mem.toString()) });
    }).on('error', function (err) { onError(err) });

    return function(cancelError, onCancelerError, onCancelerSuccess) {
      cancel();
      onCancelerSuccess();
    }
  }
}
