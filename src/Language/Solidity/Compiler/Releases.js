"use strict";

import http from "http";
import https from "https";
import MemoryStream from "memorystream";

export const _getURL = function(url) {
  const httpImpl = url.startsWith("https:") ? https : http;
  return function(onError, onSuccess) {
    var cancel = httpImpl.get(url, function (res) {
      var error;
      if (res.statusCode != 200) {
        error = new Error("Request failed to " +  url + " status code " + res.statusCode);
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