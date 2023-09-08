"use strict";

import solcMod from "solc";

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

export const defaultCompiler = solcMod;

export const version = function(solc) {
  return solc.version();
}

// Because PureScript 0.15+ converted modules to use ESM, we can't just
// require() in a CJS module -- even import()ing from a data URI will make
// node's loader assume we're trying to load an ES module. This is unfortunate
// since solc is shipped as an ES. To get around this, we create a loader hook
// that forces Node to treat certain URLs as CommonJS.
//
// This unfortunately requires Node v20.6+
//
// Because we are a PureScript package and can't assume anything about where any relative
// "pure" JS files will be, but we do know our own module's URL, we keep this function here
// and in _useCompiler, we register this file as a node loader.
const __DATA_JS_BASE64 = "data:text/javascript;base64,";
const __SOLC_CJS_MARKER = "/solc_cjs";
export function load(spec, context, next) {
  if (typeof spec === 'string' && spec.startsWith(__DATA_JS_BASE64) && spec.endsWith(__SOLC_CJS_MARKER)) {
    const cleanedSpec = spec.substring(__DATA_JS_BASE64.length, spec.length - __SOLC_CJS_MARKER.length);
    return {
      format: 'commonjs',
      shortCircuit: true,
      source: atob(cleanedSpec),
    };
  } else {
    return next(spec, context);
  }
}

export const _useCompiler = function(source) {
  return function (onError, onSuccess) {
    const mkMod = async () => {
      try {
        const url = __DATA_JS_BASE64 + btoa(source) + __SOLC_CJS_MARKER;
        const NodeModule = await import("node:module");
        NodeModule.register(import.meta.url);
        const mod = await import(url);
        return mod.default;
      } catch(e) {
        console.error(e);
        throw e;
      }
    };

    const cancel = mkMod().then(m => onSuccess(solcMod.setupMethods(m)), e => {
      onError(e);
    });

    return function(cancelError, onCancelerError, onCancelerSuccess) {
      cancel();
      onCancelerSuccess();
    };
  };
}

export const callbackSuccess = function (contents) {
  return { "contents": contents }
};

export const callbackFailure = function (error) {
  return { "error": error }
};

export const _loadRemoteVersion = function(version) {
  return function (onError, onSuccess) {
    var cancel = solcMod.loadRemoteVersion(version, function(err, solcSnapshot) {
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

export const _compile = function (solc, input, readCallback) {
  return function() {
    // support different versions of solc-js
    // to understand what's going on here, keep this in mind:
    //
    // for the __NPM PACKAGE__ "solc"
    // 0.5.x and up compile() expects stringified standard compiler JSON
    //   --> 0.5.0 - 0.5.2 keep compileStandardWrapper for backwards compatibility.
    // 0.4.11 - 0.4.26 have compileStandardWrapper which behaves like 0.5.x compile()
    // 0.4.10 and below can die in a conflagration
    //   --> compile takes three arguments, (input, optimize, callback)
    //     --> where input may or may not be stringified standard compiler JSON
    //     --> not even sure what happens when you pass false for optimize (does the JSON input override the bool?)
    //   --> so we just don't support it...
    //
    // so... if compileStandardWrapper exists we use that, cause it's the most reliable between 0.4.11 -> 0.5.x of the NPM package
    // otherwise we assume that whatever solc.version() (version of compiler, not npm package!) returns is also the version of the
    // solc npm package we're using (let's be honest, is anyone really gonna be using < 0.5?).
    // Nonetheless, before using compile(), we check that it'll behave like 0.5.x+'s., by making sure that the compiler version
    // is not overlapping with an unsupported solc NPM version.
    //
    // And now it gets worse, they apparently reuploaded older versions of solc-js blobs that take the new callback form,
    // in both compile and compileStandardWrapper at some point....
    const compile = function(i, cb) {
      const isCallbackError = function(e) {
        return e.toString().toLowerCase().includes("invalid callback");
      };
      if (solc.compileStandardWrapper) {
        try {
          return solc.compileStandardWrapper(i, cb);
        } catch(e) {
          if (isCallbackError(e)) {
            return solc.compileStandardWrapper(i, { "import": cb });
          } else {
            throw e;
          }
        }
      } else {
        const fallbackVersion = '<unknown/unsupported>';
        var version = solc.version;
        if (typeof version === 'function') {
          version = solc.version();
        } else if (typeof version !== 'string') {
          version = fallbackVersion;
        }

        // solc-js packages that were released with a version number < 0.4.11 that could've also been a compiler version are
        // --> 0.3.2 -> 0.3.6
        // --> 0.4.1
        // --> 0.4.10
        // and we have to support "-nightly.date+commit.commit" and just "+commit.commit"
        const isFallbackVersion = version === fallbackVersion;
        const isUnsupportedV3_x = version.startsWith("0.3");
        const isUnsupportedV4_x = version.startsWith("0.4.1+") || version.startsWith("0.4.1-") || version.startsWith("0.4.10+") || version.startsWith("0.4.10-");
        if (isFallbackVersion || isUnsupportedV3_x|| isUnsupportedV4_x) {
          throw new Error("Solidity version is " + version + ", which is unsupported by purescript-solc.");
        } else {
          // if we got here we're probably using version 0.5.x or above AND they actually went through on their promise to remove the deprecated
          // compileStandardWrapper (they said they'd get rid of it after 0.5.3, but 0.5.11 (latest as of this writing) still has it).
          // odds are we can just call solc.compile()!

          // unless we're running 0.5.12+, which made the `callback` argument an object. in which case, we need to
          // a. instead of passing cb directly to compile, we have to give an object that looks like `{ import: cb }`
          // b. instead of returning the string output of cb straight to solc, we have to return it an object `{ contents: "str" }`
          // shoot me in the face
          // todo: replace all this string checking with a proper version parser/comparison
          const isVersionV5_12_plus = version.startsWith("0.5.1") && !version.startsWith("0.5.1+") && !version.startsWith("0.5.1-") && !version.startsWith("0.5.10") && !version.startsWith("0.5.11");
          const isNewCallbackFormat = !version.startsWith("0.4") || isVersionV5_12_plus;

          if (isNewCallbackFormat) {
            try {
              return solc.compile(i, { "import": cb });
            } catch (e) {
              if (isCallbackError(e)) {
                return solc.compile(i, cb);
              } else {
                throw e;
              }
            }
          } else {
            try {
              return solc.compile(i, cb);
            } catch (e) {
              if (isCallbackError(e)) {
                return solc.compile(i, { "import": cb });
              } else {
                throw e;
              }
            }
          }
        }
      }
    };
    return objectify(compile(stringify(input), function(requestedFile) {
      return readCallback(requestedFile)();
    }));
  }
};