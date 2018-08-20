"use strict";

exports.undefined = undefined;

exports.spawnImpl = function (cmd) {
  return function (args) {
    return function (opts) {
      return function (onError, onSuccess) {
        var result = { stdout: "", stderr: "", code: -1 };
        var dead = false;

        opts = Object.assign({ detached: true }, opts);

        var proc = require('child_process')
          .spawn(cmd, args, opts);

        // Could also occur when sending a message or killing went bad.
        proc.on('error', function (err) {
          if (dead) return;
          onError(err);
        });

        proc.stdout.on('data', function (data) {
          result.stdout += data.toString();
        });

        proc.stderr.on('data', function (data) {
          result.stderr += data.toString();
        });

        proc.on('exit', function (code) {
          dead = true;
          result.code = code;
          onSuccess(result);
        });

        return function (cancelError, cancelerError, cancelerSuccess) {
          if (dead) cancelerSuccess();
          proc.kill(-proc.pid);
          cancelerSuccess();
        }
      }
    }
  }
}


