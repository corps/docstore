"use strict";

exports.undefined = undefined;

exports.runImpl = function (cmd) {
  return function (args) {
    return function (opts) {
      return function (onError, onSuccess) {
        var capturedOutput = "";
        var dead = false;
        var quiet = opts.quiet;
        var capture = opts.capture;

        var spawnOpts = {
          env: opts.env,
          uid: opts.uid,
          gid: opts.gid,
          detached: true,
        };

        if (!quiet) {
          console.error("> ", cmd, "--", args);
        }

        var proc = require('child_process')
          .spawn("bash", ["-o", "pipefail", "-c", cmd, "--"].concat(args), spawnOpts);

        // Could also occur when sending a message or killing went bad.
        proc.on('error', function (err) {
          if (dead) return;
          onError(err);
        });

        if (!quiet || capture) {
          proc.stdout.on('data', function (data) {
            if (capture) capturedOutput += data.toString();
            // if (!quiet) process.stderr.write(data);
          });

          proc.stderr.on('data', function (data) {
            if (!quiet) process.stderr.write(data);
          });
        }

        proc.on('exit', function (code) {
          dead = true;
          onSuccess({ code: code, captured: capturedOutput });
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


