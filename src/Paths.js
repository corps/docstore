exports.nodePath = function () {
  return require('path');
}

exports.parsePath = function(pathLib) {
  return function (path) {
    return pathLib.parse(path);
  }
}

exports.formatPath = function(pathLib) {
  return function (pathFormat) {
    pathFormat = Object.assign({}, pathFormat, { base: undefined });
    return pathLib.format(pathFormat);
  }
}

exports.segmentImpl = function(path) {
  if (path[path.length - 1] == "/") return exports.segmentImpl(path.slice(0, path.length - 1));
  return path.split("/");
}
