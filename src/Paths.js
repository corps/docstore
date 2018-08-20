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
