// module Test.Process

exports.exit = function (rv) {
  return function() {
    try { process.exit(rv); } catch (e) {
      try { phantom.exit(rv); } catch (e) {}
    };
  };
};

exports.logRed = function (s) {
  return function() {
    process.stderr.write("\x1b[31;1m" + s + "\x1b[0m\n");
  };
};

exports.logGreen = function (s) {
  return function() {
    process.stderr.write("\x1b[32m" + s + "\x1b[0m\n");
  };
};
