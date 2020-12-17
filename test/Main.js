exports.getData = function () {
  return ENV.generateData().toArray();
};

exports.getTimeout = function () {
  return ENV.timeout;
};

exports.pingRenderRate = function () {
  Monitoring.renderRate.ping();
};

exports.setTimeout = function (ms) {
  return function (fn) {
    return function () {
      return setTimeout(fn, ms);
    };
  };
};

exports.requestAnimationFrame = function (f) {
  return function () {
    window.requestAnimationFrame(function () {
      f();
    });
  };
};
