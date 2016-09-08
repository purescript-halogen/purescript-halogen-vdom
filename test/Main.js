exports.getData = function () {
  return ENV.generateData().toArray();
};

exports.getTimeout = function () {
  return ENV.timeout
};

exports.pingRenderRate = function () {
  Monitoring.renderRate.ping();
};

exports.requestAnimationFrame = function (f) {
  return function () {
    window.requestAnimationFrame(function () {
      f();
    });
  }
}
