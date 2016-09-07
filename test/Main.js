exports.getData = function () {
  return ENV.generateData().toArray();
};

exports.getTimeout = function () {
  return ENV.timeout
};

exports.pingRenderRate = function () {
  Monitoring.renderRate.ping();
};

exports.unsafeRefEq = function (a, b) {
  return a === b;
}
