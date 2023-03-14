export const getData = function () {
  return ENV.generateData().toArray();
};

export const getTimeout = function () {
  return ENV.timeout
};

export const pingRenderRate = function () {
  Monitoring.renderRate.ping();
};

export const requestAnimationFrame = function (f) {
  return function () {
    window.requestAnimationFrame(function () {
      f();
    });
  }
}
