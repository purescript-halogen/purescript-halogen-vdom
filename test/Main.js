export function getData() {
  return ENV.generateData().toArray();
}

export function getTimeout() {
  return ENV.timeout;
}

export function pingRenderRate() {
  Monitoring.renderRate.ping();
}

export function setTimeout(ms) {
  return function (fn) {
    return function () {
      return setTimeout(fn, ms);
    };
  };
}

export function requestAnimationFrame(f) {
  return function () {
    window.requestAnimationFrame(function () {
      f();
    });
  };
}
