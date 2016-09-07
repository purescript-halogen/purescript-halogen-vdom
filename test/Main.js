exports.getData = function () {
  return ENV.generateData().toArray();
};

exports.getTimeout = function () {
  return ENV.timeout
};

exports.pingRenderRate = function () {
  Monitoring.renderRate.ping();
};

exports.forInE = function (o, f) {
  return function () {
    var ks = Object.keys(o);
    for (var i = 0; i < ks.length; i++) {
      var k = ks[i];
      f(k, o[k])();
    }
  }
}

exports.buildStrMap = function (fk, fv, as) {
  var o = {};
  for (var i = 0; i < as.length; i++) {
    var a = as[i];
    o[fk(a)] = fv(a);
  }
  return o;
}

exports.diffKeysE = function (f1, f2, f3, o1, o2) {
  return function () {
    var o3 = {};
    var ks1 = Object.keys(o1);
    var ks2 = Object.keys(o2);
    for (var i = 0; i < ks1.length; i++) {
      var k = ks1[i];
      if (o2.hasOwnProperty(k)) {
        f1(k, o1[k], o2[k])();
        o3[k] = true;
      } else {
        f2(k, o1[k])();
      }
    }
    for (var i = 0; i < ks2.length; i++) {
      var k = ks2[i];
      if (o3.hasOwnProperty(k)) {
        continue;
      }
      f3(k, o2[k])();
    }
  }
};

exports.unsafeRefEq = function (a, b) {
  return a === b;
}
