"use strict";

exports.forE = function (a, f) {
  return function () {
    var b = [];
    for (var i = 0; i < a.length; i++) {
      b.push(f(a[i])());
    }
    return b;
  }
}

exports.diffWithIxE = function (a1, a2, f1, f2, f3) {
  return function() {
    var a3 = [];
    var l1 = a1.length;
    var l2 = a2.length;
    var i  = 0;
    while (1) {
      if (i < l1) {
        if (i < l2) {
          a3.push(f1(i, a1[i], a2[i])());
        } else {
          f2(i, a1[i])();
        }
      } else if (i < l2) {
        a3.push(f3(i, a2[i])());
      } else {
        break;
      }
      i++;
    }
    return a3;
  }
}

exports.eqNode = refEq;

function refEq (a) {
  return function (b) {
    return a === b;
  }
}
