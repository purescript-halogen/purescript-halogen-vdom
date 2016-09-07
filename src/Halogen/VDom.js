"use strict";

exports.forE = function (a, f) {
  return function () {
    var b = [];
    for (var i = 0; i < a.length; i++) {
      b.push(f(i, a[i])());
    }
    return b;
  }
}

exports.foldWithIxE = function (r, a, f) {
  return function () {
    for (var i = 0; i < a.length; i++) {
      r = f(i, r, a[i])();
    }
    return r;
  }
}

exports.replicateE = function (n, f) {
  return function () {
    for (var i = 0; i < n; i++) {
      f();
    }
  }
}

exports.whenE = function (b, e) {
  return function () {
    if (b) {
      e();
    }
  }
}

exports.diffWithIxE = function (a1, a2, f1, f2, f3) {
  return function () {
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

exports.replaceChild = function (a, b, c) {
  return function () {
    c.replaceChild(a, b);
  }
}

exports.removeLastChild = function (a) {
  return function () {
    a.removeChild(a.lastChild);
  }
}

exports.removeFirstChild = function (a) {
  return function () {
    a.removeChild(a.firstChild);
  }
}

exports.appendChild = function (a, b) {
  return function () {
    b.appendChild(a);
  }
}

exports.unsafeInsertChildIx = function (i, a, b) {
  return function () {
    b.insertBefore(a, b.childNodes.item(i));
  }
}

exports.unsafeChildIx = function (i, d) {
  return d.childNodes.item(i);
}

exports.unsafeChildLength = function (d) {
  return d.childNodes.length;
}

exports.eqNode = refEq;

function refEq (a, b) {
  return a === b;
}
