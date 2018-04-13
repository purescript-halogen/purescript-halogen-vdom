"use strict";

exports.unsafeGetAny = function (key, obj) {
  return obj[key];
};

exports.unsafeHasAny = function (key, obj) {
  return obj.hasOwnProperty(key);
};

exports.unsafeSetAny = function (key, val, obj) {
  return function () {
    obj[key] = val;
  };
};

exports.unsafeDeleteAny = function (key, obj) {
  return function () {
    delete obj[key];
  };
};

exports.forE = function (a, f) {

  return function () {
    var b = [];
    for (var i = 0; i < a.length; i++) {
      b.push(f(i, a[i])());
    }
    return b;
  };
};

exports.forInE = function (o, f) {
  return function () {
    var ks = Object.keys(o);
    for (var i = 0; i < ks.length; i++) {
      var k = ks[i];
      f(k, o[k])();
    }
  };
};

exports.replicateE = function (n, f) {
  return function () {
    for (var i = 0; i < n; i++) {
      f();
    }
  };
};

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
  };
};

exports.strMapWithIxE = function (as, fk, f) {
  return function () {
    var o = {};
    for (var i = 0; i < as.length; i++) {
      var a = as[i];
      var k = fk(a);
      o[k] = f(k, i, a)();
    }
    return o;
  };
};

exports.diffWithKeyAndIxE = function (o1, as, fk, f1, f2, f3) {
  return function () {
    var o2 = {};
    for (var i = 0; i < as.length; i++) {
      var a = as[i];
      var k = fk(a);
      if (o1.hasOwnProperty(k)) {
        o2[k] = f1(k, i, o1[k], a)();
      } else {
        o2[k] = f3(k, i, a)();
      }
    }
    for (var k in o1) {
      if (k in o2) {
        continue;
      }
      f2(k, o1[k])();
    }
    return o2;
  };
};

exports.refEq = function (a, b) {
  return a === b;
};

exports.createTextNode = function (s, doc) {
  return function () {
    return {type: "textView", children: [], props: {text: s}}
  };
};

exports.setTextContent = function (s, n) {
  return function () {
    n.textContent = s;
  };
};

exports.createElement = function (ns, name, doc) {
  return function () {
    return {type: name, children: [], props: {}, __ref: window.createPrestoElement()}
  };
};

exports.insertChildIx = function (type, i, a, b) {
  return function () {
    var n = (b.children[i]) || {props: {id: "-1"}};

    if (n.props.id !== a.props.id) {
      if (type == "patch") {
        window.addChild(a, b, i);
      }

      a.parentNode = b;
      b.children.splice(i, 0, a);
    }
  };
};

exports.removeChild = function (a, b) {
  return function () {
    var childIndex = -1;

    if (b && a.parentNode.props.id === b.props.id) {
      for (var i=0; i<b.children.length; i++) {
        if (b.children[i].props.id == a.props.id) {
          childIndex = i;
        }
      }
    }

    if (childIndex > -1) {
      window.removeChild(a, b, childIndex);
      a.props.__removed = true;
      b.children.splice(childIndex, 1);
    }
  };
};

exports.unsafeParent = function (a) {
  if (a.parentNode.props.__removed) {
    a.props.__removed = true;
    return null;
  } else {
    return a.parentNode;
  }
};

exports.setAttribute = function (ns, attr, val, el) {
  return function () {
    if (ns != null) {
      el.setAttributeNS(ns, attr, val);
    } else {
      el.setAttribute(attr, val);
    }
  };
};

exports.removeAttribute = function (ns, attr, el) {
  return function () {
    if (ns != null) {
      el.removeAttributeNS(ns, attr);
    } else {
      el.removeAttribute(attr);
    }
  };
};

exports.addEventListener = function (ev, listener, el) {
  return function () {
    el.addEventListener(ev, listener, false);
  };
};

exports.removeEventListener = function (ev, listener, el) {
  return function () {
     el.removeEventListener(ev, listener, false);
  };
};

exports.jsUndefined = void 0;
