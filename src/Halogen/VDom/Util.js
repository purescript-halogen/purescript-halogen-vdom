"use strict";

// removeEventListener
// removeAttribute
// removeProperty
// pokeMutMap
// addEventListener
// setAttribute

exports.cancelBehavior = function (ty) {
    var canceler = window.__CANCELER[ty];
    canceler();
}

exports.unsafeGetAny = function (key, obj) {
  return obj[key];
};

exports.unsafeGetProp = function (key, obj) {
  if (obj.props)
    return obj.props[key];
  else return;
};

exports.unsafeHasAny = function (key, obj) {
  return obj.hasOwnProperty(key);
};

exports.updateProperty = function (key, val, obj) {
  window.updateProperty(obj, {value0: key, value1: val})
};

exports.addProperty = function (key, val, obj) {
  window.addProperty(obj, {value0: key, value1: val})
};

exports.unsafeSetAny = function (key, val, obj) {
    obj[key] = val;
};

exports.unsafeSetProp = function (key, val, obj) {
  obj.props[key] = val;
};

exports.removeProperty = function (key, val, obj) {
  obj.props[key] = val;
  delete obj.props[key];
};

exports.unsafeDeleteAny = function (key, obj) {
  delete obj.props[key];
};

exports.forE = function (a, f) {
  var b = [];
  for (var i = 0; i < a.length; i++) {
    b.push(f(i, a[i]));
  }
  return b;
};

exports.forEachE = function (a, f) {
  for (var i = 0; i < a.length; i++) {
    f(a[i]);
  }
};

exports.forInE = function (o, f) {
  var ks = Object.keys(o);
  for (var i = 0; i < ks.length; i++) {
    var k = ks[i];
    f(k, o[k]);
  }
};

exports.replicateE = function (n, f) {
  for (var i = 0; i < n; i++) {
    f();
  }
};

exports.diffWithIxE = function (a1, a2, f1, f2, f3) {
  var a3 = [];
  var l1 = a1.length;
  var l2 = a2.length;
  var i  = 0;
  while (1) {
    if (i < l1) {
      if (i < l2) {
        a3.push(f1(i, a1[i], a2[i]));
      } else {
        f2(i, a1[i]);
      }
    } else if (i < l2) {
      a3.push(f3(i, a2[i]));
    } else {
      break;
    }
    i++;
  }
  return a3;
};

exports.strMapWithIxE = function (as, fk, f) {
  var o = {};
  for (var i = 0; i < as.length; i++) {
    var a = as[i];
    var k = fk(a);
    o[k] = f(k, i, a);
  }
  return o;
};

exports.diffWithKeyAndIxE = function (o1, as, fk, f1, f2, f3) {
  var o2 = {};
  for (var i = 0; i < as.length; i++) {
    var a = as[i];
    var k = fk(a);
    if (o1.hasOwnProperty(k)) {
      o2[k] = f1(k, i, o1[k], a);
    } else {
      o2[k] = f3(k, i, a);
    }
  }
  for (var k in o1) {
    if (k in o2) {
      continue;
    }
    f2(k, o1[k]);
  }
  return o2;
};

exports.diffPropWithKeyAndIxE = function (o1, as, fk, f1, f2, f3, el) {
  var o2 = {};
  var replace = false;
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
    replace = true;
    f2(k, o1[k])();
  }
  if (replace)
    window.replaceView(el);
  return o2;
};

exports.refEq = function (a, b) {
  return a === b;
};

exports.createTextNode = function (s, doc) {
  return {type: "textView", children: [], props: {text: s}}
};

exports.setTextContent = function (s, n) {
  n.textContent = s;
};

exports.createElement = function (ns, name, doc) {
  return {type: name, children: [], props: {}, __ref: window.createPrestoElement()}
};

exports.insertChildIx = function (type, i, a, b) {
  var n = (b.children[i]) || {__ref: {__id: "-1"}};

  if (!a)
    console.warn("CUSTOM VDOM ERROR !! : ", "Trying to add undefined element to ", b);
  if (a && n.__ref.__id !== a.__ref.__id) {
    if (type == "patch") {
      window.addChild(a, b, i);
    }

    a.parentNode = b;
    b.children.splice(i, 0, a);
  }
};

exports.removeChild = function (a, b) {
  var childIndex = -1;

  if (b && a.parentNode.__ref.__id === b.__ref.__id) {
    for (var i=0; i<b.children.length; i++) {
      if (b.children[i].__ref.__id == a.__ref.__id) {
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

exports.parentNode = function (a) {
  if (a.parentNode.props.__removed) {
    a.props.__removed = true;
    return null;
  } else {
    return a.parentNode;
  }
};

exports.setAttribute = function (ns, attr, val, el) {
  if (ns != null) {
    el.setAttributeNS(ns, attr, val);
  } else {
    el.setAttribute(attr, val);
  }
};

exports.removeAttribute = function (ns, attr, el) {
  if (ns != null) {
    el.removeAttributeNS(ns, attr);
  } else {
    el.removeAttribute(attr);
  }
};

exports.addEventListener = function (pr, ev, listener, el) {
  // el.addEventListener(ev, listener, false);
  el.props[ev] = listener;
  if(pr == "patch") {
    window.replaceView(el);
  }
};

exports.removeEventListener = function (ev, listener, el) {
   // el.removeEventListener(ev, listener, false);
   delete el.props[ev];
};

exports.jsUndefined = void 0;
