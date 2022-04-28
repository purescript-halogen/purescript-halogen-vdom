"use strict";

export function unsafeGetAny(key, obj) {
  return obj[key];
}

export function unsafeHasAny(key, obj) {
  return obj.hasOwnProperty(key);
}

export function unsafeSetAny(key, val, obj) {
    obj[key] = val;
}

export function unsafeDeleteAny(key, obj) {
  delete obj[key];
}

export function forE(a, f) {
  var b = [];
  for (var i = 0; i < a.length; i++) {
    b.push(f(i, a[i]));
  }
  return b;
}

export function forEachE(a, f) {
  for (var i = 0; i < a.length; i++) {
    f(a[i]);
  }
}

export function forInE(o, f) {
  var ks = Object.keys(o);
  for (var i = 0; i < ks.length; i++) {
    var k = ks[i];
    f(k, o[k]);
  }
}

export function replicateE(n, f) {
  for (var i = 0; i < n; i++) {
    f();
  }
}

export function diffWithIxE(a1, a2, f1, f2, f3) {
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
}

export function strMapWithIxE(as, fk, f) {
  var o = {};
  for (var i = 0; i < as.length; i++) {
    var a = as[i];
    var k = fk(a);
    o[k] = f(k, i, a);
  }
  return o;
}

export function diffWithKeyAndIxE(o1, as, fk, f1, f2, f3) {
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
}

export function refEq(a, b) {
  return a === b;
}

export function createTextNode(s, doc) {
  return doc.createTextNode(s);
}

export function setTextContent(s, n) {
  n.textContent = s;
}

export function createElement(ns, name, doc) {
  if (ns != null) {
    return doc.createElementNS(ns, name);
  } else {
    return doc.createElement(name)
  }
}

export function insertChildIx(i, a, b) {
  var n = b.childNodes.item(i) || null;
  if (n !== a) {
    b.insertBefore(a, n);
  }
}

export function removeChild(a, b) {
  if (b && a.parentNode === b) {
    b.removeChild(a);
  }
}

export function parentNode(a) {
  return a.parentNode;
}

export function setAttribute(ns, attr, val, el) {
  if (ns != null) {
    el.setAttributeNS(ns, attr, val);
  } else {
    el.setAttribute(attr, val);
  }
}

export function removeAttribute(ns, attr, el) {
  if (ns != null) {
    el.removeAttributeNS(ns, attr);
  } else {
    el.removeAttribute(attr);
  }
}

export function hasAttribute(ns, attr, el) {
  if (ns != null) {
    return el.hasAttributeNS(ns, attr);
  } else {
    return el.hasAttribute(attr);
  }
}

export function addEventListener(ev, listener, el) {
  el.addEventListener(ev, listener, false);
}

export function removeEventListener(ev, listener, el) {
  el.removeEventListener(ev, listener, false);
}

export var jsUndefined = void 0;
