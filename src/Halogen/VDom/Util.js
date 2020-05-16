"use strict";

exports.unsafeGetAny = function (key, obj) {
  return obj[key];
};

exports.unsafeHasAny = function (key, obj) {
  return obj.hasOwnProperty(key);
};

exports.unsafeSetAny = function (key, val, obj) {
    obj[key] = val;
};

exports.unsafeDeleteAny = function (key, obj) {
  delete obj[key];
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

exports.diffWithIxE = function (oldElems, newElems, onBothElements, onOldElement, onNewElement) {
  var outputs = [];
  var oldElemsLength = oldElems.length;
  var newElemsLength = newElems.length;
  var i  = 0;
  while (1) {
    if (i < oldElemsLength) {
      if (i < newElemsLength) {
        outputs.push(onBothElements(i, oldElems[i], newElems[i]));
      } else {
        onOldElement(i, oldElems[i]);
      }
    } else if (i < newElemsLength) {
      outputs.push(onNewElement(i, newElems[i]));
    } else {
      break;
    }
    i++;
  }
  return outputs;
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

exports.strMapWithIxE = function (children, propToStrKey, f) {
  var o = {};
  for (var i = 0; i < children.length; i++) {
    var child = children[i];
    var key = propToStrKey(child);
    o[key] = f(key, i, child);
  }
  return o;
};

exports.refEq = function (a, b) {
  return a === b;
};

exports.createTextNode = function (s, doc) {
  return doc.createTextNode(s);
};

exports.setTextContent = function (s, n) {
  n.textContent = s;
};

exports.createElement = function (ns, name, doc) {
  if (ns != null) {
    return doc.createElementNS(ns, name);
  } else {
    return doc.createElement(name)
  }
};

exports.insertChildIx = function (i, elem, parent) {
  var referenceNode = parent.childNodes.item(i) || null;
  if (referenceNode !== elem) {
    // insert before referenceNode, if referenceNode is null - inserted at the end
    parent.insertBefore(elem, referenceNode);
  }
};

exports.removeChild = function (elem, parent) {
  if (parent && elem.parentNode === parent) {
    parent.removeChild(elem);
  }
};

exports.parentNode = function (elem) {
  return elem.parentNode;
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

exports.hasAttribute = function (ns, attr, el) {
  if (ns != null) {
    return el.hasAttributeNS(ns, attr);
  } else {
    return el.hasAttribute(attr);
  }
};

exports.getAttribute = function (ns, attr, el) {
  if (ns != null) {
    return el.getAttributeNS(ns, attr);
  } else {
    return el.getAttribute(attr);
  }
};

exports.addEventListener = function (ev, listener, el) {
  el.addEventListener(ev, listener, false);
};

exports.removeEventListener = function (ev, listener, el) {
  el.removeEventListener(ev, listener, false);
};

exports.jsUndefined = void 0;

exports.getNodeType = function(el) {
  return el.nodeType
}

exports.nodeIsTextNode = function(el) {
  return el.nodeType === 3
}

exports.nodeIsElementNode = function(el) {
  return el.nodeType === 1
}

exports.getTextContent = function(el) {
  return node.textContent;
}

exports.getNamespaceURI = function(el) {
  return node.namespaceURI
}

exports.anyToString = function (a) {
  return String(a);
};

exports.warnAny = function(message, x) {
  console.warn(message, x)
}

exports.logAny = function(message, x) {
  console.log(message, x)
}
