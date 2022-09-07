"use strict";

// removeEventListener
// removeAttribute
// removeProperty
// pokeMutMap
// addEventListener
// setAttribute


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

exports.diffWithIxE = function (fnObject, a1, a2, f1, f2, f3) {
  // console.log("This fails in chunking because:", fnObject, a1, a2, f1, f2, f3);
  var actions = [];
  var a3 = [];
  var l1 = a1.length;
  var l2 = a2.length;
  var i  = 0;
  while (1) {
    if (i < l1) {
      if (i < l2) {
        a3.push(f1(actions, i, a1[i], a2[i]));
      } else {
        f2(actions, i, a1[i]);
      }
    } else if (i < l2) {
      a3.push(f3(actions, i, a2[i]));
    } else {
      break;
    }
    i++;
  }
  if(actions.length > 0) {
    fnObject.updateChildren(actions);
  }
  return a3;
};

exports.strMapWithIxE = function (as, fk, f) {
  var o = {};
  var m = {};
  for (var i = 0; i < as.length; i++) {
    var a = as[i];
    var k = fk(a);
    o[k] = f(k, i, m, a);
  }
  return o;
};

exports.diffWithKeyAndIxE = function (fnObject, o1, as, fk, f1, f2, f3) {
  var o2 = {};
  var actions = [];
  for (var i = 0; i < as.length; i++) {
    var a = as[i];
    var k = fk(a);
    if (o1.hasOwnProperty(k)) {
      o2[k] = f1(actions, k, i, o1[k], a);
    } else {
      o2[k] = f3(actions, k, i, a);
    }
  }
  for (var k in o1) {
    if (k in o2) {
      continue;
    }
    f2(actions, k, o1[k]);
  }
  if(actions.length > 0) {
    fnObject.updateChildren(actions);
  }
  return o2;
};

exports.diffPropWithKeyAndIxE = function (fnObject, o1, as, fk, f1, f2, f3, el) {
  var removedProps = [];
  var o2 = {};
  var updatedProps = {}
  var replace = false;
  for (var i = 0; i < as.length; i++) {
    var a = as[i];
    var k = fk(a);
    if (o1.hasOwnProperty(k)) {
      o2[k] = f1(k, i, updatedProps, o1[k], a);
    } else {
      o2[k] = f3(k, i, updatedProps, a);
    }
  }
  for (var k in o1) {
    if (k in o2) {
      continue;
    }
    replace = true;
    f2(k, o1[k]);
    removedProps.push(k);
  }
  if (replace) {
    for(var key in updatedProps) {
      el.props[key] = updatedProps[key];
    }
    fnObject.replaceView(el, removedProps);
  } else if(Object.keys(updatedProps).length > 0) {
    fnObject.updateProperties(el, updatedProps);
  }
  return o2;
};

exports.diffArrayOfObjects = function (fnObject, listState, el, oldArray, newArray, updatedProps) {
  // TODO :: Optimise with old Array + list State in the future;
  var hasDiff = false
  if(oldArray.length != newArray.length) {
    hasDiff = true;
  } else {
    for(var j = 0; j < newArray.length; ++j) {
      for(var key in newArray[j]) {
        hasDiff = newArray[j][key] != oldArray[j][key];
        if(hasDiff)
          break;
      }
      if(hasDiff)
        break;
    }
  }
  if(hasDiff) {
    updatedProps.listData = newArray
  }
}

exports.refEq = function (a, b) {
  return a === b;
};

exports.createTextNode = function (s) {
  return {type: "textView", children: [], props: {text: s}}
};

exports.setTextContent = function (s, n) {
  n.textContent = s;
};

exports.createElement = function (fnObject, ns, name) {
  return {type: name, children: [], props: {}, __ref: fnObject.createPrestoElement()};
};

exports.createChunkedElement = function(fnObject, ns, name) {
  return {type: name, chunkedLayout: true, children: [], layouts: [], props: {}, __ref: fnObject.createPrestoElement()};
}

exports.createMicroapp = function (fnObject, requestId, service ) {
  return {type: "microapp", children: [], props: {}, requestId : requestId, __ref: fnObject.createPrestoElement(), service : service};
};

exports.insertChildIx = function (obj, type, i, a, b) {
  var n = (b.children[i]) || {__ref: {__id: "-1"}};
  if (!a)
    console.warn("CUSTOM VDOM ERROR !! : ", "Trying to add undefined element to ", b);

  if (n === a) {
    return;
  }

  if (type !== "patch") {
    a.parentNode = b;
    b.children.splice(i, 0, a);

    return;
  }

  var index = b.children.indexOf(a);
  if (index !== -1) {
    b.children.splice(index, 1);
    obj.push({action : "move", parent : b, elem : a, index : i})
  } else {
    obj.push({action : "add", parent : b, elem : a, index : i})
  }
  b.children.splice(i, 0, a);
  a.parentNode = b;
};

exports.insertChunkIx = function(obj, opType, index, child, parentNode) {
  var n = (parentNode.children[index]) || {__ref: {__id: "-1"}};
  if (!child)
    console.warn("CUSTOM VDOM ERROR !! : ", "Trying to add undefined element to ", parentNode);

  if (n === child) {
    return;
  }
  if (opType !== "patch") {
    child.layout.parentNode = child.shimmer.parentNode = parentNode;
    parentNode.children.splice(index, 0, child.shimmer);
    parentNode.layouts.splice(index, 0, child.layout);
    return;
  }
}

exports.diffChunkWithIxE = function(fnObject, a1, a2, f1, f2, f3) {
  var actions = [];
  var a3 = [];
  var l1 = a1.length;
  var l2 = a2.length;
  var i  = 0;
  while (1) {
    if (i < l1) {
      if (i < l2) {
        a3.push(f1(actions, i, a1[i].layout, a2[i]));
      } else {
        f2(actions, i, a1[i].layout);
      }
    } else if (i < l2) {
      a3.push(f3(actions, i, a2[i]));
    } else {
      break;
    }
    i++;
  }
  if(actions.length > 0) {
    fnObject.updateChildren(actions);
  }
  return a3;
}

exports.removeChild = function (fnObject, a, b) {
  var childIndex = -1;

  if (b && a.parentNode.__ref.__id === b.__ref.__id) {
    for (var i=0; i<b.children.length; i++) {
      if (b.children[i].__ref.__id == a.__ref.__id) {
        childIndex = i;
      }
    }
  }

  if (childIndex > -1) {
    fnObject.removeChild(a,b,childIndex);
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

exports.addEventListener = function (fnObject, pr, ev, listener, el) {
  try{
    if((typeof fnObject.manualEventsName != "undefined") &&
      (Array.isArray(fnObject.manualEventsName)) &&
      (typeof fnObject.setManualEvents == "function") &&
      (fnObject.manualEventsName.indexOf(ev) != -1)
    ){
      fnObject.setManualEvents(ev)(listener)();
    }
  } catch(err){
    console.error("Error while checking for manualEvents \n",err);
  }
  el.props[ev] = listener;
  if(pr == "patch") {
    fnObject.replaceView(el, []);
  }
};

exports.removeEventListener = function (ev, listener, el) {
   // el.removeEventListener(ev, listener, false);
   delete el.props[ev];
};

exports.jsUndefined = void 0;

exports.generateUUID = function() {
  function s4() {
          return Math.floor((1 + Math.random()) * 0x10000)
                  .toString(16)
                  .substring(1);
  }
  return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
          s4() + '-' + s4() + s4() + s4();
}