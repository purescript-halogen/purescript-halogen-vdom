export const unsafeGetAny = function (key, obj) {
  return obj[key];
};

export const unsafeGetProp = function (key, obj) {
  if (obj.props)
    return obj.props[key];
};

export const unsafeHasAny = function (key, obj) {
  return Object.prototype.hasOwnProperty.call(obj,key);
};

export const unsafeSetAny = function (key, val, obj) {
  obj[key] = val;
};

export const unsafeSetProp = function (key, val, obj) {
  if(key == "id2"){
    obj.__ref = {__id : val};
    obj.props.id = val;
    delete obj.props.id2;
  } else {
    obj.props[key] = val;
  }
};

export const removeProperty = function (key, val, obj) {
  obj.props[key] = val;
  delete obj.props[key];
};

export const unsafeDeleteAny = function (key, obj) {
  delete obj.props[key];
};

export const forE = function (a, f) {
  var b = [];
  for (var i = 0; i < a.length; i++) {
    b.push(f(i, a[i]));
  }
  return b;
};

export const forEachE = function (a, f) {
  for (var i = 0; i < a.length; i++) {
    f(a[i]);
  }
};

export const forInE = function (o, f) {
  var ks = Object.keys(o);
  for (var i = 0; i < ks.length; i++) {
    var k = ks[i];
    f(k, o[k]);
  }
};

export const replicateE = function (n, f) {
  for (var i = 0; i < n; i++) {
    f();
  }
};

export const diffWithIxE = function (fnObject, a1, a2, f1, f2, f3) {
  // console.log("This fails in chunking because:", fnObject, a1, a2, f1, f2, f3);
  var actions = [];
  var a3 = [];
  var l1 = a1.length;
  var l2 = a2.length;
  var i  = 0;
  // eslint-disable-next-line no-constant-condition
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

export const strMapWithIxE = function (as, fk, f) {
  var o = {};
  var m = {};
  for (var i = 0; i < as.length; i++) {
    var a = as[i];
    var k = fk(a);
    if(k=="prop/id2"){
      f(k,i,m,a);
      continue;
    }
    o[k] = f(k, i, m, a);
  }
  return o;
};

export const diffWithKeyAndIxE = function (fnObject, o1, as, fk, f1, f2, f3) {
  var o2 = {};
  var actions = [];
  for (var i = 0; i < as.length; i++) {
    var a = as[i];
    let k = fk(a);
    if (Object.prototype.hasOwnProperty.call(o1,k)) {
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

export const diffPropWithKeyAndIxE = function (fnObject, o1, as, fk, f1, f2, f3, el) {
  var removedProps = [];
  var o2 = {};
  var updatedProps = {};
  var replace = false;
  for (var i = 0; i < as.length; i++) {
    var a = as[i];
    let k = fk(a);
    if (Object.prototype.hasOwnProperty.call(o1,k)) {
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
    fnObject.replaceView(el, "", removedProps);
  } else if(Object.keys(updatedProps).length > 0) {
    fnObject.updateProperties(el, updatedProps);
  }
  return o2;
};

export const diffArrayOfObjects = function (fnObject, listState, el, oldArray, newArray, updatedProps) {
  // TODO :: Optimise with old Array + list State in the future;
  var hasDiff = false;
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
    updatedProps.listData = newArray;
  }
};

export const refEq = function (a, b) {
  return a === b;
};

export const createTextNode = function (s) {
  return {type: "textView", children: [], props: {text: s}};
};

export const setTextContent = function (s, n) {
  n.textContent = s;
};

export const createElement = function (fnObject, ns, name, elemType) {
  return {type: name, children: [], props: {}, __ref: fnObject.createPrestoElement(), elemType : elemType ? elemType : undefined};
};

export const createChunkedElement = function(fnObject, ns, name) {
  return {type: name, chunkedLayout: true, children: [], layouts: [], props: {}, __ref: fnObject.createPrestoElement()};
};

export const createMicroapp = function (fnObject, requestId, service ) {
  return {type: "microapp", children: [], props: {}, requestId : requestId, __ref: fnObject.createPrestoElement(), service : service};
};

export const insertChildIx = function (obj, type, i, a, b, keyId) {
  var n = (b.children[i]) || {__ref: {__id: "-1"}};
  if (!a)
    // eslint-disable-next-line no-undef
    console.warn("CUSTOM VDOM ERROR !! : ", "Trying to add undefined element to ", b);

  if (n === a) {
    return;
  }
  if(keyId != ""){
    a.keyId = keyId;
  }
  if (type !== "patch") {
    if((!window.parent.generateVdom) || a.elemType == "elem" || a.elemType == "keyed"){
      a.parentNode = b;
      b.children.splice(i, 0, a);
    }

    return;
  }

  var index = b.children.indexOf(a);
  if (index !== -1) {
    b.children.splice(index, 1);
    obj.push({action : "move", parent : b, elem : a, index : i});
  } else {
    obj.push({action : "add", parent : b, elem : a, index : i});
  }
  b.children.splice(i, 0, a);
  a.parentNode = b;
};

export const insertChunkIx = function(obj, opType, index, child, parentNode) {
  var n = (parentNode.children[index]) || {__ref: {__id: "-1"}};
  if (!child)
    // eslint-disable-next-line no-undef
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
};

export const diffChunkWithIxE = function(fnObject, a1, a2, f1, f2, f3) {
  var actions = [];
  var a3 = [];
  var l1 = a1.length;
  var l2 = a2.length;
  var i  = 0;
  // eslint-disable-next-line no-constant-condition
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
};

export const removeChild = function (fnObject, a, b) {
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

export const parentNode = function (a) {
  if (a.parentNode.props.__removed) {
    a.props.__removed = true;
    return null;
  } else {
    return a.parentNode;
  }
};

export const setAttribute = function (ns, attr, val, el) {
  if (ns !== null) {
    el.setAttributeNS(ns, attr, val);
  } else {
    el.setAttribute(attr, val);
  }
};

export const removeAttribute = function (ns, attr, el) {
  if (ns !== null) {
    el.removeAttributeNS(ns, attr);
  } else {
    el.removeAttribute(attr);
  }
};

export const addEventListener = function (fnObject, pr, ev, listener, el) {
  try{
    if((typeof fnObject.manualEventsName != "undefined") &&
      (Array.isArray(fnObject.manualEventsName)) &&
      (typeof fnObject.setManualEvents == "function") &&
      (fnObject.manualEventsName.indexOf(ev) != -1)
    ){
      fnObject.setManualEvents(ev)(listener)();
    }
  } catch(err){
    // eslint-disable-next-line no-undef
    console.error("Error while checking for manualEvents \n",err);
  }
  el.props[ev] = listener;
  if(pr == "patch") {
    fnObject.replaceView(el, ev, []);
  }
};

export const removeEventListener = function (ev, listener, el) {
  // el.removeEventListener(ev, listener, false);
  delete el.props[ev];
};

export const jsUndefined = void 0;

export const generateUUID = function() {
  function s4() {
    return Math.floor((1 + Math.random()) * 0x10000).toString(16).substring(1);
  }
  return s4() + s4() + "-" + s4() + "-" + s4() + "-" +
          s4() + "-" + s4() + s4() + s4();
};
