"use strict";

exports.createTextNode = function (s, doc) {
  return function () {
    return doc.createTextNode(s);
  };
};

exports.setTextContent = function (s, n) {
  return function () {
    n.textContent = s;
  };
};

exports.createElement = function (name, doc) {
  return function () {
    return doc.createElement(name)
  };
};

exports.createElementNS = function (ns, name, doc) {
  return function () {
    return doc.createElementNS(ns, name);
  };
};

exports.tryRemoveChild = function (a, b) {
  return function () {
    try {
      b.removeChild(a);
    } catch (e) {
    }
  };
};

exports.removeLastChild = function (a) {
  return function () {
    a.removeChild(a.lastChild);
  };
};

exports.insertChildIx = function (i, a, b) {
  return function () {
    b.insertBefore(a, b.childNodes.item(i) || null);
  };
};

exports.unsafeChildIx = function (i, d) {
  return function () {
    return d.childNodes.item(i);
  };
};
