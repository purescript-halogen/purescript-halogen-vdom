"use strict";

exports.eqPropValues = function (x) {
  return function (y){
    return x == y;
  };
};
