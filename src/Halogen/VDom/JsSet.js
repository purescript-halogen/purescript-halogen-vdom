exports.empty = function() {
  return new Set()
}

exports._delete = function(value, set) {
  set.delete(value)
}

exports._add = function(value, set) {
  set.add(value)
}

exports._size = function(set) {
  return set.size
}

exports._has = function(value, set) {
  return set.has(value)
}

exports._toArray = function(set) {
  return Array.from(set)
}
