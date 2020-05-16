exports.mkSet = function() {
  return new Set()
}

exports.removeSetMember = function(value, set) {
  set.remove(value)
}

exports.addSetMember = function(value, set) {
  set.add(value)
}

exports.setSize = function(set) {
  return set.size
}

exports.setToArray = function(set) {
  return Array.from(set)
}
