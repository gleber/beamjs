var path = exports;

path.join = function() {
  return erlang.apply('filename', 'join', Array.prototype.slice.apply(arguments));
}
