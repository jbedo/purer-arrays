list(
  peekImpl = function(i) function(xs) function() xs$elems[[i + 1]],
  pokeImpl = function(i) function(a) function(xs) function() x$elems[[i + 1]] <- a
)
