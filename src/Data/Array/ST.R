list(
  new = function() list(len = 0, elems = list()),
  peekImpl = function(just) function(nothing) function(i) function(xs) function() if (i >= 0 && i < xs$len) just(xs$elems[[i + 1]]) else nothing,
  poke = function(i) {
    function(a) {
      function(xs) {
        function() {
          if (i < 0 || i >= xs$len) {
            return(F)
          }
          xs$elems[[i + 1]] <- a
          return(T)
        }
      }
    }
  },
  popImpl = function(just) {
    function(nothing) {
      function(xs) {
        function() {
          if (xs$len > 0) {
            xs$len <- xs$len - 1
            just(xs[[xs$len + 1]])
          } else {
            nothing
          }
        }
      }
    }
  },
  pushAll = function(as) {
    function(xs) {
      function() {
        xs$elems <- c(xs$elems, as)
        xs$len <- xs$len + length(as)
      }
    }
  },
  shiftImpl = function(just) {
    function(nothing) {
      function(xs) {
        function() {
          if (xs$len == 0) {
            return(nothing)
          }
          x <- xs$elems[[1]]
          xs$len <- xs$len - 1
          xs$elems <- xs$elems[-1]
          just(x)
        }
      }
    }
  },
  unshiftAll = function(as) {
    function(xs) {
      function() {
        xs$elems <- c(as, xs$elems)
        xs$len <- xs$len + length(as)
      }
    }
  },
  splice = function(i) {
    function(howMany) {
      function(bs) {
        function(xs) {
          function() {
            prefix <- if (i > 0) xs$elems[1:i] else list()
            suffix <- if (i + howMany < xs$len) xs$elems[(i + howMany + 1):xs$len] else list()
            xs$elems <- c(prefix, bs, suffix)
            xs$len <- length(xs$elems)
            xs
          }
        }
      }
    }
  },
  unsafeFreeze = function(xs) function() xs$elems,
  unsafeThaw = function(xs) function() list(len = length(xs), elems = xs),
  copyImpl = function(xs) function() xs,
  freeze = function(xs) function() xs$elems,
  thaw = function(xs) function() list(len = length(xs), elems = xs),
  sortByImpl = function(compare) {
    function(fromOrdering) {
      function(xs) {
        function() {
          sort <- function(xs) {
            if (length(xs) <= 1) {
              return(xs)
            }
            head <- xs[[1]]
            tail <- xs[-1]
            msk <- sapply(tail, function(x) fromOrdering(compare(head, xs))) < 0
            c(sort(tail[msk]), head, sort(tail[!msk]))
          }
          xs$elems <- sort(xs$elems)
          xs
        }
      }
    }
  },
  toAssocArray = function(xs) function() list(len = length(xs), elems = lapply(seq(along = xs), function(i) list(value = xs[[i]], index = i - 1)))
)
