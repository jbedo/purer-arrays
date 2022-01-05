list(
  range = function(start) function(end) as.list(start:end),
  replicate = function(count) function(value) as.list(rep(value, count)),
  fromFoldableImpl = (function() {
    cons <- function(head) function(tail) c(head, tail)
    function(foldr) function(xs) (foldr(cons)(list())(xs))
  })(),
  length = length,
  unconsImpl = function(empty) function(nex) function(xs) if (length(xs) == 0) empty(xs) else nex(xs[[1]])(xs[-1]),
  indexImpl = function(just) function(nothing) function(xs) function(i) if (i < 0 || i >= length(xs)) nothing else just(xs[[i + 1]]),
  findMapImpl = function(nothing) {
    function(isJust) {
      function(f) {
        function(xs) {
          for (i in seq(along = xs)) {
            if (isJust(result <- f(xs[[i]]))) {
              return(result)
            }
          }
          return(nothing)
        }
      }
    }
  },
  findIndexImpl = function(just) {
    function(nothing) {
      function(f) {
        function(xs) {
          for (i in seq(along = xs)) {
            if (f(xs[[i]])) {
              return(just(i - 1))
            }
          }
          return(nothing)
        }
      }
    }
  },
  findLastIndexImpl = function(just) {
    function(nothing) {
      function(f) {
        function(xs) {
          for (i in rev(seq(along = xs))) {
            if (f(xs[[i]])) {
              return(just(i - 1))
            }
          }
          return(nothing)
        }
      }
    }
  },
  insertAt_ = function(just) {
    function(nothing) {
      function(i) {
        function(a) {
          function(l) {
            if (i < 0 || i > length(l)) {
              return(nothing)
            }
            prefix <- if (i > 0) l[1:i] else list()
            suffix <- if (i < length(l)) l[(i + 1):length(l)] else list()
            c(prefix, a, suffix)
          }
        }
      }
    }
  },
  deleteAt_ = function(just) {
    function(nothing) {
      function(i) {
        function(l) {
          if (i < 0 || i >= length(l)) {
            return(nothing)
          }
          l[-(i + 1)]
        }
      }
    }
  },
  updateAt_ = function(just) {
    function(nothing) {
      function(i) {
        function(a) {
          function(l) {
            if (i < 0 || i >= length(l)) {
              return(nothing)
            }
            prefix <- if (i > 0) l[1:i] else list()
            suffix <- if (i < length(l) - 1) l[(i + 1):length(l)] else list()
            c(prefix, a, suffix)
          }
        }
      }
    }
  },
  reverse = rev,
  concat = c,
  filter = function(f) function(xs) xs[sapply(xs, f)],
  partition = function(f) {
    function(xs) {
      msk <- sapply(xs, f)
      list(yes = xs[msk], no = xs[!msk])
    }
  },
  scanl = function(f) {
    function(b) {
      function(xs) {
        acc <- b
        ys <- xs
        for (i in seq(along = xs)) {
          acc <- f(acc)(xs[[i]])
          ys[[i]] <- acc
        }
        ys
      }
    }
  },
  scanr = function(f) {
    function(b) {
      function(xs) {
        acc <- b
        ys <- xs
        for (i in seq(along = xs)) {
          acc <- f(xs[[i]])(acc)
          ys[[i]] <- acc
        }
        ys
      }
    }
  },
  sortByImpl = function(compare) {
    function(fromOrdering) {
      sort <- function(xs) {
        if (length(xs) <= 1) {
          return(xs)
        }
        head <- xs[[1]]
        tail <- xs[-1]
        msk <- sapply(tail, function(y) fromOrdering(compare(head)(y))) < 0
        c(sort(tail[msk]), head, sort(tail[!msk]))
      }
    }
  },
  slice = function(s) function(e) function(l) l[(s + 1):e],
  zipWith = function(f) {
    function(xs) {
      function(ys) {
        n <- min(length(xs), length(ys))
        if (n == 0) {
          return(list())
        }
        lapply(1:n, function(i) f(xs[[i]])(ys[[i]]))
      }
    }
  },
  any = function(p) function(xs) any(sapply(xs, p)),
  all = function(p) function(xs) all(sapply(xs, p)),
  unsafeIndexImpl = function(xs) function(i) xs[[i + 1]]
)
