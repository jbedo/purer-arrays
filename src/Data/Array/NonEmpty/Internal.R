list(
  foldr1Impl = function(f) {
    function(xs) {
      acc <- xs[[length(xs)]]
      if (length(xs) > 1) {
        for (i in (length(xs) - 1):1) acc <- f(xs[[i]])(acc)
      }
      acc
    }
  },
  foldl1Impl = function(f) {
    function(xs) {
      acc <- xs[[1]]
      if (length(xs) > 1) {
        for (i in 1:(length(xs) - 1)) acc <- f(acc)(xs[[i]])
      }
      acc
    }
  },
  traverse1Impl = (function() {
    cons <- function(a) function(b) c(a, b)
    function(apply) {
      function(map) {
        function(f) {
          buildFrom <- function(x, ys) apply(map(cons)(f(x)))(ys)
          function(array) {
            go <- function(acc, len) {
              if (len == 0) {
                return(list(isCont = F, acc = acc))
              }
              last <- array[[len]]
              list(isCont = T, cont = function() go(buildFrom(last, acc), len - 1))
            }
            acc <- map(list)(f(array[[length(array)]]))
            result <- go(acc, length(array) - 1)
            while (result$isCont) {
              result <- result$cont()
            }
            result$acc
          }
        }
      }
    }
  })()
)
