# Call sapply() over a series of list type-columns and returns them as vectors

fapply = function (data, vars, FUN,...){
    apply(data[vars],
          2, # on columns
          function(x) sapply(x,FUN))
}
