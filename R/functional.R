zipWith <- function(a, b, f) {
    if (length(a) != length(b)) { stop; }
    if (typeof(a) != typeof(b)) { stop; }
    ret <- vector(typeof(a), length(a))
    for (i in 1:length(a)) { ret[i] = f(a[i], b[i]); }
    return(ret);
}

map <- function(l, f) {
    ret <- vector(typeof(l), length(l))
    for (i in 1:length(l)) {
        ret[i] = f(l[i])
    }
    return(ret)
}
