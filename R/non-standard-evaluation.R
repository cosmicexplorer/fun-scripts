## TODO: make this into a good library
## also think about implementing syntax sugar like magrittr!

## TODO: make a way to merge environments which respects parenthood (transitive
## parenthood too!), but also allows the order of the arguments (or some other
## metric) to determine precedence of bindings in merged

## TODO: make a way to iterate over both keys and values at the same time for an
## env, named vector, or named list (just getting the names list and running off
## of that alone assumes unique names or is hard lol)

switch_do_default_desc <- "input"

switch_do <- function(type_val, desc = switch_do_default_desc, ...) {
    clauses <- as.list(substitute(list(...)))[-1L]
    recognized <- names(clauses)
    stopifnot(length(clauses) > 0 &&
              !any(duplicated(recognized)))
    if (type_val %in% recognized) {
        eval.parent(clauses[[type_val]])
    } else {
        clause_names_joined <- paste(recognized, collapse = ", ")
        stop(sprintf(
            paste("argument '%s' was not recognized as an instance of '%s'.",
                  "recognized values:\n%s"),
            type_val, desc, clause_names_joined))
    }
}

switch_do_apply <- function (actions, val_arg, desc = switch_do_default_desc) {
    eval(as.call(c(quote(switch_do), val_arg, desc, actions)))
}

build_matcher <- function (pat) {
    if (is.primitive(pat))
    if (is.function(pat)) { pat }
}

valid_name_p <- function (names, require_uniq = F) {
    make.names(names, unique = require_uniq) == names
}

funcall <- function (fn, env = parent.frame(), ...) {
    args <- as.list(substitute(list(...)))[-1L]
    do.call(fn, args, envir = env, quote = T)
}

env <- function (parent = emptyenv(), ) {
    created <- new.env(parent = parent)
}

library(hash)
all_derived <- function (x, climb) {
    used <- env()
    new <- env(x)
    while (length(new) != 0) {
        result <- eapply()
    }
}

derived_p <- function (parent_set, x, climb) {
    tbl <- hash()
    Find(x = parent_envs, f = function (env) )
}

merge_environments <- function (envs, precedence = ) {

}

make_macro_builtins_default <- quote(list(
    pf = parent.frame(),
    cf = sys.frame(),
    args = as.list(substitute(list(...)))[-1L]))

make_macro_fun <- function (name,
                            builtins = make_macro_builtins_default,
                            pf = parent.frame(),
                            block) {
    with_builtins <- substitute(block, builtins)
    eval(substitute(name), pf)
}

gensym_counter <- integer(0)

gensym_reset_counter <- function (pf = parent.frame(),
                                  counter_quoted = quote(gensym_counter)) {
    if (!is.integer(gensym_counter)) {

    }
}

gensym <- function (frame = parent.frame()) {
    gensym_counter
}

zero_arg_func <- function (block, frame) {
    function () {
        eval(subbed, frame)
    }
}

single_arg_func <- function (block, frame) {
    function ()
}

default_argspec <- list(
    types = list(
        list(matcher = zero_arg_func, restriction = quote(exclusive)),
        list(matcher = single_arg_func, arg_matcher = "^_$"),
        list(name = "positional", arg_matcher = "^_([0-9]+)$")
        list(name = "named", arg_matcher = "")),
    default = )

make_lambda <- function (argspec, block) {
    subbed <- substitute(block)
    pf <- parent.frame()
    function () {
        eval(subbed, pf)
    }
}
## z <- 1
## f <- make_lambda({print("woah"); z + 2})
## z <- 4
## f()

## NOTE: this is more of a utility function than a macro -- still applicable, i
## think
extract_delims <- function (n, delim_maybe,
                            null_alt = rep("", n),
                            true_alt = rep("", n)) {
    result <- if (is.null(delim_maybe)) { null_alt }
              else if (delim_maybe) { true_alt }
              else { delim_maybe }
    stopifnot(is.vector(result) &&
              length(result) == n &&
              is.character(result))
    result
}

make_delim_extractor <- function (true_default) {
    stopifnot(is.vector(true_default) &&
              is.character(true_default))
    n <- length(true_default)
    function (arg) {
        extract_delims(n, arg, true_alt = true_default)
    }
}

paren_delims <- make_delim_extractor(c("(", ")"))


## entries is a named list with quoted values -- a NULL value means a non-value
## wrap this with as.pairlist to use it as a function -- see "anon_one"
make_alist <- function (keys, values, env = parent.frame()) {
    len <- length(keys)
    stopifnot(!any(duplicated(keys)) &&
              len == length(values))
    ret <- rep(alist(,)[1], len)
    names(ret) <- eval(keys, env)
    for (i in 1:len) {
        key <- keys[[i]]
        val <- values[[i]]
        if (!is.null(val)) {
            ret[[key]] <- eval(val, env)
        }
    }
    ret
}

add_entries_env <- function (env, entries) {
    created <- new.env(parent = env)
    for (n in names(entries)) {
        assign(n, entries[n], envir = created)
    }
    created
}

anon_one <- function (body, name = ".", env = parent.frame()) {
    args <- make_alist(list(name), list(NULL), env = env)
    eval(call("function", as.pairlist(args), substitute(body)), env)
}

option_pred <- is.null

with_bindings <- function (bindings, expr, env = parent.frame()) {
    cat("enter with bind\n")
    subbed <- substitute(expr)
    p(subbed)
    pf <- parent.frame()
    bind_sub <- eval(substitute(bindings), pf)
    p(bind_sub)
    bind_env <- list2env(bind_sub, parent = env)
    p(desc = "huh",
      get("option_pred", envir = bind_env))
    ret <- eval(subbed, bind_env, enclos = emptyenv())
    cat("exit with bind\n")
    ret
}

set_option_pred <- function (pred, block, env = parent.frame()) {
    cat('enter set pred\n')
    subbed_block <- substitute(block)
    p(subbed_block)
    toEval <- bquote(
        with_bindings(.(list(option_pred = match.fun(pred))),
                      .(subbed_block),
                      env = .(env)))
    p(toEval)
    ret <- eval(toEval)
    cat("exit set pred\n")
    ret
}

`%?%` <- function (lhs, rhs_nse, env = parent.frame()) {
    cat("enter op\n")
    pred <- get("option_pred", envir = env)
    p(pred)
    ret <- if (pred(lhs)) { lhs }
    else {
        rhs_expr <- substitute(rhs_nse)
        p(rhs_expr)
        bound_eval <- bquote(with_bindings(list(. = lhs), .(rhs_expr),
                                           env = .(env)))
        eval(bound_eval)
    }
    cat("exit op\n")
    ret
}

reduce_until_helper <- function (value, rest, test, f, default, frame) {
    delayedAssign("retVal", eval(default, frame))
    if (test(value)) { return(retVal) }
    for (el in rest) {
        value <- f(value, el)
        if (test(value)) { return(retVal) }
    }
}

reduce_until <- function (x, init, until, f, default, right = F, invert = F,
                          frame = parent.frame()) {
    test <- if (invert) { Negate(until) } else { until }
    directed <- if (right) { rev(x) } else { x }
    delayedAssign("accumulation", if (hasArg(init)) {
                                      list(val = init, rest = directed)
                                  } else {
                                      stopifnot(length())
                                      list(val = )
                                  })
    delayedAssign("retVal", if (hasArg(default)) { eval(default, frame) }
                            else { accumulated })
    if (hasArg(init)) {
        accumulated <- init
        hasValue = T
    } else if ()
    directed <- if (right) { rev(x) } else { x }
    accumulated <-
        if (hasArg(init)) {
            init
        } else if (length(x) == 0) {
            stop(paste("no 'init' or 'default' value provided",
                       "to return for zero-length collection 'x'"))
        } else { first(directed) }
    delayedAssign("directed", if (right) { rev(x) }
                              else { x })
    delayedAssign("accumulated",
                  )
    delayedAssign("rest", if (hasArg(init)) { directed }
                          else { tail(directed, n = -1) })

    should_exit <- if (invert) { quote(!until(accumulated)) }
                   else { quote(until(accumulated)) }
    if (xor(until(accumulated), invert)) {
        return(retVal)
    }
    for (el in rest) {
        accumulated <- f(accumulated, el)
        if (xor(until(accumulated), invert)) {
            return(retVal)
        }
    }
    accumulated
}

can_iterate <- function (arg, each, test) {
    if (!(is.list(arg) || is.vector(arg))) { return(FALSE) }
    if (length(arg) == 0) { return(FALSE) }
    if (hasArg(each)) {
        for (el in arg) {
            if (!each(el))
        }
    }

    if () {
        if (hasArg(each)) {
            for (el in arg) {
                if (!each(el)) { return(FALSE) }
            }
        }
        if (hasArg(test)) { test(arg) }
        TRUE
    }
        (!hasArg(test) || )
    if (is.list(arg)) {
        if (length(arg) == 0) {
            FALSE
        } else {
            if (hasArg(check)) {}
            checks <- lapply(arg, check)
            all(unlist(checks))
        } else { TRUE }
    } else if (is.vector(arg)) {

    } else { FALSE }
    if (!(is.list(arg) || is.vector(arg))) { return(FALSE) }
    if (length(arg) == 0) { return(FALSE) }
    if (hasArg(check)) {
        if (is.vector(arg)) { check(arg) }
        else if (is.list(arg)) {
            checks <- lapply(arg, check)
            all(unlist(checks))
        }
        for (el in arg) {
            if (!check(el)) { return(FALSE) }
        }
    }
    TRUE
}

do_unless <- function (val, block, pred = is.null, invert = F,
                       name = ".", frame = parent.frame()) {
    if (xor(pred(val), invert)) { return(val) }

    subbed <- substitute(block)
    print(subbed)
    env_with_val <- new.env(list(. = val), parent = frame)
    assign(name, val, envir = env_with_val)
    eval(subbed, envir = env_with_val, enclos = emptyenv())
}

split_at <- function (coll, init, frame = parent.frame()) {

    if (eval(hasArg(init), envir = frame, enclos = frame))
}

reduce_until <- function (seq, f, init, until, default,
                          frame = parent.frame()) {
    stopifnot(hasArg(init) || length(seq) != 0)
    delayedAssign()
    accum <- if (hasArg(init)) {
                 list(val = eval(init, frame), rest = seq)
             } else {
                 list(val = first(seq), rest = tail(seq, n = -1))
             }
    if (until(accum$val)) {  }

    Reduce(x = seq)
}

## TODO: make this print recursion depth!
p <- function (expr, desc, top = "---\n", bottom = "***\n", defn = "+++\n",
               pf = parent.frame()) {
    cat(top)
    if (hasArg(desc)) {
        cat(paste("(", desc, ")\n", sep = ""))
    }
    subbed <- substitute(expr)
    to_strings <- capture.output(print(subbed))
    out_str <- paste0(collapse = "\n", c(to_strings, ""))
    cat(out_str)
    cat(defn)
    ret <- eval(subbed, pf)
    print(ret)
    cat(bottom)
    expr
}
