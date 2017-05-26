## TODO: make this into a good library
## also think about implementing syntax sugar like magrittr!

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
