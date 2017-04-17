cur_dir <- dirname(sys.frame(1)$ofile)

cran <- getCRANmirrors()
narrowed <- tryCatch(error = function(e) cran, {
    locale_out <- system2("locale", stdout = T, stderr = F)
    lang_lines <- gsub(
        "^LANG=[a-z]+_([a-z]+).*$|.", "\\L\\1", locale_out,
        perl = T, ignore.case = T)
    ccode <- lang_lines[lang_lines != ""][1]
    cran[cran$CountryCode == ccode,]
})

npings <- 3

run_program <- function(hosts, argv, do_stdout = T) {
    prog <- argv[1]
    args <- argv[-1]
    stopifnot(Sys.which(prog) == "")
    system2(prog, args, input = hosts, stdout = do_stdout, stderr = F)
}

ping_parallel <- function(hosts, num_pings) {
    p <- as.character(num_pings)
    run_program(hosts,
                c("parallel", "--line-buffered", "-n", "1", "ping", "-c", p))
}

ping_script <- file.path(cur_dir, "ping-parallel.sh")
ping_xargs_var <- "XARGS_ENV"
ping_xargs <- function(hosts, num_pings) {
    p <- as.character(num_pings)
    tmpdir <- tempdir()
    tryCatch({
        run_program(hosts, do_stdout = F,
                    c("xargs", "-P", "-0", "-n", "1",
                      sprintf("--process-slot-var=%s", ping_xargs_var),
                      ping_script, "{}", tmpdir, p, ping_xargs_var))
        run_program(list.files(tmpdir), c("xargs", "cat"))
    }, finally = { unlink(tmpdir, T, T) } )
}
