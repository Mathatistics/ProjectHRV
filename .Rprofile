options(stringsAsFactors = FALSE)

options("repos" = c(CRAN = "https://cran.rstudio.com/"))

.env <- new.env()
.env$"%nin%" <- function(x, y) !(x %in% y) 
.env$n <- function(df) matrix(names(df)) 
.env$s <- base::summary
.env$h <- utils::head
.env$ht <- function(d) rbind(head(d,10),tail(d,10))
.env$hh <- function(d) if (class(d) == "matrix" | class(d) == "data.frame") d[1:5, 1:5]
.env$read.cb <- function(...) {
  ismac <- Sys.info()[1] == "Darwin"
  if (!ismac) read.table(file = "clipboard", ...)
  else read.table(pipe("pbpaste"), ...)
}
.env$unrowname <- function(x) {
  rownames(x) <- NULL
  x
}
.env$lsa <- function() {
    obj_type <- function(x) class(get(x, envir = .GlobalEnv)) # define environment
    foo = data.frame(sapply(ls(envir = .GlobalEnv), obj_type))
    foo$object_name = rownames(foo)
    names(foo)[1] = "class"
    names(foo)[2] = "object"
    return(unrowname(foo))
}
  
.env$lsp <- function(package, all.names = FALSE, pattern) {
  package <- deparse(substitute(package))
  ls(
    pos = paste("package", package, sep = ":"),
    all.names = all.names,
    pattern = pattern
  )
}
.env$macopen <- function(...) if (Sys.info()[1] == "Darwin") system("open .")
.env$o       <- function(...) if (Sys.info()[1] == "Darwin") system("open .")
attach(.env)

.First <- function() {
  auto.loads <- c('utils', 'ggplot2', 'data.table', 'reshape2')
  silentLoadPackage <- function(pkgs){
    suppressWarnings(suppressPackageStartupMessages(
      library(pkgs, character.only = TRUE, warn.conflicts = FALSE)))
  }
  invisible(sapply(auto.loads, silentLoadPackage))
  cat("\014") 
  cat("Workspace: [", getwd(), ']\n')
  timestamp(prefix = '', suffix = '')
  rm(list = ls())
  cat('I am ready Mr. Rimalaya!\n')
  cat('Welcome to Project HRV!!\n')
}

.Last <- function() {
  hist_file <- Sys.getenv("R_HISTFILE")
  if (hist_file == "") hist_file <- "~/.RHistory"
  savehistory(hist_file)
  cat("\nGoodbye at ", date(), "\n")
}