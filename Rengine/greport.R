#' This is report execution script that should be handled by Rscript
#' command. 
#' 
#' For command line arguments reference please run it with --help. Check report 
#' documentation for report specific input parameters.
#'
#' Only 6 things should be visible within each report:
#' 1) apt_include - function to install packages
#' 2) global_user_variables - list with user provided variables
#' 3) apt_path_resolve - function for path resolution
#' 4) global_config - file path to R script that can redefine default variables
#' 5) apt_logger - general purpose logging function
#' 6) apt_logger_verbose - logging function for verbose output

#' General purpose logging function.
#' 
#' @section General purpose
#' @param ... what to log.
#' @examples
#' apt_logger("Log message:", list("multiple", "objects", "here"), "!")
apt_logger <- function(..., sep = " ") {
    tcont <- do.call(paste, c(unlist(list(...)), list(sep = sep)))
    suppressWarnings(
        message("#### ", as.character(Sys.time()), ": ", tcont)
    )
}

apt_logger("GReport command line: ", commandArgs(trailingOnly = TRUE))

.cran <- "http://cran.rstudio.com/"
.htmlOptions <- c("smartypants", "base64_images")
.startWd <- getwd()
.usageString <- "
Usage:
  greport.R -tFILE -dDIR [-cFILE] [-oFILE] [-uk] [-fDIR] [-eREXPR...] [-xREXPR...] [--notoc] [--verbose]

Options:
  -t FILE, --template=FILE  Path to RMD template.
  -o FILE, --output=FILE    Path to HTML report.
  -c FILE, --config=FILE    Path to configuration script.
  -d DIR, --Rbindir=DIR     Path to R library functions root.
  -k, --savepics            Keep pictures after the test.
  -f DIR, --picsdir=DIR     Path to pictures folder (no ending slash).
  -u, --update              Try to update all the packages.
  -e REXPR, --expr=REXPR    Evaluate R expression, like setting variable.
  -x REXPR, --sexpr=REXPR   Evaluate R expression and track value as string (always use '/' as file separators).
  --notoc                   Do not create table of contents.
  --verbose, -v             Be more verbose than default.
"

#' Dependency resolution and library inclusion function.
#' 
#' @param pkgs character vector of package names. Packages will be checked,
#' installed if required and included into global environment.
#' @param repositories character vector of repository CRAN URLs.
apt_include <- function(pkgs, repositories = .cran) {
    lib <- .libPaths()[1]
    .isPkgInstalled <- function(pkg) 
        pkg %in% installed.packages(lib.loc = lib)[, "Package"]
    
    for (dep in pkgs) {
        if (!.isPkgInstalled(dep)) {
            install.packages(
                dep, 
                dependencies = TRUE,
                repos = repositories, 
                lib = lib,
                quiet = TRUE,
                verbose = FALSE
            )
        }
        suppressWarnings(
            suppressPackageStartupMessages(
                library(
                    dep,
                    lib.loc = lib,
                    quietly = TRUE,
                    warn.conflicts = FALSE, 
                    character.only = TRUE,
                    verbose = FALSE
                )
            )
        )
    }
}

#' Relative paths resolution function (relative to working directory).
#' 
#' @param path any path to resolve.
#' @param exists specifies if existence file check should be done.
#' @param wd root working dir for relative paths resolution.
apt_path_resolve <- function(path, exists = FALSE, wd = .startWd) 
    normalizePath(
        getAbsolutePath(pathname = path, workDirectory = wd), 
        winslash = "/", 
        mustWork = exists
    )

apt_include(c("knitr", "markdown", "Cairo", "docopt", "Rcpp", "R.utils"), .cran)

.markdownFile <- tempfile(pattern = "temp", tmpdir = tempdir(), fileext = ".md")
.markdownFile <- apt_path_resolve(.markdownFile, exists = FALSE)

.opt <- docopt(.usageString, args = commandArgs(trailingOnly = TRUE))

#' Verbose output logging function. 
#' 
#' @param ... what to log.
apt_logger_verbose <- if (.opt$verbose) apt_logger else function(...) cat(NULL)

if (.opt$update) {
    apt_logger("Running libraries update...")
    update.packages(ask = FALSE, repos = .cran, instlib = .libPaths()[1], quiet = TRUE)
}

if (is.null(.opt$picsdir)) 
    .opt$picsdir <- paste0(tempdir(), "/figure")

.picsdir <- apt_path_resolve(.opt$picsdir, exists = FALSE)

opts_chunk$set(
    dev = "CairoPNG", 
    self.contained = TRUE, 
    dpi = 96,
    fig.path = sub("([^/])$", "\\1/", .picsdir)
)

.report <- ifelse(
    is.null(.opt$output),
    paste(basename(.opt$template), ".html", sep = ""), 
    .opt$output
)

.report <- apt_path_resolve(.report, exists = FALSE)
.template <- apt_path_resolve(.opt$template, exists = TRUE)

global_user_variables <- .opt$expr
if (!is.null(.opt$sexpr)) {
    global_user_variables <- c(global_user_variables, sapply(
        .opt$sexpr,
        function(expr) {
            if (grepl("=", expr)) sub("=(.*)$", "='\\1'", expr, perl = TRUE)
            else sprintf("'%s'", expr)
        }
    ))
}

apt_logger("User expressions: ", global_user_variables)

global_config <- .opt$config
if (!is.null(global_config))
    global_config <- apt_path_resolve(global_config, exists = TRUE)

tryCatch({
    sourceCpp(file.path(.opt$Rbindir, "functions.cpp"))
    source(file.path(.opt$Rbindir, "functions.R"))
    source(file.path(.opt$Rbindir, "mdutils.R"))
    source(file.path(.opt$Rbindir, "libreport.R"))
    source(file.path(.opt$Rbindir, "migrator.R"))
    
    global_base_path <- apt_path_resolve(.startWd, exists = TRUE)
    setwd(dirname(.opt$template))
    knit(.template, .markdownFile, quiet = TRUE, envir = new.env())
    markdownToHTML(
        .markdownFile, 
        output = .report, 
        options = c(.htmlOptions, if (.opt$notoc) NULL else "toc"),
        fragment.only = FALSE
    )
}, error = function(e) {
    apt_logger("ERROR: ", toString(e))
}, finally = {
    setwd(.startWd)
    if (file.exists(.markdownFile)) {
        apt_logger("Remove intermediate markdown file: ", .markdownFile)
        file.remove(.markdownFile)
    }
    if (!.opt$savepics && file.exists(.picsdir)) {
        apt_logger("Remove pictures dir: ", .picsdir)
        unlink(.picsdir, recursive = TRUE)
    }
})
