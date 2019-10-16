#' Historical data migration utilities library.
#'
#' This file contains classes and functions that can be used by historical data
#' constructor to upgrade historical data from previous versions.

#' Create a list with specific version updagraders. Versions are ordered, i.e.
#' list("v0", "v1", ...). Each entry is a function that takes historical data 
#' dataset (what is stored in file) and returns dataset for the next version.
#' So upgrade is made in pipeline manner, where each upgrader cares only about
#' next version.
#' 
#' @param business_groups character vector with mapping between transaction 
#' prefixes and business groups (v0 -> v1 upgrader dependency).
#' @param group_missing if transaction name doesn't match any prefix in 
#' business_groups, business group will be set to this value 
#' (v0 -> v1 upgrader dependency).
#' @param tzinfo timezone tz string for test ids conversion from POSIXct to 
#' characters (v0 -> v1 upgrader dependency).
#' @param ... ignored.
#' @return ordered upgraders list where names are versions and values are 
#' actual functions.
create_migration_list <- function(business_groups = c(Total = "Total"),
                                  group_missing = "Other",
                                  tzinfo = "GMT", 
                                  ...) {
    list(
        "v0" = function(data) {
            test_begin <- .epoch2text(data$stats$begin, tzinfo)
            summary <- data$stats[, list(
                test.id = test_begin,
                sum.end = end,
                sum.duration = duration,
                sum.vu = users,
                sum.thr = `total throughput`,
                sum.thr.avg = `avg throughput`,
                sum.txn.total = `total txns`,
                sum.tps.avg = `average tps`,
                sum.failed = `total failed`,
                sum.fail.ratio = `total failed` / 
                    ifelse(`total txns`, `total txns`, 1),
                vcs.url = `vcs url`,
                vcs.rev = `vcs rev`
            )]
            result <- list(
                .ids = test_begin,
                .verdicts = setNames(
                    rep(TRUE, length(test_begin)),
                    test_begin
                ),
                Summary = list(
                    Total = list(
                        Total = summary
                    )
                )
            )
            data$transactions <- c(data$transactions, "Total")
            bgroups <- .assign_bgroups(
                data$transactions,
                names(business_groups), 
                business_groups,
                group_missing
            )
            for (txn_pos in seq(1, length(bgroups))) {
                txn <- data$transactions[txn_pos]
                bgroup <- bgroups[txn_pos]
                src.d <- data[[txn]]
                result$Transactions[[bgroup]][[txn]] <- src.d[,list(
                    test.id = .epoch2text(label, tzinfo),
                    txn.min = min,
                    txn.avg = avg,
                    txn.max = max,
                    txn.std = std,
                    txn.se  = std / sqrt(ifelse(total, total, 1)),
                    txn.median = `50 %`,
                    txn.p90 = `90 %`,
                    txn.passed = passed,
                    txn.failed = failed,
                    txn.total = total,
                    txn.errate = failed / ifelse(total, total, 1)
                )]
            }
            result
        },
        "v1" = function(data) {
            # moving categories to new locations 
            data$Elapsed <- data$Transactions
            data$Transactions <- NULL
            data$Summary$JMeter$Test <- data$Summary$Total$Total
            data$Summary$Total <- NULL
            rlist <- list(
                list("Summary", "sum."),
                list(c("Elapsed", "Latency"), "txn."),
                list("Hardware", "hw.")
            )
            for (entry in rlist) {
                categories <- entry[[1]]
                prefix <- entry[[2]]
                for (catname in categories) {
                    for (groupname in names(data[[catname]])) {
                        for (elname in names(data[[catname]][[groupname]])) {
                            tbl <- data[[catname]][[groupname]][[elname]]
                            if (!is.null(tbl)) {
                                cols.after <- sub(
                                    paste0("^", prefix), 
                                    "", 
                                    colnames(tbl)
                                )
                                setnames(tbl, cols.after)
                                tbl <- unique(tbl, by = "test.id")
                                data[[catname]][[groupname]][[elname]] <- tbl
                                data[[catname]]$test.id <- c(
                                    data[[catname]]$test.id, 
                                    tbl$test.id
                                )
                                data[[catname]][[groupname]]$test.id <- c(
                                    data[[catname]][[groupname]]$test.id, 
                                    tbl$test.id
                                )
                            }
                        }
                    }
                }
            }
            data
        }
    )
}

#' Create a function that can be used as migrator, i.e. takes current historical
#' dataset as input and upgrades it to most recent version automatically. 
#' Version of input is detected automatically.
#' 
#' @param ... arguments to create_migration_list function (migrator.R).
#' @return migrator function (takes historical data as inpit and returns 
#' upgraded version).
create_migrator <- function(...) {
    function(data) {
        l <- create_migration_list(...)
        sversions <- names(l)
        version <- data$.version
        if (is.null(version)) {
            # either v0 or v1
            version <- ifelse(
                all(c(".ids", ".verdicts") %in% names(data)), 
                "v1", 
                "v0"
            )
        }
        cdata <- data
        pos <- which(version == sversions)
        if (length(pos) > 0) {
            for (entry in names(l[seq(pos, length(l))])) {
                apt_logger(
                    "Upgrading historical data from version: ", 
                    entry
                )
                cdata <- l[[entry]](cdata)
            }
        }
        cdata
    }
}
