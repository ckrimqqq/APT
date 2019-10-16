# Heart and the most important part of libraries.
# 
# Central localtion of data analysis functions, readers and helpers.
# All of them are format agnostic, i.e. no assumptions are made about where 
# they will be used: in rmd files, user scripts or interactively, so only R 
# data types are returned by them: lists, data.tables, etc.

apt_include(c("data.table", "ggplot2"))

############### HELPER ###############

#' Convert Java timestamp (in ms) to POSIXct in UTC timezone.
#'
#' @type function
#' @signature .epoch2posixct(x, tzone = "UTC")
#' @section time utilities
#' @param x epoch time in ms
#' @param tzone time zone string
#' @return POSIXct representation of x
#' @example
#' .epoch2posixct(12345678, "Europe/Andorra")
.epoch2posixct <- function(x, tzone = "UTC") {
    # warnings should be supressed to prevent Windows timezone warnings
    suppressWarnings(as.POSIXct(x, origin = "1970-01-01", tz = tzone))
}

#' Convert Java timestamp to string format with custom timezone.
#' List of available timezones can be found at Wikipedia page:
#' http://en.wikipedia.org/wiki/List_of_tz_database_time_zones (tz column).
#'
#' @type function
#' @signature .epoch2text(epoch, tzone = "US/Eastern", format = "%Y-%m-%d %H:%M:%S %z")
#' @section time utilities
#' @param epoch epoch time in ms
#' @param tzone time zone string
#' @param format time format specification
#' @return string representation of epoch time in given time zone
#' @example
#' .epoch2text(12345678, "Europe/Armenia")
.epoch2text <- function(epoch, tzone = "US/Eastern", 
                        format = "%Y-%m-%d %H:%M:%S %z") {
    # warnings should be supressed to prevent Windows timezone warnings
    suppressWarnings(
        format(.epoch2posixct(epoch, tzone), format = format, tz = tzone)
    )
}

#' Convert number of seconds to humain-readable string 
#' (e.g. '5 hours 4 minutes 1 seconds'). Seconds are always reported.
#'
#' @type function
#' @signature .timediff2words(secdiff)
#' @section time utilities
#' @param secdiff number of seconds to convert
#' @return human-readable representation of given number
#' @example
#' .timediff2words(333)
.timediff2words <- function(secdiff) {
    seconds <- sign(secdiff) * secdiff
    dels <- c(seconds, 86400, 3600, 60)
    r <- Reduce(function(left, right) { c(left, tail(left, 1) %% right) }, dels)
    vec.n <- mapply(function(n, d) floor(n / d), r, c(tail(dels, -1), 1))
    vec.d <- c("day", "hour", "minute", "second")
    nonzero <- c(head(vec.n, -1) > 0, TRUE)
    args <- as.list(mapply(function(n, d) 
        paste(n, paste0(d, ifelse(n == 1, "", "s")), sep = " "), 
        vec.n[nonzero], vec.d[nonzero]))
    do.call(paste, c(args, sep = " "))
}

#' Find most frequent value in series (sample mode)
#'
#' @type function
#' @signature stat_mode(x)
#' @section statistical functions
#' @param x vector of discrete values (integers)
#' @return most frequent value in discrete sample
#' @example 
#' stat_mode(c(1, 1, 1, 2, 2, 3))
stat_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

#' Validate data.frame against specified schema and convert columns types.
#' 
#' @type function
#' @signature .returnAs(source_dt, columns, types)
#' @section JMeter readers
#' @param source_dt data.table instance
#' @param columns character vector of mandatory column names
#' @param types character vector of the same length as columns with character
#' representation of corresponding column types: "integer", "numeric", "logical",
#' etc. For user defined types corresponding 'as.*' and 'is.*' functions are 
#' required.
#' @return modified (inplace) source_dt
#' @example 
#' .returnAs(
#'   data.table(a = c(1, 2, 3), b = c('1', '2', '3')),
#'   c('a', 'b'),
#'   c('character', 'integer')
#' )
.returnAs <- function(source_dt, columns, types) {
    d <- source_dt
    actual_names <- colnames(d)
    missing_names <- setdiff(columns, actual_names)
    if (length(missing_names) != 0) 
        do.call(stop,c("Missing names: ", list(missing_names)))
    mapply(function(name, type) {
        .is_func <- paste0("is.", type)
        .as_func <- paste0("as.", type)
        if (!do.call(.is_func, list(x = d[[name]])) )
            d[, (name) := do.call(.as_func, list(x = d[[name]]))]
    }, columns, types)
    d
}

############### READERS/WRITERS ###############

#' Create JMeter dataset time (aka crop) filter for filter chain. 
#' 
#' @type function
#' @signature jmeter_filter_crop(deltas = c(0, 0), duration = NULL)
#' @section JMeter readers
#' @param deltas integer 2 element vector. Elements are number of seconds 
#' that should be removed from the beginning and end of the test 
#' correspondingly.
#' @param duration target test length. If after removing data according to 
#' deltas test duration is greater than duration it will be truncated (by
#' removing points from the end of the dataset) to be <= duration.
#' @return filter function (data.table -> data.table)
#' @example 
#' f <- jmeter_filter_crop(duration = 10)
#' f(
#'   data.table(
#'     timeStamp = c(1, 4, 5, 7, 11), 
#'     elapsed = runif(5, 0, 1000)
#'   )
#' )
jmeter_filter_crop <- function(deltas = c(0, 0), duration = NULL) {
    function(jm) {
        start <- min(jm$timeStamp)
        end <- max(jm$timeStamp)
        test_time <- if (length(ts) > 0) {
            c(start, end) + c(1, -1) * head(deltas, 2)
        } else {
            c(0, 0)
        }
        
        if (!is.null(duration) && test_time[2] - test_time[1] > duration)
            test_time[2] <- test_time[1] + duration
        
        jm[test_time[1] <= timeStamp & timeStamp <= test_time[2]]
    }
}

#' Create JMeter name filter for filter chain. 
#' 
#' @type function
#' @signature jmeter_filer_name(field = "label", pattern_ok = ".*")
#' @section JMeter readers
#' @param field column name for applying filter.
#' @param pattern_ok PCRE that should match valid transactions only.
#' @return filter function (data.table -> data.table)
#' @example 
#' f <- jmeter_filer_name(field = "label", pattern_ok = "^a")
#' f(
#'   data.table(
#'     timeStamp = seq(1, 5),
#'     label = c("a1", "b1", "c1", "a2", "a3")
#'   )
#' )
jmeter_filer_name <- function(field = "label", pattern_ok = ".*") {
    function(jm) {
        d <- unique(jm[, field, with = FALSE])
        d <- d[grepl(pattern_ok, d[[field]], perl = T)]
        merge(jm, d, by = field, sort = FALSE)
    }
}

#' Validate JMeter schema, conv2ert and transform columns inplace. 
#' Required JMeter columns are: "timeStamp", "elapsed", "bytes", "success", 
#' "label", "allThreads".
#' Bytes will be converted to KB and msec to sec.
#' 
#' @type function
#' @signature .process_extras(j, columns, classes = NULL, modifier = NULL)
#' @section JMeter readers
#' @param j JMeter data.table
#' @param columns character vector of additional reqired column names. 
#' @param classes character vector (the same length as columns) with textual 
#' representation of column types: "integer", "logical", etc.
#' @param modifier integer vector (the same length as columns) with divisors. 
#' Each metric in column will be divided by corresponding divisor.
#' @return modified (inplace) j
.process_extras <- function(j, columns, classes = NULL, modifier = NULL) {
    columns <- c("timeStamp", "elapsed", "bytes", 
                 "success", "label", "allThreads", columns)
    classes <- c("numeric", "integer", "integer", 
                 "logical", "character", "integer", classes)
    modifier <- c(1000, 1000, 1024, NA, NA, NA, modifier)
    
    j <- .returnAs(j, columns, classes)
    idxs <- which(!is.na(modifier))
    columns <- columns[idxs]
    modifier <- modifier[idxs]
    for (i in seq(1, length(columns))) {
        name <- columns[i]
        j[, (name) := j[[name]] / modifier[i]]
    }
    j
}

#' Rename and filter transaction names.
#' 
#' @type function
#' @signature .process_rename(j, name_field = "label", pattern_rename = "(.*)", match_group = 1)
#' @section JMeter readers
#' @param j JMeter data.table
#' @param name_field character column name where transaction names are stored.
#' @param pattern_rename PCRE rename pattern. Should contain at least 1 capture
#' group.
#' @param match_group integer that specifies capture group from pattern_rename 
#' where new name is stored.
#' @return modified (inplace) j
.process_rename <- function(j, 
                            name_field = "label",
                            pattern_rename = "(.*)",
                            match_group = 1) {
    nname <- paste0("old.", name_field)
    setnames(j, name_field, nname)
    d <- unique(j[, nname, with = FALSE])
    d[, (name_field) := sub(pattern_rename, paste0("\\", match_group), d[[nname]])]
    j <- merge(j, d, by = nname, sort = FALSE)
    j[, (nname) := NULL]
    j
}

#' Recalculate total nummber of virtual users for distributed tests.
#' 
#' @type function
#' @signature .process_distributed(j, distributed = FALSE, pattern = "(.*)", match_group = 1, machine_field = "label", machine_default = "generator-0")
#' @section JMeter readers
#' @param j JMeter data.table
#' @param distributed enable user recalculation. 
#' @param pattern PCRE machine name extraction pattern with at least 1 capture 
#' group.
#' @param match_group integer that specifies capture group from pattern 
#' where machine name is stored.
#' @param machine_field character column name that contains machine data.
#' @param machine_default character placeholder for distributed = FALSE mode
#' @return modified (inplace) j
.process_distributed <- function(j, 
                                 distributed = FALSE, 
                                 pattern = "(.*)",
                                 match_group = 1,
                                 machine_field = "label",
                                 machine_default = "generator-0") {
    d <- unique(j[, machine_field, with = FALSE])
    if (distributed) {
        d[, machine := 
              gsub(pattern, paste0("\\", match_group), d[[machine_field]])]
        
    } else {
        d[, machine := machine_default]
    }
    j <- merge(j, d, by = machine_field, sort = FALSE)
    j[, totalThreads := .fix_users_cpp(allThreads, machine)]   
    j
}

#' Assign business groups to each sample.
#' 
#' @type function
#' @signature .process_bgroups(j, groups = NULL, group_missing = "Other", field_src = "label")
#' @section JMeter readers
#' @param j JMeter data.table
#' @param groups character vector where names correspond to transaction prefixes
#' and values to their business groups.
#' @param group_missing group by default (if not matches any prefix).
#' @param field_src character column name where transaction names are stored.
#' @return modified (inplace) j
.process_bgroups <- function(j, 
                             groups = NULL, 
                             group_missing = "Other", 
                             field_src = "label") {
    d <- unique(j[, field_src, with = FALSE])
    d[, businessGroup := .assign_bgroups(
        d[[field_src]], 
        names(groups), 
        groups,
        group_missing
    )]
    merge(j, d, by = field_src, sort = FALSE)
}

#' Read JMeter results file into memory.
#'
#' @type function
#' @signature read_jmeter(filename, extras = list(), filter_chain = list(), name_filer = list(), distributed = list(), business_groups = list(), suc_names = c("Passed", "Failed"), verbose = FALSE)
#' @section JMeter readers
#' @param filename path to JMeter results file.
#' @param extras list of arguments to .process_extras.
#' @param filter_chain list of filter functions (data.table -> data.table).
#' @param name_filer list of arguments to .process_rename.
#' @param distributed list of arguments to .process_distributed.
#' @param business_groups list of arguments to .process_bgroups.
#' @param suc_names character vector with 2 elements: humain readable label for
#' passed transactions and for failed correspondingly.
#' @param verbose enable verbose CSV reader.
#' @return JMeter data.table
#' @example 
#' head(read_jmeter("test/datasets/jmeter-tiny.csv"), 5)
read_jmeter <- function(filename,
                        extras = list(),
                        filter_chain = list(),
                        name_filer = list(),
                        distributed = list(),
                        business_groups = list(),
                        suc_names = c("Passed", "Failed"), 
                        verbose = FALSE) {
    if (is.character(filename) && file.exists(filename)) {
        # suppress fread warnings
        j <- suppressWarnings(fread(
            filename, 
            sep = ",", 
            header = T, 
            stringsAsFactors = F, 
            data.table = T, 
            integer64 = "numeric",
            showProgress = verbose,
            verbose = verbose
        ))
        j <- do.call(.process_extras, c(list(j), extras))
        j <- Reduce(function(l, r) r(l), filter_chain, j)
        
        j <- do.call(.process_rename, c(list(j), name_filer))
        j <- do.call(.process_bgroups, c(list(j), business_groups))
        
        setorder(j, timeStamp)
        j <- do.call(.process_distributed, c(list(j), distributed))
        
        j[, total := "Total"]
        j[, success_hr := .rename_logical(success, suc_names[1], suc_names[2])]
        j
    }
}

#' Read results file into memory.
#'
#' @type function
#' @signature read_hw(filename, jmeter = NULL, group_extractor = NULL)
#' @section Hardware data readers
#' @param filename path to hardware stats.
#' @param jmeter JMeter data.table
#' @param group_extractor optional function for hw groups extraction. It takes
#' the following parameters: host and parsed metric name (e.g. for 
#' system.cpu.util[,idle] it will be 'system.cpu.uril', '', 'idle'). 
#' @return hardware stats data.table
#' @example
#' head(read_hw("test/datasets/hw.data"), 5)
read_hw <- function(filename, jmeter = NULL, group_extractor = NULL) {
    if (is.character(filename) && file.exists(filename)) {
        rexp <- "^(?:(?:([^,]*)(?:,([^,]*)(?:,([^,]*)(?:,([^,]*)(?:,([^,]*)(?:,([^,]*)(?:,([^,]*)(?:,([^,]*)(?:,([^,]*))?)?)?)?)?)?)?)?)?)?$"
        hw <- suppressWarnings(fread(
            filename,
            sep = ",",
            header = TRUE,
            stringsAsFactors = FALSE,
            check.names = FALSE
        ))
        setnames(hw, c("metric", "params"), c("old.metric", "old.params"))
        
        if (!is.function(group_extractor))
            group_extractor <- function(h, g, ...) g
        
        ugr <- unique(hw[, c("hostname", "old.params"), with = FALSE])
        ugrname <- sub("^(.*?)\\[.*?$", "\\1", ugr$old.params)
        ugrparams <- strsplit(
            sub("^.*?\\[(.*?)\\].*$", "\\1", ugr$old.params), 
            ",", 
            fixed = TRUE
        )
        ugr$group <- unlist(mapply(
            function(h, g, args) do.call(group_extractor, c(list(h, g), args)),
            ugr$hostname,
            ugrname,
            ugrparams
        ))
        hw <- merge(
            hw,
            ugr[!is.na(group)], 
            by = c("hostname", "old.params"), 
            sort = FALSE
        )
        
        lmetr <- unique(hw[, c("old.metric", "old.params"), with = FALSE])
        lparams <- sub("^.*?\\[(.*?)\\].*$", "\\1", lmetr$old.params)
        lmetric <- sub("\\$(\\d)", "\\\\\\1", lmetr$old.metric)
        lmetr$metric <- mapply(
            function(m, p) gsub(rexp, m, p, perl = TRUE),
            lmetric,
            lparams
        )
        hw <- merge(hw, lmetr, by = c("old.params", "old.metric"), sort = FALSE)
        
        hw[, timeStamp := clock] # zabbix clock is in sec
        hw[, old.params := NULL]
        hw[, old.metric := NULL]
        hw[, clock := NULL]
        setorder(hw, timeStamp)
        
        if (!is.null(jmeter)) {
            jmeter_time <- jmeter$timeStamp
            test_duration <- c(jmeter_time[1], jmeter_time[length(jmeter_time)])
            hw[test_duration[1] <= timeStamp & timeStamp <= test_duration[2]]
        } else {
            hw
        }
    }
}

#' Read file with SQL performance data into memory.
#'
#' @type function
#' @signature read_sqldata(path)
#' @section Hardware data readers
#' @param path path to SQL performance data stats.
#' @return Oracle data data.table
#' @example 
#' head(read_sqldata("test/datasets/db.data"), 5)
read_sqldata <- function(path) {
    if (is.character(path) && file.exists(path)) {
        sqldata <- suppressWarnings(fread(
            path,
            sep = ",",
            header = TRUE,
            stringsAsFactors = FALSE,
            check.names = FALSE,
            integer64 = "numeric",
            na.strings = "null"
        ))
        sqldata$cpu <- as.integer(round(sqldata$cpu / 1000)) / 1000
        sqldata$iowait <- as.integer(round(sqldata$iowait / 1000)) / 1000
        sqldata$elapsed <- as.integer(round(sqldata$elapsed / 1000)) / 1000
        sqldata
    }
}

#' Read file with SQL performance metadata into memory.
#' 
#' @type function
#' @signature read_sqlmeta(path, tzone = "GMT", ...)
#' @section Oracle data readers
#' @param path path to SQL performance metadata stats.
#' @return SQL metadata data.table.
#' @example 
#' head(read_sqlmeta("test/datasets/dbmeta.data"), 5)
read_sqlmeta <- function(path, tzone = "GMT", ...) {
    if (is.character(path) && file.exists(path)) {
        sqlmeta <- suppressWarnings(fread(
            path,
            sep = ",",
            header = TRUE,
            stringsAsFactors = FALSE,
            check.names = FALSE
        ))
        setnames(
            sqlmeta, 
            c("snap_beg", "snap_end", "beg", "end"),
            c("snap.first", "snap.last", "test.begin", "test.finish")
        )
        setcolorder(sqlmeta, c(3, 4, 1, 2))
        sqlmeta$test.begin <- .epoch2text(sqlmeta$test.begin, tzone)
        sqlmeta$test.finish <- .epoch2text(sqlmeta$test.finish, tzone)
        cbind(sqlmeta, ...)
    }
}

#' Write multiple data.frames to CSV file.
#'
#' @type function
#' @signature dump_2_csv(file, datas, append = FALSE)
#' @section Table writers
#' @param file path to results CSV file.
#' @param datas list of data.frames to write.
#' @param append do not remove file if it exists.
dump_2_csv <- function(file, datas, append = FALSE) { 
    cols <- !append
    for (d in datas) {
        write.table(d, file = file, sep = ",", append = append, 
                    col.names = cols, row.names = FALSE)
        append <- TRUE
        cols <- FALSE
    }
}

############ HISTORICAL DATA #############

#' Load variable from file.
#'
#' @param x character vector of the variable names that were saved. Function 
#' tries to get value from each of the specified variable names. It stops once
#' it finds defined.
#' @param file file where variables were saved.
#' @return variable value.
.loadx <- function(x, file) {
    senv <- new.env()
    load(file, envir = senv)
    for (xs in x) {
        if (exists(xs, where = senv)) 
            return(get(xs, envir = senv))
    }
}

#' Save variable to file.
#'
#' @param object object that will be saved.
#' @param varname character name of variable that should point to object.
#' @param filename character path to file.
.savex <- function(object, varname, filename) {
    assign(varname, object)
    save(list = varname, file = filename)
}

#' A Reference Class to represent historical data object.
#'
#' @type class
#' @section historical API
#' @signature HistoryData
#' @field data actual data stored in file.
#' @field id test id, meaningful only if object is used not in read only mode.
#' @field writes flag that indicates if any changes were made to data.
#' @field filename name of associated historical data file.
#' @field active_tests character vector of active test ids, meaningful in trend context.
#' @field last_test character id of last test == tail(active_tests, 1).
#' @field version version of current historical data file.
setRefClass(
    Class = "HistoryData",
    fields = list(
        data = "list",
        id = "character",
        writes = "logical",
        filename = "character",
        active_tests = "character",
        last_test = "character",
        version = "character"
    ),
    methods = list(
        #' Read history object from file.
        #'
        #' @type method
        #' @signature initialize(pfilename = "", tzone = "GMT", spec = NULL, pid = NULL, migrator = NULL)
        #' @param pfilename character path to historical data file.
        #' @param tzone tzinfo timezone specification.
        #' @param pid test id.
        #' @param spec if integer active_tests is set to spec latest test ids, otherwise.
        #' @param migrator function that migrates historical data to most recent version.
        #' interprets spec as vector of test ids to show.
        #' @example
        #' new("HistoryData")
        initialize = function(pfilename = "", 
                              tzone = "GMT", 
                              spec = NULL,
                              pid = NULL,
                              migrator = NULL) {
            version <<- "v2"
            filename <<- if (is.character(pfilename)) pfilename else ""
            if (is.character(filename) && file.exists(filename)) {
                data <<- .loadx(c("history_obj", "acc.stats"), filename)
                if (is.function(migrator)) {
                    data <<- migrator(data)
                }
                active_tests <<- if (is.numeric(spec)) {
                    tail(data$.ids, spec)
                } else {
                    if (is.null(spec)) character(0) else spec
                }
                last_test <<- tail(active_tests, 1)
            }
            id <<- if (is.null(pid) || (pid %in% data$.ids)) {
                .epoch2text(as.integer(Sys.time()), tzone = tzone)
            } else {
                pid
            }
            writes <<- FALSE
        },
        #' Check if some path within data contains any active tests.
        #' 
        #' - path character vector of path within data, i.e. c("Cat") or
        #' c("Cat", "group") or even c("Cat", "group", "element")
        #' - obj used by function itself in recursive mode. Should be NULL if
        #' invoked by user.
        is_active = function(path = NULL, obj = NULL) {
            if (is.null(obj))
                obj <- data
            if (length(path) == 0) {
                TRUE
            } else if (length(path) == 1) {
                any(active_tests %in% obj[[path]]$test.id)
            } else {
                is_active(path = tail(path, -1), obj = obj[[head(path, 1)]])
            }
        },
        #' Get all known ids of given path. 
        #' 
        #' For categories and groups ids are union of all test id that have 
        #' written smth under given path. For elements it's just a test.id 
        #' column.
        #' 
        #' - path character vector of path within data, i.e. c("Cat") or
        #' c("Cat", "group") or even c("Cat", "group", "element")
        #' - obj used by function itself in recursive mode. Should be NULL if
        #' invoked by user.
        get_ids = function(path = NULL, obj = NULL) {
            if (is.null(obj))
                obj <- data
            if (length(path) == 0) {
                NULL
            } else if (length(path) == 1) {
                obj[[path]]$test.id
            } else {
                get_ids(path = tail(path, -1), obj = obj[[head(path, 1)]])
            }
        },
        #' Get available names (excluding special names) of given path.
        #' 
        #' - category if NULL top-level data names will be returned (exluding
        #' internal structures).
        #' - group if category is NULL prints name of top-level data, otherwise
        #' if NULL prints category names else group names.
        get_names = function(category = NULL, group = NULL) {
            if (is.null(category)) {
                names(data)[!grepl("^[.]", names(data))]
            } else {
                path <- category
                obj <- data[[category]]
                if (!is.null(group)) {
                    obj <- obj[[group]]
                    path <- c(path, group)
                }
                Filter(
                    function(fr) is_active(c(path, fr)),
                    setdiff(names(obj), "test.id")
                )
            }
        },
        #' Get table from historical data with respect to 
        #' set of active test ids.
        #'
        #' - category character name of data category.
        #' - group character name of data group.
        #' - element character name of data unit.
        #' - active if TRUE returns table with active_tests only otherwise 
        #' all.
        get_table = function(category, group, element, active = TRUE) {
            res <- data[[category]][[group]][[element]]
            if (active && !is.null(res) && is.data.table(res)) {
                res[res$test.id %in% active_tests]
            } else {
                res
            }
        },
        #' Get verdicts for active tests as vector
        #' 
        #' - tests vector of specific test ids
        get_verdicts = function(tests = NULL) {
            if (!is.null(tests)) {
                data$.verdicts[tests]
            } else {
                data$.verdicts
            }
        },
        #' Get table with active tests verdicts.
        #' 
        #' - logical_names logical values (as strings) to
        #' humain readable names mapping.
        get_verdicts_table = function(logical_names = NULL) {
            res <- data.table(
                test.id = active_tests, 
                status = get_verdicts(active_tests)
            )
            if (!is.null(logical_names)) {
                res[, status := logical_names[paste0("", status)]]
            }
            res
        },
        #' Adds current test id in given category and group lookup tables.
        #' 
        #' - category character name of category.
        #' - group character name of group.
        set_ids_path = function(category, group) {
            data[[category]]$test.id <<- 
                union(data[[category]]$test.id, id)
            data[[category]][[group]]$test.id <<- 
                union(data[[category]][[group]]$test.id, id)
        },
        #' Add row to historical data.
        #'
        #' - category character name of data category.
        #' - group character name of data group.
        #' - name character name of data alias.
        #' - what data.table with 1 row.
        add_row = function(category, group, name, what) {
            if (!"test.id" %in% colnames(what)) {
                what[, test.id := id]
                tot <- length(colnames(what))
                setcolorder(what, c(tot, seq(1, tot - 1)))
            }
            if (is.null(data[[category]][[group]])) {
                data[[category]][[group]][[name]] <<- what
            } else {
                data[[category]][[group]][[name]] <<- 
                    rbindlist(
                        list(data[[category]][[group]][[name]], what),
                        fill = TRUE
                    )
            }
            set_ids_path(category, group)
            writes <<- TRUE
        },
        #' Add table to historical data.
        #' 
        #' - d table to insert.
        #' - category character name of data category.
        #' - group_field either column name where group data is stored in table or
        #' character name of group of each row.
        #' - name_field either column name where alias data is stored in table or
        #' character name of alias of each row.
        #' - exclude_cols character vector of column names that should be removed
        #' from inserted data.table.
        add_table = function(d, 
                             category = "assets", 
                             group_field = "all", 
                             name_field = "label", 
                             exclude_cols = NULL) {
            gx <- group_field %in% colnames(d)
            nx <- name_field %in% colnames(d)
            for (i in seq(1, nrow(d))) {
                dv <- d[i]
                cols <- setdiff(
                    colnames(dv), 
                    c(group_field, name_field, exclude_cols)
                )
                gr <- ifelse(gx, dv[[group_field]], group_field)
                nm <- ifelse(nx, dv[[name_field]], name_field)
                add_row(
                    category = category,
                    group =  gr,
                    name = nm,
                    what = dv[, cols, with = FALSE]
                )
            }
        },
        #' Write historical data to file.
        #' 
        #' - verdict current test SLA verdict.
        dump = function(verdict = TRUE) {
            if (writes && is.character(filename)) {
                d <- data
                d$.version <- version 
                d$.ids <- c(d$.ids, id)
                d$.verdicts[id] <- verdict
                .savex(d, "history_obj", filename)
            }
        },
        #' Remove tests from historical data by id.
        #'
        #' - ids character vector of test ids to remove.
        remove = function(h, ids) {
            ns <- names(data)
            ns <- ns[!grepl("^[.]", ns, perl = TRUE)]
            for (categ in ns) {
                for (group in names(data[[categ]])) {
                    for (metric in names(data[[categ]][[group]])) {
                        dt <- data[[categ]][[group]][[metric]]
                        data[[categ]][[group]][[metric]] <<- 
                            dt[!dt$test.id %in% ids]
                    }
                }
            }
            writes <<- TRUE
        },
        #' Find relative metric excess for a given test (relative increase comparing
        #' previous available test) in single category. 
        #'
        #' - test_id test id of base test (if NULL last_test is used).
        #' - field metric for which excess is calculated.
        #' - ignore_names character vector of aliases that will be ignored during
        #' lookup.
        #' - category category for lookup.
        #' returns degradation data.table
        degradation = function(field,
                               test_id = NULL,
                               ignore_names = NULL,
                               category = "Elapsed") {
            if (is.null(test_id))
                test_id <- last_test
            cgroups <- character(0)
            cnames <- character(0)
            ccurr <- numeric(0)
            cprev <- numeric(0)
            cdiff <- numeric(0)
            cdegr <- numeric(0)
            cbase <- character(0)
            for (bgroup_name in get_names(category)) {
                for (metr_name in get_names(category, bgroup_name)) {
                    if (!metr_name %in% ignore_names) {
                        metr <- data[[category]][[bgroup_name]][[metr_name]]
                        rn <- tail(which(metr$test.id == test_id), 1)
                        curr_val <- ifelse(
                            length(rn) > 0,
                            metr[rn][[field]], 
                            NA_real_
                        )
                        prev_val <- NA_real_
                        prev_test <- NA_character_
                        if (length(rn) > 0 && rn > 1) {
                            # take nearest test which metric is not NA
                            metr_before <- metr[seq(1, rn - 1)]
                            prn <- tail(which(!is.na(metr_before[[field]])), 1)
                            if (length(prn) > 0) {
                                prev_test <- metr[prn]$test.id
                                prev_val <- metr[prn][[field]]
                            }
                        }
                        delta <- curr_val - prev_val
                        cgroups <- c(cgroups, bgroup_name)
                        cnames <- c(cnames, metr_name)
                        ccurr <- c(ccurr, curr_val)
                        cprev <- c(cprev, prev_val)
                        cdiff <- c(cdiff, delta)
                        cdegr <- c(cdegr, delta / ifelse(prev_val, prev_val, 1))
                        cbase <- c(cbase, prev_test)
                    }
                }
            }
            data.table(
                group = cgroups,
                name = cnames,
                curr = ccurr,
                prev = cprev,
                diff = cdiff,
                value = cdegr,
                prev = cbase
            )[order(-value)]
        }
    )
)

############### EXTRACTORS ###############

#' Find top N groups in data.table. 
#'
#' @param dt target data.table.
#' @param flist list k = v, where k is a name of metric and v is a function to 
#' apply to group.
#' @param field column name for metric of interest.
#' @param group column name for grouping.
#' @param n integer specifying the number of returned groups.
#' @param asc use ascending sort order.
#' @param field column to return from each group.
#' @return list where names are names(flist) and values are corresponding 
#' top N vectors.
top <- function(dt, flist, field, group, n = 5, 
                asc = FALSE, return_field = "label") {
    direction <- ifelse(asc, 1, -1)
    agg_f <- function(d) {
        setNames(lapply(flist, function(.f) .f(d[[field]])), names(flist))
    }
    dta <- dt[, agg_f(.SD), by = group]
    fields <- names(flist)
    setNames(lapply(fields, function(field) {
        ret <- dta[order(direction * dta[[field]]), return_field, with = FALSE]
        head(ret[[1]], n)
    }), fields)
}

############### SLA FUNCTIONS ###############

#' Define service level agreement (SLA) for specific column.
#' 
#' @param sla_column column name for SLA.
#' @param values either a single value or list where names are business groups.
#' This allows to define SLA for a specific business groups. Type of values
#' depends on binary op function.
#' @param op binary function that takes each metric and corresponding value from
#' values (if values is a list then value is choosen to match the business group
#' of the row) and produces logical output specifying if SLA is met.
#' @param critical boolean flag that indicates that any SLA validation failure
#' results in test failure.
#' @return SLA list.
define_sla <- function(sla_column, values, op = "<", critical = FALSE) {
    f <- function(data, business_groups) {
        col <- data[[sla_column]]
        scol <- length(col)
        comp <- if (is.list(values)) {
            if (!is.null(business_groups)) {
                vg <- values[business_groups]
                vg[sapply(vg, is.null)] <- NA
                unlist(vg)
            } else {
                rep(NA, scol)
            }
        } else values
        do.call(op, list(col, comp))
    }
    list(
        column = sla_column,
        operation = f,
        critical = critical
    )
}

#' Reference class for SLA results storage.
#'
#' @field verdict boolean flag that indicates if the current test is passed 
#' or failed.
#' @field results actual results storage.
#' @field slas list with SLA definitions.
setRefClass(
    Class = "SLAResults",
    fields = list(
        verdict = "logical",
        results = "list",
        slas = "list"
    ),
    methods = list(
        #' Validate SLA on specific table.
        #' 
        #' - tbl table to check SLAs.
        #' - tbl_id character name of SLA results matrix id. Results will be 
        #' saved to sla_obj[[tbl_id]].
        #' - class_id character name of SLA location within SLA list.
        #' - business_groups either NULL or character vector (with length equals to 
        #' nrow(tbl)) with group information per source table row.
        check = function(tbl, tbl_id, class_id = tbl_id, business_groups = NULL) {
            curr_slas <- slas[[class_id]]
            rcols <- colnames(tbl)
            entries <- nrow(tbl)
            ns <- setNames(
                lapply(rcols, function(x) rep(NA, entries)), 
                colnames(tbl)
            )
            sla_tbl <- do.call(cbind, ns)
            for (sla in curr_slas) {
                if (sla$column %in% colnames(sla_tbl)) {
                    ncol <- sla$operation(tbl, business_groups)
                    if (sla$critical)
                        verdict <<- verdict && all(ncol[!is.na(ncol)])
                    sla_tbl[, sla$column] <- ncol
                }
            }
            results[[tbl_id]] <<- sla_tbl
        },
        result_for = function(tbl_id) {
            results[[tbl_id]]
        },
        #' Produce SLA results summary table.
        #' 
        #' - ids character vector of test ids.
        #' - verdicts logical vector of test verdicts with the same length as ids.
        #' - descr nested list with the following structure:
        #' list(
        #'   sla_table_id_1 = list(
        #'     humain_readable_col_name_1 = real_col_name_1,
        #'     ...
        #'   ),
        #'   ...
        #' ). It specifies which SLA results and which columns in them have to be 
        #' included into summary.
        #' 
        #' return SLA summary data.table.
        summary = function(ids, 
                           verdicts = rep(TRUE, length(ids)), 
                           descr = NULL) {
            d <- data.table(test.id = ids, test = verdicts)
            for (table.id in names(descr)) {
                tnames <- names(descr[[table.id]])
                tcols <- descr[[table.id]]
                for (i in seq(1, length(tcols))) {
                    rd <- results[[table.id]][, tcols[i]]
                    if (!is.null(rd) && !all(is.na(rd))) {
                        d[, (tnames[i]) := rd]
                        verdicts <- ifelse(is.na(rd), TRUE, rd) & verdicts
                    }
                }
            }
            d[, total := verdicts]
            d
        }
    )
)

############### ANALYSIS ###############

#' Create test description table.
#'
#' @param jm JMeter data.table.
#' @param tzone test timezone for time stamps.
#' @param ... list of additional columns for table (name = colname, 
#' value = value).
#' @return test description data.table
summary_general <- function(jm, tzone, ...) {
    start.raw <- jm$timeStamp[1]
    end.raw <- jm$timeStamp[length(jm$timeStamp)]
    start.str <- .epoch2text(start.raw, tzone)
    end.str <- .epoch2text(end.raw, tzone)
    duration.sec <- end.raw - start.raw
    duration.sec <- ifelse(duration.sec == 0, 1, duration.sec)
    duration <- .timediff2words(duration.sec)
    total <- nrow(jm)
    failed <- 0
    
    maxusers <- max(jm$totalThreads)
    throughput <- sum(jm$bytes)
    
    failed <- nrow(jm[success == FALSE])
    failed.ratio <- ifelse(total == 0, 0, failed / total)
    
    rsrc <- list(begin = start.str,
                 end = end.str,
                 duration = duration,
                 vu = maxusers,
                 thr = throughput,
                 thr.avg = throughput / duration.sec,
                 txn.total = total,
                 tps.avg = total / duration.sec,
                 failed = failed,
                 fail.ratio = failed.ratio)
    do.call(data.table, c(rsrc, list(...)))
}

#' Calculate generic summary for grouped data.
#' 
#' @param dt source data.table
#' @param action callback function that calculates summary for single group.
#' Single param to it is a list with following content:
#'  * name   - name of group
#'  * groups - business groups for group
#'  * total  - number of samples
#'  * passed - number of passed samples (== total if success_field is NULL)
#'  * values - numeric vector of values
#' It must return a list (data.table row): column names and corresponding values. 
#' @param group_key either character vector of column names for grouping or NULL
#' if no grouping is required.
#' @param stat_col character name of column with values for analysis.
#' @param bgroup_col character name of column with business group information.
#' @param success_field character name of column with success flag (T/F).
#' @param success_only remove failed samples from analysis.
#' @param total_name name of Total transaction.
#' @return generic summary data.table.
.generic_summary <- function(dt, 
                             action,
                             group_key = "label",
                             stat_col = "elapsed",
                             bgroup_col = "businessGroup",
                             success_field = "success", 
                             success_only = FALSE, 
                             total_name = "Total") {
    no_grouping <- is.null(group_key)
    appl <- function(name, sdf) {
        series <- if (is.null(success_field) || !success_only) {
            sdf[[stat_col]]
        } else {
            sdf[sdf[[success_field]]][[stat_col]]
        }
        plen <- if (is.null(success_field)) {
            nrow(sdf) # all passed
        } else {
            sum(sdf[[success_field]])
        }
        xname <- do.call(paste, c(name, list(sep = ", ")))
        groups <- if (is.null(bgroup_col) || 
                      bgroup_col %in% names(name) || 
                      no_grouping) {
            xname
        } else {
            groups <- sort(unique(sdf[[bgroup_col]]))
            ifelse(length(groups) == 1, groups,
                   sprintf("%s, ...", head(groups, 1)))
        }
        r <- action(list(
            name = xname,
            groups = groups,
            total = nrow(sdf), # all in subgroup, not just passed
            passed = plen,
            values = series
        ))
    }
    if (no_grouping) {
        dt[, appl(total_name, .SD)]
    } else {
        res <- dt[, appl(.BY, .SD), by = group_key]
        res[, which(!colnames(res) %in% group_key), with = FALSE]
    }
}

#' Create distribution summary table.
#' 
#' It calculates the following metrics: min, max, avg, sd, se, median, 90% line 
#' and provides passed/failed/total counts. 
#' 
#' @param dt source data.table.
#' @param ... arguments to .generic_summary.
#' @return distribution summary data.table.
distribution_summary <- function(dt, ...) {
    distr_func <- function(percs) {
        function(args) {
            series <- args$values
            tlen <- args$total
            plen <- args$passed
            slen <- length(series)
            fld <- tlen - plen
            list(
                name = args$name,
                group = args$groups,
                min = ifelse(slen, min(series), NA_real_),
                avg = ifelse(slen, mean(series), NA_real_),
                max = ifelse(slen, max(series), NA_real_),
                std = sd(series),
                se = ifelse(slen, sd(series) / sqrt(slen), NA_real_),
                median = median(series),
                p90 = quantile(series, 0.9),
                passed = plen,
                failed = fld,
                total = tlen,
                errate = ifelse(tlen, fld / tlen, 0)
            )
        }
    }
    .generic_summary(dt, action = distr_func(percentiles), ...)
}

#' Create distribution table.
#' 
#' Distribution table contains values of ecdf function at specified points. In
#' simple words it estimates the fraction of samples less than or equals to
#' the given points.
#' 
#' @param dt source data.table.
#' @param points points of interest.
#' @param ... arguments to .generic_summary.
#' @return distribution data.table.
distribution_table <- function(dt, points = c(0.5, seq(1, 10, 1)), ...) {
    distr_func <- function(pts) {
        function(args) {
            cecdf <- if (length(args$values) > 0) ecdf(args$values) 
            else function(x) NA_real_
            
            nms <- paste0("t", points)
            c(name = args$name,
              group = args$groups, 
              setNames(lapply(pts, cecdf), nms))
        }
    }
    .generic_summary(dt, action = distr_func(points), ...)
}

#' Create frequency summary table.
#' 
#' Frequency table contains number of samples within groups and their relative
#' frequency.
#' 
#' @param dt source data.table.
#' @param ... arguments to .generic_summary.
#' @return frequency data.table.
frequency_summary <- function(dt,  ...) {
    tlen <- ifelse(nrow(dt), nrow(dt), 1)
    action <- function(args) {
        list(
            name = args$name,
            group = args$group,
            count = length(args$values)
        )
    }
    res <- .generic_summary(dt, action = action, ...)
    res[, ratio := count / tlen]
    res
}

#' Percentiles table.
#' 
#' Percentiles table contains specified sample percentiles. n % percentile is 
#' an element of sample, so that n % of sample values are less than it.
#' 
#' @param dt source data.table.
#' @param percentiles percentiles of interest.
#' @param ... arguments to .generic_summary.
#' @return percentiles data.table.
percentiles_summary <- function(dt, percentiles=c(0.5, 0.75, 0.9, 0.95, 0.99),
                                ...) {
    distr_func <- function(percs) {
        function(args) {
            d <- quantile(args$values, percs)
            c(list(name = args$name, group = args$groups),
              setNames(d, paste0("p", percs)))
        }
    }
    .generic_summary(dt, action = distr_func(percentiles), ...)
}

#' Trend summary table.
#' 
#' Trend summary table contains absolute and relative differences of any given
#' metric in dt between current/previous test and current/n-moving mean.
#' 
#' @param dt source data.table.
#' @param id_field column name of test ids.
#' @param field column name of target metric.
#' @param verdicts logical vector with length == nrow(dt) with test SLA verdicts.
#' @param n_mm N parameter for n-moving mean algorithm.
#' @return trend summary data.table.
trend_summary <- function(dt,
                          id_field, 
                          field,
                          verdicts = rep(TRUE, nrow(dt)),
                          verdicts_names = NULL,
                          n_mm = 20) {
    r <- dt[, c(id_field, field), with = FALSE]
    setnames(r, c(id_field, field), c("test.id", "val"))
    if (is.null(verdicts_names)) {
        r[, verdict := verdicts]
    } else {
        r[, verdict := verdicts_names[as.character(verdicts)]]
    }
    setcolorder(r, c(1, 3, 2))
    for (cr in list(list("prev", 1), list("mm", n_mm))) {
        cr_v <- paste0("val.", cr[[1]])
        cr_abs <- paste0("abs.", cr[[1]])
        cr_rel <- paste0("rel.", cr[[1]])
        r[, (cr_v) := .mov_mean(r$val, verdicts, n = cr[[2]])]
        r[, (cr_abs) := r$val - r[[cr_v]]]
        r[, (cr_rel) := r[[cr_abs]] / ifelse(r[[cr_v]], r[[cr_v]], 1)]
    }
    r
}

############### PLOT ###############


#' Initialize empty plot.
#' 
#' @param data_df data data.frame or NULL.
#' @param data_aes ggplot2 aesthetics object.
#' @param gtitle plot title.
#' @param gdelta bin width for histogram/barcharts (displayed in title).
#' @param gxlab X-axis label.
#' @param gylab Y-axis label.
#' @param gpalette NULL or palette name from colorbrewer.org
#' @param show_y_zero view y = 0 on Y-axis.
#' @param ... ignored.
#' @return ggplot2 plot
.init_plot <- function(data_df = NULL, 
                       data_aes = NULL,
                       gtitle = "Chart",
                       gdelta = NULL,
                       gxlab = "X",
                       gylab = "Y",
                       gpalette = NULL,
                       show_y_zero = TRUE,
                       legend_position = "bottom",
                       legend_cols = 3,
                       ...) {
    p <- if (!is.null(data_df)) {
        if (!is.null(data_aes))
            ggplot(data = data_df, data_aes)
        else
            ggplot(data = data_df)
    } else ggplot()
    
    if (!is.null(gdelta)) {
        gtitle <- bquote(.(gtitle) (delta == .(sprintf("%.3f", gdelta))))
    }
    
    p <- p +
        xlab(label = gxlab) +
        ylab(label = gylab) +
        ggtitle(gtitle) +
        theme(
            plot.title = element_text(hjust = 0.5),
            legend.title = element_blank(), 
            legend.position = legend_position, 
            legend.spacing = unit(2, "mm")
        ) +
        guides(col = guide_legend(ncol = legend_cols))
    
    p <- if (!is.null(gpalette)) {
        p + scale_color_brewer(palette = gpalette) + 
            scale_fill_brewer(palette = gpalette) 
    } else p
    
    if (show_y_zero) {
        p + expand_limits(y = 0)
    } else p
}

#' Create and configure lines for ggplot2 plot. 
#' 
#' @param geom_args arguments to geom_line.
#' @param line_size size of line.
#' @param ... ignored.
#' @return geom_line instance
.line <- function(geom_args, line_size = 1, ...)
    do.call(geom_line, c(geom_args, list(size = line_size)))

#' Create and configure points for ggplot2 plot. 
#' 
#' @param geom_args arguments to geom_point.
#' @param point_size size of point.
#' @param ... ignored.
#' @return geom_point instance
.point <- function(geom_args, point_size = 3, ...)
    do.call(geom_point, c(geom_args, list(size = point_size)))

#' Create and configure bars for ggplot2 plot.
#' 
#' @param geom_args arguments to geom_bar.
#' @param ... ignored.
#' @return geom_bar instance.
.bar <- function(geom_args, ...)
    do.call(geom_bar, 
            c(geom_args, list(
                stat = "identity", 
                colour = "black", 
                position = "dodge")
            ))

#' Create and configure boxplot for ggplot2 plot.
#' 
#' @param geom_args arguments to geom_boxplot.
#' @param ... ignored.
#' @return geom_boxplot instance.
.box <- function(geom_args, ...)
    do.call(geom_boxplot, c(geom_args, stat = "identity"))

#' Helper function for data key columns reduction and renaming.
#' 
#' @param df target data.frame
#' @param key character vector of column names that represents table key.
#' @return modified df
.reduce_data <- function(df, key) {
    if (length(key) > 1) {
        key_old <- key
        key <- do.call(paste, c(key, list(sep = '.')))
        df[, (key) := do.call(paste, df[, key_old, with = FALSE])]
    }
    key
}

#' Create normalized replacement function.
#' 
#' @param func original function.
#' @param norm_c normalization constant.
#' @return normalized replacement function.
.normalized_metric <- function(func, norm_c)
    function(x) func(x) / norm_c

#' Granularize time for time series data (in place).
#' 
#' @param dt data.table with mandatory timeStamp column. 
#' @param timezone character tz name for times parsing.
#' @param gran granularity interval or NULL.
#' @param bins if gran is NULL divide test into N bins uniformly.
.pre_timechart <- function(dt, timezone, gran = NULL, bins = 50) {
    if (is.null(gran))
        gran <- (dt$timeStamp[nrow(dt)] - dt$timeStamp[1]) / bins
    dt[, timeStamp := .epoch2posixct(timeStamp - timeStamp %% gran, timezone)]
}

#' Create linechart.
#' 
#' @param result source data.frame.
#' @param xaxis column name of X-axis values (considered as factors).
#' @param series column names of metrics to be printed.
#' @param series_names list with mapping between series and names to be printed 
#' in legend.
#' @param chartops arguments to geom primitives functions.
#' @param init_plot base ggplot2 object (draw above it).
#' @param use_points print points.
#' @param rotate_text rotate X-axis labels for 45 degrees.
#' @param ... ignored.
#' @return ggplot2 chart.
chart_linechart <- function(result,
                            xaxis,
                            series,
                            series_names = NULL,
                            chartops = list(),
                            init_plot = NULL,
                            use_points = FALSE,
                            rotate_text = TRUE,
                            ...) {
    r <- melt(result, xaxis, series)
    
    if (!is.null(series_names))
        r[, variable := unlist(series_names[as.character(r$variable)])]
    
    r[[xaxis]] <- as.factor(r[[xaxis]])
    
    p <- if (is.null(init_plot)) {
        do.call(.init_plot, chartops)
    } else {
        init_plot
    }
    
    geom_args <- list(
        data = r,
        aes_string(
            x = xaxis, 
            y = "value", 
            colour = "variable",
            group = "variable"
        )
    )
    
    geoms <- list(
        do.call(.line, c(list(geom_args), chartops)),
        do.call(.point, c(list(geom_args), chartops))
    )
    geoms_p <- c(TRUE, use_points)
    
    Reduce('+', geoms[geoms_p], p) + 
        theme(
            axis.text.x = element_text(
                angle = ifelse(rotate_text, 45, 0), 
                hjust = 1
            )
        )
}

#' Create timeseries chart for single metric.
#' 
#' @param result source data.frame.
#' @param key column name of group key data (use 'total' to group all).
#' @param timezone timezone.
#' @param yaxis column name of metric to print.
#' @param chartops arguments to geom primitives functions.
#' @param init_plot base ggplot2 object (draw above it).
#' @param granularity chart granularity.
#' @param averaging_foo granularity replacement function.
#' @param use_points print points.
#' @param normalize_metric normalize metric (i.e. KB/s).
#' @param ... ignored.
#' @return ggplot2 chart.
chart_timeseries <- function(result,
                             key = "total",
                             timezone = "GMT",
                             yaxis = "elapsed",
                             chartops = list(),
                             init_plot = NULL,
                             granularity = 1,
                             averaging_foo = mean,
                             use_points = FALSE,
                             normalize_metric = FALSE,
                             ...) {
    r <- result[, c("timeStamp", yaxis, key), with = FALSE]
    t_values <- r$timeStamp[nrow(r)] - r$timeStamp[1]
    if (t_values > 10) {
        .pre_timechart(r, timezone, gran = granularity)
        afoo <- if (normalize_metric) {
            .normalized_metric(averaging_foo, granularity)
        } else {
            averaging_foo
        }
        r <- r[, afoo(.SD[[yaxis]]), by = c(key, "timeStamp")]
        
        key <- .reduce_data(r, key)
        
        p <- if (is.null(init_plot)) {
            do.call(.init_plot, chartops)
        } else {
            init_plot
        }
        
        geom_args <- list(
            data = r,
            aes_string(x = "timeStamp", y = "V1", colour = key)
        )
        
        geoms <- list(
            do.call(.line, c(list(geom_args), chartops)),
            do.call(.point, c(list(geom_args), chartops))
        )
        geoms_p <- c(TRUE, use_points)
        
        Reduce('+', geoms[geoms_p], p)
    }
}

#' Create barchart for single metric.
#' 
#' @param result source data.frame.
#' @param key column name of group key data (use 'total' to group all).
#' @param metric column name of metric to print.
#' @param chartops arguments to geom primitives functions.
#' @param init_plot base ggplot2 object (draw above it).
#' @param x_bins number of bins for barchart.
#' @param cumulative TRUE enables cumulative mode.
#' @param ... ignored.
#' @return ggplot2 chart.
.chart_bar <- function(result,
                       key,
                       metric = "elapsed",
                       chartops = list(),
                       init_plot = NULL,
                       x_bins = 50,
                       cumulative = FALSE,
                       ...) {
    r <- result[, c(key, metric), with = FALSE]
    key <- .reduce_data(r, key)
    
    setnames(r, metric, "metric")
    x_rng <- range(r$metric, finite = TRUE)
    x_length <- x_rng[2] - x_rng[1]
    bin_width <- ifelse(x_length, x_length / x_bins, .5)
    x_seq <- seq(x_rng[1], x_rng[2], by = bin_width)
    afunc <- function(metric) {
        len <- ifelse(length(metric), length(metric), 1)
        counts <- .grouped_count(metric, x_seq)
        list(
            metric = x_seq, 
            value = if (cumulative) cumsum(counts) / len else counts
        )
    }
    geom_args <- list(
        data = r[, afunc(metric), by = key], 
        aes_string(x = "metric", y = "value", fill = key)
    )
    
    p <- if (is.null(init_plot)) {
        do.call(.init_plot, c(chartops, gdelta = bin_width))
    } else {
        init_plot
    }
    
    p + do.call(.bar, c(list(geom_args), chartops))
}

#' Create histogram for single metric.
#' 
#' See .chart_bar docs for parameters description.
#' @return ggplot2 chart.
chart_histogram <- function(...) .chart_bar(..., cumulative = FALSE)

#' Create ecdf-plot for single metric.
#' 
#' See .chart_bar docs for parameters description.
#' @return ggplot2 chart.
chart_ecdfplot <- function(...) .chart_bar(..., cumulative = TRUE)

#' Create boxplot for single metric.
#' 
#' @param result source data.frame.
#' @param key column name of group key data (use 'total' to group all).
#' @param metric column name of metric to print.
#' @param chartops arguments to geom primitives functions.
#' @param init_plot base ggplot2 object (draw above it).
#' @param flip rotate chart by 90 degrees to the right.
#' @param outliers remove outliers from data (see boxplot.stats for details).
#' @param ... ignored.
#' @return ggplot2 chart.
chart_boxplot <- function(result,
                          key,
                          metric = "elapsed",
                          chartops = list(),
                          init_plot = NULL,
                          flip = FALSE,
                          outliers = TRUE,
                          colorful = TRUE,
                          ...) {
    r <- result[, c(key, metric), with = FALSE]
    key <- .reduce_data(r, key)
    
    p <- if (is.null(init_plot)) {
        do.call(.init_plot, chartops)
    } else {
        init_plot
    }
    
    aggr <- function(s) {
        stats <- boxplot.stats(s)$stats
        d <- if (outliers) {
            c(min(s), stats[c(2,3,4)], max(s))
        } else {
            stats
        }
        as.list(d)
    }
    r.stat <- r[, aggr(.SD[[metric]]), by = key]
    setnames(r.stat, c("x", "y0", "y25", "y50", "y75", "y100"))
    
    geom_args <- list(
        data = r.stat, 
        if (colorful) {
            aes(
                x = x,
                ymin = y0,
                lower = y25,
                middle = y50,
                upper = y75,
                ymax = y100,
                fill = x
            )
        } else {
            aes(
                x = x,
                ymin = y0,
                lower = y25,
                middle = y50,
                upper = y75,
                ymax = y100,
                group = x
            )
        }
    )
    
    g <- p + do.call(.box, c(list(geom_args), chartops))
    if (flip) g + coord_flip() else g
}

#' Create boxplots over time plot for single metric.
#' 
#' @param result source data.frame.
#' @param metric column name of metric to print.
#' @param timezone timezone.
#' @param chartops arguments to geom primitives functions.
#' @param init_plot base ggplot2 object (draw above it).
#' @param outliers remove outliers from data (see boxplot.stats for details).
#' @param x_bins number of intervals to quantize x-Axis (time axis).
#' @param ... ignored.
#' @return ggplot2 chart.
chart_util <- function(result,
                       metric = "elapsed",
                       timezone = "GMT",
                       chartops = list(),
                       init_plot = NULL,
                       outliers = TRUE,
                       x_bins = 50,
                       colorful = FALSE,
                       ...) {
    t_values <- result$timeStamp[length(result$timeStamp)] - result$timeStamp[1]
    if (t_values > 10) {
        r <- result[, c("timeStamp", metric), with = FALSE]
        .pre_timechart(r, timezone, bins = x_bins)
        chart_boxplot(
            r,
            key = "timeStamp", 
            metric = metric,
            chartops = chartops,
            init_plot = init_plot,
            flip = FALSE,
            outliers = outliers,
            colorful = colorful,
            ...
        )
    }
}
