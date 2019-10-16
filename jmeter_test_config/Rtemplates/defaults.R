#' enable or disable verbose mode
param_verbose <- TRUE

#' list of filters that will be applied to JMeter dataset.
#' Filters are applied sequentially one after another in pipeline fashion.
#' To add custom filter just add your function to the list. It should take 
#' JMeter data.table as input and return it's subset as output. data 
#' modifications are strictly forbidden.
param_jm_filter_chain <- list(
    #' name filter:
    #' * field - JMeter file column name where names for filtering are stored.
    #' * pattern_ok - regular expression that matches only valid transaction names.
    jmeter_filer_name(field = "label", pattern_ok = ".*"),
    
    #' test time filter:
    #' * deltas - 2 el. integer vector specifying number of seconds to remove from 
    #' the beginning and end of the test correspondingly.
    #' * duration - desired duration of the test. If after the crop with deltas 
    #' duration of the test is longer than this parameter - it will be truncated
    #' (by removing points from the end of the test) to match target duration.
    jmeter_filter_crop(deltas = c(0, 0), duration = NULL)
)

#' input to .process_rename function:
#' * name_field - JMeter file column name where transaction names are stored.
#' * pattern_ok - regular expression that matches only valid transaction names.
#' * pattern_rename - regular expression for transaction rename. Should contain 
#' at least one capture groups.
#' * match_group - number of capture group that stores new transaction name 
#' after applying pattern_rename regular expression.
param_names <- list(
    name_field = "label",
    pattern_rename = "(.*)",
    match_group = 1
)

#' input to .process_distributed function:
#' * distributed - should be set to TRUE if single load generator was used for 
#' test.
#' * machine_default - load generator name for non distributed test.
#' * pattern - regular expression for machine name extraction. Should contain 
#' at least one capture groups.
#' * match_group - number of capture group that stores machine name 
#' after applying pattern regular expression.
#' * machine_field - column name of JMeter file where machine name data is 
#' stored.
param_distributed <- list(
    distributed = FALSE,
    machine_default = "generator-0"
)

#' input to .process_bgroups function
#' * groups - mapping between transaction prefixes and business group names.
#' * group_missing - group by default (no info was provided in groups for 
#' specific transaction).
#' * field_src - JMeter file column name where transaction names are stored.
#' 
#' NOTE: always map Total to Total if you redefine this list and unless you
#' fully understand consequences
param_business_group <- list(
    groups = c("Total" = "Total"),
    group_missing = "Other",
    field_src = "label"
)

#' use success samples only in response time/latency summary tables
#' (except for passed/failed/total counts)
param_stat_success <- TRUE

#' timezone name (tz) for reporting time data
param_timezone <- "GMT"

#' function that produces hw metric group name from provided components
param_hw_group <- function(hostname, groupname, ...) {
    l <- list(...)
    if (groupname == "jmx" && length(l) > 0) {
        paste(groupname, tail(l, 1))
    } else {
        groupname
    }
}

#' max number of digits to print after decimal point
param_digits <- 3

#' granularity for time series. only single point per granularity interval is
#' printed
param_granularity <- 15

#' number of items to print in top-N most frequent and time consuming 
#' transactions charts
param_top_els <- 5

#' mapping between logical values and humain readable strings
param_logical_names = c(
    "TRUE" = "Passed",
    "FALSE" = "Failed"
)

#' numeric vector of points (in sec) for distribution table
param_distribution_points <- c(0.5, seq(1, 10, 1))

#' numeric vector of points for percentiles table. Use 0.9 to find 90% 
#' percentile
param_distribution_percentiles <- c(0.5, 0.75, 0.9, 0.95, 0.99)

#' character names of environment variables that contain VCS URL and revision
#' data (may be SVN revision or git commit tag)
param_env_vcs_url <- "SVN_URL"
param_env_vcs_rev <- "SVN_REVISION"

#' geom primitives settings for charts
param_charts_common <- list(
    line_size = 1,               # line width
    point_size = 3,              # point size
    gpalette = NULL,             # name of palette to use
    show_y_zero = TRUE,          # show zero on Y-Axis
    legend_cols = 3              # number of columns in chart legends
)

#' number of bins for boxplot over time chart.
param_util_bins <- 50

#' number of bins in barcharts
param_bar_bins <- 50

#' number of recent tests to show in trend report or character
#' vector with test ids to include into report
param_trend_tests <- 10 # c('2015-01-12 06:31:54 +0300', etc.)

#' number of transactions to show on trend degradation table
param_trend_degrtop <- 10

#' number of samples for moving mean (average) calculation
param_trend_mmean <- 20

#' number of SQL queries to show in SQL performance table
param_sqlrpt_top <- 15

# degradation is less than
.sla_degradation <- list(define_sla("value", 15, critical = TRUE))
.sla_deltas <- list(
    define_sla("verdict", param_logical_names["TRUE"], op = "=="),
    define_sla("rel.prev", 10),
    define_sla("rel.mm", 10)
)

#' service level agreements list
#' please refer to separate SLA configuration guides to customize this
param_sla <- list(
    "report.description" = list(
        define_sla("fail.ratio", 0.1, critical = TRUE) # 10% failure ratio
    ),
    "report.respcodes" = NULL,
    "elapsed.summary" = list(
        define_sla("p90", list("Total" = 10, "Other" = 10), critical = TRUE),
        define_sla("errate", list("Total" = 0.01, "Other" = 0.01), critical = TRUE)
    ),
    "elapsed.distribution" = lapply(
        param_distribution_points, 
        function(p) define_sla(paste0("t", p), 0.9, op = ">=")
    ),
    "elapsed.percentiles" = list(
        define_sla("p0.9", list("Total" = 10, "Other" = 10), critical = TRUE)
    ),
    "Latency.summary" = NULL,
    "Latency.distribution" = NULL,
    "Latency.percentiles" = NULL,
    "trend.status" = list(
        define_sla("status", param_logical_names["TRUE"], op = "==")
    ),
    "elapsed.degradation" = .sla_degradation,
    "hardware.degradation" = .sla_degradation,
    "codes.degradation" = .sla_degradation,
    "oracle.degradation" = .sla_degradation,
    "elapsed.deltas" = .sla_deltas,
    "hardware.deltas" = .sla_deltas,
    "codes.deltas" = .sla_deltas,
    "oracle.deltas" = .sla_deltas
)

#' SLA summary table description list.
#' names are SLA results ids and values are character vectors (names are humain
#' readable column names and values are source table column names). 
param_trend_summary <- list(
    "elapsed.summary.trend" = c(
        "90% RT" = "p90",
        "Err. ratio" = "errate"
    ),
    "elapsed.deltas.avg" = c(
        "ART Prev. Diff." = "rel.prev", 
        "ART MAvg. Diff." = "rel.mm"
    ),
    "elapsed.deltas.p90" = c(
        "90% RT Prev. Diff." = "rel.prev", 
        "90% RT MAvg. Diff." = "rel.mm"
    )
)

#' This list is used to simplify certain TABLE operations and currently not 
#' widely used. This may change in future releases.
jmeter_schema <- list(
    "time" = list(
        "name_sys" = "timeStamp",
        "name_full" = "test time",
        "name_short" = "time",
        "name_abbr" = "t",
        "name_hist" = "",
        "units" = "sec"
    ),
    "elapsed" = list(
        "name_sys" = "elapsed",
        "name_full" = "response time",
        "name_short" = "resp. time",
        "name_abbr" = "RT",
        "name_hist" = "Elapsed",
        "units" = "sec"
    ),
    "latency" = list(
        "name_sys" = "Latency",
        "name_full" = "latency time",
        "name_short" = "lat. time",
        "name_abbr" = "LT",
        "name_hist" = "Latency",
        "units" = "sec"
    ),
    "bytes" = list(
        "name_sys" = "bytes",
        "name_full" = "throughput",
        "name_short" = "throughput",
        "name_abbr" = "Thr",
        "name_hist" = "Bytes",
        "units" = "KB"
    ),
    "respcodes" = list(
        "name_sys" = "responseCode",
        "name_full" = "response codes",
        "name_short" = "resp. codes",
        "name_abbr" = "resp. codes",
        "name_hist" = "Codes",
        "units" = ""
    )
)
