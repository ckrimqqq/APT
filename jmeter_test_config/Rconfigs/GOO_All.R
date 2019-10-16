param_verbose <- FALSE
param_report_brief <- FALSE

param_jm_filter_chain <- list(
  jmeter_filer_name(pattern_ok = "^GOO.*")
#  , jmeter_filter_crop(deltas = c(2100, 300), duration = 3600)
)

param_names$pattern_rename <- "(.*)"

# Environment variables where build information 
# (e.g. Version control URL and revision number)
# is stored
param_env_vcs_url <- "APP_SVN_URL"
param_env_vcs_rev <- "APP_SVN_REVISION"

param_charts_common <- list(
    line_size = 0.3,             # line width
    point_size = 3,              # point size
    gpalette = NULL,             # name of palette to use
    show_y_zero = TRUE,          # show zero on Y-Axis
    legend_cols = 3              # number of columns in chart legends
)

# Load HW config
#source(file.path(dirname(.opt$config), 'GOO-HW.R'))

# Load TX config
source(file.path(dirname(.opt$config), 'GOO-TX.R'))

param_distributed <- list(
    distributed = TRUE,
    pattern = ".*?@(.*?) \\d+-\\d+",
    match_group = 1,
    machine_field = "threadName"
)

param_timezone <- "EST"
param_stat_success <- TRUE
param_granularity <- 30

