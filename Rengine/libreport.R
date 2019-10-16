#' Report utilities library.
#'
#' This file contains classes and functions that can be used from md files and
#' make report generation process much simpler.

apt_include("memoise")

#' Reference class for resource dependencies managment.
#'
#' Each plugin included into report can add JS and CSS dependencies here,
#' and report will manage them automatically.
#' 
#' @field scripts script dependencies vector.
#' @field styles styles dependencies vector.
#' @field template HTML template dependency string.
setRefClass(
    Class = "ResourceDependencies",
    fields = list(
        scripts = "character",
        styles = "character",
        template = "character"
    ),
    methods = list(
        #' Add CSS styles as resource dependency.
        #'
        #' - css character vector of paths to related CSS styles.
        add_styles = function(css) {
            styles <<- union(styles, css)
        },
        #' Add JS scripts as resource dependency.
        #'
        #' - js character vector of paths to related JS scripts.
        add_scripts = function(js) {
            scripts <<- union(scripts, js)
        },
        #' Set HTML template for Markdown - HTML conversion.
        #'
        #' - html character path to related HTML template.
        set_template = function(html) {
            template <<- html
        }
    )
)

#' Return results tuple that builder event processing functions can understand.
#'
#' This function can be called from get_summary, get_element but not from 
#' get_group (instance of AbstractReportSource is expected). It's better to use
#' this one instead of direct list creation.
#' 
#' @field d dataset that will be visible as current_data within event Rmd file.
#' @field name dataset name that will be visible as current_name within 
#' event Rmd file.
reportResTuple <- function(d, name) list(d = d, alias = name)

#' Abstract class for Report Source instances.
#'
#' Defines methods for iterating over abstract data source (typically defined
#' within each plugin) and data retrieval from them. It is assumed that any
#' object has no more than 3 levels of hierarchy:
#' - object itself
#' - - groups
#' - - - elements
#' If some of levels are undefined corresponding get method should return NULL,
#' that will cause JMeter to stop digging to levels below.
setRefClass(
    Class = "AbstractReportSource",
    fields = list(
        #' data itself, usually data.frame, data.table or list
        d = "ANY",
        #' alias name for data source. Should be unique for each plugin.
        alias = "character"
    ),
    methods = list(
        #' Get summary for report source. Either AbstractReportSource descendant
        #' or result from reportResTuple function.
        get_summary = function() NULL,
        #' Get iterable list/vector of group names.
        get_groups = function() NULL,
        #' Get AbstractReportSource descendant class with group data.
        #'
        #' - groupname name of the group to retrieve data for.
        get_group = function(groupname) NULL,
        #' Get iterable list/vector of element names.
        get_elements = function() NULL,
        #' Get AbstractReportSource descendant class with element data or result 
        #' of reportResTuple function.
        #'
        #' - elemname name of the element to retrieve data for.
        get_element = function(elemname) NULL
    ),
    contains = c("VIRTUAL")
)

#' Reference Builder class.
#'
#' This class is used to automatically manage plugin dependencies, include
#' third party components and traverse data source hierarchies.
#' 
#' @field plugins all plugins that R finds under config directory are registered here.
#' @field registry path resolution function relative to components registry.
#' @field settings registred plugin handlers.
#' @field envir associated environment of current builder.
#' @field level current level of builder for headings.
#' @field hlevel level of last header.
setRefClass(
    Class = "ReportBuilder",
    fields = list(
        plugins = "list",
        registry = "function",
        settings = "list",
        envir = "environment",
        level = "numeric",
        hlevel = "numeric"
    ),
    methods = list(
        #' Class constructor.
        #'
        #' - assoc_envir associated environment object.
        #' - registry_func registry fetcher function.
        #' - registry_loc is registry_func is not a function this should be
        #' character path to repository.
        initialize = function(assoc_envir = parent.frame(3), # 3 because new(...) adds 2 parent envirs
                              registry_func = NULL,
                              registry_loc = "registry") {
            plugins <<- list()
            settings <<- list()
            level <<- 0
            hlevel <<- 0
            envir <<- assoc_envir
            
            if (is.function(registry_func)) {
                registry <<- registry_func
            } else {
                registry <<- init_registry(registry_loc)
            }
        },
        #' Go one level down.
        level_down = function(by = 1) {
            level <<- level + by
        },
        #' Go one level up.
        level_up = function(by = 1) {
            level <<- level - by
        },
        #' Set variable to associated environment.
        #'
        #' - varname character name of variable.
        #' - value variable value.
        set_env = function(varname, value) {
            assign(varname, value, envir = envir)
        },
        #' Get variable from associated environment.
        #'
        #' - varname character name of variable.
        get_env = function(varname) {
            if (is_defined(varname)) {
                get(varname, envir = envir)
            } else {
                NULL
            }
        },
        #' Checks if variable is defined in associated environment.
        #'
        #' - varname character name of variable.
        is_defined = function(varname) {
            exists(varname, envir = envir)
        },
        #' Initialize builder from config directory.
        #'
        #' Upon initializing calls pinit_* rmds in registry if it
        #' exists for each plugin.
        #' Also creates separate builder and isolated environment for each
        #' plugin and sets 2 variables in it:
        #' 1) builder - plugin local builder (if you need to include any *.rmd
        #' from children you chould use builder$include)
        #' 2) handler - data source instance associated with this plugin.
        #'
        #' - rpath path to config directory.
        init_plugins = function(rpath) {
            output <- NULL
            level <<- 0
            if (!is.null(rpath) && file.exists(rpath)) {
                modes <- sort(list.files(rpath, pattern = "^\\d+_.+$"))
                plugins <<- lapply(modes, function(plug_mode) {
                    mname <- tolower(sub("^\\d+_(.+)$", "\\1", plug_mode))
                    child <- new(
                        "ReportBuilder",
                        registry_func = registry,
                        assoc_envir = new.env(parent = envir)
                    )
                    child$set_env("builder", child)
                    init_file <- registry("pinit_", mname)
                    if (file.exists(init_file)) {
                        output <- eval_rmds(init_file)
                        if (!is.null(settings[[mname]]))
                            child$set_env("handler", settings[[mname]])
                    }
                    list(
                        fetcher = init_registry(file.path(rpath, plug_mode)),
                        type = mname,
                        dummy = child
                    )
                })
                apt_logger_verbose(
                    "INFO: activated plugins: ",
                    do.call(paste, as.list(names(settings)))
                )
            } else {
                apt_logger_verbose("WARN: report schema not specified or poins to bad location")
            }
            # must not return anything
        },
        #' Terminate builder and plugins.
        #'
        #' Upon terminating calls pterm_* rmds in registry if it
        #' exists for each plugin.
        terminate = function() {
            output <- NULL
            for (plugin in rev(plugins)) {
                term_file <- registry("pterm_", plugin$type)
                if (file.exists(term_file))
                    output <- eval_rmds(term_file)
            }
            # must not return anything
        },
        #' Start geport creation.
        #'
        #' This method prints topmost misc things like CSS styles and sets
        #' necessary options to shared data structures.
        #'
        #' - htmltemplate path to HTML template that is used by Markdown
        #' to HTML convertor.
        #' - stylesheets character vector of paths to dependent CSS styles.
        #' - tzvar character name of variable with timezone info.
        #'
        #' This function prints things through cat function and returns nothing.
        begin = function(htmltemplate = "resources/template.html",
                         stylesheets = "resources/css/report.css",
                         tzvar = "param_timezone") {
            apt_logger("INFO: report rmd -> md conversion started")
            options(markdown.HTML.template = htmltemplate)
            options(tz = get_env(tzvar))
            Sys.setenv(TZ = get_env(tzvar))
            header_html <- "</p><div id='blockrightcol'><div id='content'><p>"
            header_style <- lapply(stylesheets, .embed_css)
            print(c(header_html, header_style))
        },
        #' Finish geport creation.
        #'
        #' This method prints misc things in footer like JS scripts.
        #'
        #' - scripts character vector of paths to dependent JS scripts.
        #'
        #' This function prints things through cat function and returns nothing.
        end = function(scripts = "resources/js/sorter.js") {
            apt_logger("INFO: report rmd -> md conversion finished")
            footer_html <- c("</div></div>", lapply(scripts, .embed_js))
            print(footer_html)
        },
        #' Print text through cat function.
        #'
        #' - what vector of things for printing.
        #' - no_endlines do not append trailing endlines.
        #'
        #' This function prints things through cat function and returns nothing.
        print = function(what, no_endlines = FALSE) {
            if (!no_endlines)
                what <- c(what, "\n\n")
            cat(paste0(what, collapse = "\n"))
        },
        #' Evaluate text as rmd template.
        #'
        #' This function doesn't use cat, it just returns output!
        #'
        #' - text text of rmd template for evaluation.
        #' - tname name of component for logging.
        #' - use.expand apply knit_expand before knit_child to replace template
        #' variables substitution (variable syntax {{varname}}).
        eval_text = function(text, tname = "fragment", use.expand = FALSE) {
            tryCatch({
                ftime <- proc.time()
                if (use.expand) {
                    text <- do.call(knit_expand, list(text = text), envir = envir)
                }
                block <- knit_child(text = text, envir = envir)
                ftime <- proc.time() - ftime
                apt_logger_verbose(
                    sprintf(
                        "'%s' done (usr: %.2f, sys: %.2f, clock: %.2f).",
                        tname, ftime[1], ftime[2], ftime[3]
                    )
                )
                block
            }, error = function(e) {
                apt_logger(
                    "ERROR: error thrown during child document",
                    tname,
                    "processing. Error message:\n",
                    e$message
                )
            })
        },
        #' Evaluate rmd templates from file.
        #'
        #' This function doesn't use cat, it just returns output!
        #'
        #' - paths character vector of paths to rmd templates.
        #' - use.expand apply knit_expand before knit_child to replace template
        #' variables substitution (variable syntax {{varname}}).
        eval_rmds = function(paths, use.expand = FALSE) {
            output <- NULL
            for (path in paths) {
                output <- c(
                    output,
                    eval_text(
                        .embed_res(path),
                        tname = basename(path),
                        use.expand = use.expand
                    )
                )
            }
            paste(output, collapse = "\n")
        },
        #' Include rmd template to current template.
        #'
        #' This is universal component inclusion function that makes reports
        #' extremely flexible.
        #'
        #' - name relative path to child rmd within component registry
        #' WITHOUT extension.
        #' - arguments list of variables that should be set before rmd
        #' evaluation (names are variable names and values are their values).
        #' - plugin.owner if called from rmd that supports plugins set this to
        #' rmd name. R will search for a component not in registry, but in
        #' ${name}_plugins dir.
        #' - on_level set the level of heading that should become a root of
        #' fragment. If on_level = 1, then any heading 1 in child document
        #' would become heading 2 in parent. To embed use last_heading - 1
        #' value or do not include any heading at all before include.
        #' - use.expand apply knit_expand before knit_child to replace template
        #' variables substitution (variable syntax {{varname}}).
        include = function(name,
                           arguments,
                           plugin.owner = NULL,
                           on_level = NA,
                           nested_env = TRUE,
                           use.expand = FALSE) {
            include.env <- if (nested_env) new.env(parent = envir) else envir
            original.env <- envir
            original.hlevel <- hlevel
            envir <<- include.env
            path <- if (is.null(plugin.owner)) {
                registry(name)
            } else {
                registry(plugin.owner, "_plugins/", name)
            }
            for (arg_name in names(arguments)) {
                set_env(arg_name, arguments[[arg_name]])
            }
            delta <- ifelse(is.na(on_level), hlevel, on_level)
            if (delta > 0) level_down(by = delta)
            hlevel <<- 0
            print(eval_rmds(path, use.expand = use.expand))
            if (delta > 0) level_up(by = delta)
            hlevel <<- original.hlevel
            envir <<- original.env
        },
        #' Create Markdown heading mark with given nested level relative to
        #' builder level.
        #'
        #' This function doesn't print anything, it just returns output as
        #' character. Normally it should be used with rmd like this:
        #'
        #' `r builder$heading(n)` Text of n-th heading
        #'
        #' - rel_level relative level of heading.
        heading = function(rel_level = 1) {
            hlevel <<- rel_level
            paste(rep("#", level + rel_level), collapse = "")
        },
        #' Add variables from different sources to associated environment.
        #'
        #' Files are processed before textual variables.
        #'
        #' - expvars variables that consumer expects to see after the execution.
        #' If variable is not provided by any source it'll be set to NULL.
        #' - is_file vector of the same length as expvars. Contains logical
        #' values, indicating if corresponding argument in expvars is file.
        #' - var_files character vector of paths to R scripts for sourcing.
        #' - text_vars list of variables (names/values) that should be evaluated
        #' in current environment.
        inject_vars = function(expvars,
                               is_file = rep(TRUE, length(expvars)),
                               var_files = NULL,
                               text_vars = NULL) {
            for (var_file in var_files) {
                if (file.exists(var_file))
                    source(var_file)
            }
            lapply(text_vars, function(txt) eval(parse(text = txt), envir = envir))
            
            for (i in seq(1, length(expvars))) {
                varn = expvars[i]
                if (!is_defined(varn)) {
                    set_env(varn, NULL)
                    apt_logger_verbose("WARN: variable ", varn, " is not exist (check plugin docs to see if it's OK).")
                } else if (is_file[i]) {
                    set_env(varn, apt_path_resolve(get_env(varn), exists = FALSE))
                }
            }
        },
        #' Called when new event occures.
        #'
        #' Event is characterized by name, plugin and handler.
        #'
        #' R looks for file in plugin folder that matches with current event
        #' type.
        #'
        #' - what event character name.
        #' - data plugin handler.
        #' - plugin associated plugin.
        #' - output character output buffer.
        seq_do_eval = function(what, data, plugin, output = NULL) {
            if (file.exists(what) && !is.null(data)) {
                plugin$dummy$set_env("current_data", data$d)
                plugin$dummy$set_env("current_name", data$alias)
                plugin$dummy$level <- level
                c(output, plugin$dummy$eval_rmds(what))
            } else {
                output
            }
        },
        #' Triggers processing of sequential events.
        #'
        #' Event seq is traversed and for each plugin corresponding method is
        #' triggered:
        #'
        #' event_seq <- c("e1", "e2")
        #' - e1 for plugin#1
        #' - e1 for plugin#2
        #' - e2 for plugin#1
        #' - e2 for plugin#2
        #'
        #' Sets 2 variables within child environment:
        #' 1) current_name - handler alias
        #' 2) current_data - handler data
        #'
        #' NOTE: handler$d == current_data
        #'
        #' - event_seq character vector of event names.
        #' - as_new_lvl evaluate fragment in new level.
        linear_event_seq = function(event_seq = "summary", as_new_lvl = FALSE) {
            output <- NULL
            
            if (as_new_lvl) level_down()
            for (event in event_seq) {
                for (plugin in plugins) {
                    sett <- settings[[plugin$type]]
                    if (!is.null(sett)) {
                        output <- seq_do_eval(
                            what = plugin$fetcher(event),
                            data = sett$get_summary(),
                            plugin = plugin,
                            output = output
                        )
                    }
                }
            }
            if (as_new_lvl) level_up()
            print(output)
        },
        #' Triggers processing of hierarchical events.
        #'
        #' Method traverses each plugins' handler recursively and triggers
        #' corresponding events.
        #'
        #' on.block        - plugin#1
        #'  - on.group     - plugin#1
        #'    - on.el      - plugin#1
        #'    - ...
        #'  - on.group_end - plugin#1
        #'  - on.group     - plugin#1
        #'  - ...
        #' on.block_end    - plugin#1
        #' on.block        - plugin#2
        #' ...
        #'
        #' Sets 2 variables within child environment:
        #' 1) current_name - alias of current level handler
        #' 2) current_data - data of current level handler
        #'
        #' NOTE: handler$d != current_data, since handler is root handler and
        #' current_data amy refer to group data or element.
        #'
        #' - on.* character event names for defined levels.
        #' - as_new_lvl start each on.block event evaluation in new level.
        hier_event_seq = function(on.block = "block",
                                  on.group = "group",
                                  on.el = "element",
                                  on.group_end = "group_end",
                                  on.block_end = "block_end",
                                  as_new_lvl = FALSE) {
            output <- NULL
            if (as_new_lvl) level_down()
            for (plugin in plugins) {
                sett <- settings[[plugin$type]]
                if (!is.null(sett)) {
                    output <- seq_do_eval(
                        what = plugin$fetcher(on.block),
                        data = sett,
                        plugin = plugin,
                        output = output
                    )
                    level_down()
                    for (group in sett$get_groups()) {
                        group.dat <- sett$get_group(group)
                        output <- seq_do_eval(
                            what = plugin$fetcher(on.group),
                            data = group.dat,
                            plugin = plugin,
                            output = output
                        )
                        
                        level_down()
                        for (element in group.dat$get_elements()) {
                            output <- seq_do_eval(
                                what = plugin$fetcher(on.el),
                                data = group.dat$get_element(element),
                                plugin = plugin,
                                output = output
                            )
                        }
                        level_up()
                        
                        output <- seq_do_eval(
                            what = plugin$fetcher(on.group_end),
                            data = group.dat,
                            plugin = plugin,
                            output = output
                        )
                    }
                    level_up()
                    output <- seq_do_eval(
                        what = plugin$fetcher(on.block_end),
                        data = sett,
                        plugin = plugin,
                        output = output
                    )
                }
            }
            if (as_new_lvl) level_up()
            print(output)
        },
        #' Register handler for specific plugin.
        #'
        #' Should only be called in pinit_ initializers.
        #'
        #' If plugin initializer never calls this plugin won't be registered and
        #' will be ignored during event processing.
        #'
        #' - name caharacter name of plugin, should match with plugin folder
        #' name, e.g. 'jmeter' for '01_jmeter'
        #' - handler data source proxy object.
        register_plugin_handler = function(name, handler) {
            settings[[name]] <<- handler
        }
        )
    )

#' Return content of file as single string.
#'
#' @param file file location
#' @return file contents as single string
.embed_res <- memoise(
    function(path) paste(readLines(path), collapse = "\n")
)

#' Get embedded HTML version of JS file
#'
#' @param file script location
#' @return HTML string with embedded JS
.embed_js <- function(path)
    sprintf("<script type='text/javascript'>%s</script>", .embed_res(path))

#' Get embedded HTML version of CSS file
#'
#' @param file stylesheet location
#' @return HTML string with embedded CSS
.embed_css <- function(path)
    sprintf("<style>%s</style>", .embed_res(path))

#' Create function for path resolution relative to given root. Extention .rmd
#' will be given automatically.
#'
#' @param registry root for paths resolution.
#' @return function that maps input to path relative to given root.
init_registry <- function(registry = "registry") {
    function(...) {
        extra <- list(...)
        extra <- c(extra, ".rmd")
        file.path(registry, do.call(paste0, extra))
    }
}
