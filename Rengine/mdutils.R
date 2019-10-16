#' This file contains helper functions for transforming R data types and 
#' outputs of functions to HTML, Markdown or text.
#' 
#' Rmd files are general consumers of this API.

apt_include("base64enc")

#' Convert single string to valid html id for navigation.
#' 
#' @param text text to convert to html id.
#' @return valid html id.
to_id <- function(text)
    paste0("id_", gsub("=", "", base64encode(charToRaw(text)), fixed = TRUE))

### HTML TABLE PRINTER ###

#' Create html printer function.
#' 
#' @param float_digits max number of digits after the point to print for 
#' decimals.
#' @return text-serialization function with arguments:
#' * els - vector with elements to textify.
#' * wrap_text - serialized string html tag wrapper.
#' * html_attrs - character vector with the same length as els that specifies 
#' html tag attributes for each element.
#' * reduce_with - separator for merging els to single string.
#' and returns character vector with serialized content.
.tag_wrapper <- function(float_digits = getOption("digits")) {
    popen <- "<%s%s%s>"
    pclose <- "</%s>"
    float_format <- paste0("%.", float_digits, "f")
    function(els, html_attrs = NULL, wrap_text = "td", reduce_with = NULL) {
        if (is.character(reduce_with)) {
            els <- paste0(els, collapse = reduce_with) 
        }
        use_attrs <- !is.null(html_attrs) && nchar(html_attrs) > 0
        fmt <- rep(ifelse(is.double(els), float_format, "%s"), length(els))
        tags <- paste0(popen, fmt, pclose)
        whspc <- ifelse(use_attrs, " ", "")
        html_attrs <- if (use_attrs) html_attrs else rep("", length(fmt))
        sprintf(tags, wrap_text, whspc, html_attrs, els, wrap_text)
    }
}

#' Rename and remove data.table columns.
#' 
#' @param tbl target data.table
#' @param tbl_names a list that maps source column names to target. If target 
#' column name is NA it will be removed.
#' @return modified tbl.
.rename_table <- function(tbl, tbl_names) {
    names_available <- names(tbl_names)
    cols <- colnames(tbl)
    cols_rename <- cols[cols %in% names_available]
    cols_remove <- cols_rename[is.na(tbl_names[cols_rename])]
    cols_inc <- setdiff(cols, cols_remove)
    tbl <- tbl[, cols_inc, with = FALSE] # filtering column if name set to NA
    rename_candidates <- tbl_names[setdiff(cols_rename, cols_remove)]
    if (length(rename_candidates) > 0) {
        setnames(tbl, names(rename_candidates), unlist(rename_candidates))
    }
    tbl
}

#' Generate HTML class based on SLA validation status.
#' 
#' @param sla_res logical vector with SLA results
#' @param class_sla list with 2 character elements: pass and fail. Each contains
#' HTML class names for passed and failed SLA.
#' @return character vector with HTML class information.
.sla_classes <- function(sla_res, class_sla)
    ifelse(is.na(sla_res), rep("class=''", length(sla_res)), 
           sprintf("class='%s'", ifelse(sla_res, class_sla$pass, class_sla$fail)))

#' Convert column data to links.
#' 
#' @param tbl source data.table
#' @param cols character vector of column names to create links from.
#' @param wrapper formatter function created via call to .tag_wrapper
#' @return tbl with columns converted to links.
.linkify_cols <- function(tbl, cols, wrapper) {
    for (col in cols) {
        if (cols %in% colnames(tbl)) {
            tbl[[cols]] <- lapply(
                tbl[[cols]], 
                function(el) wrapper(el, sprintf("href='#%s'", to_id(el)), "a")
            )
        }
    }
    tbl
}

#' Convert data.table elements to string
#' 
#' @param tbl source data.table
#' @param sla_tbl logial SLA matrix with same dim as tbl.
#' @param frmt formatter function created via call to .tag_wrapper
#' @param col_names character vector or list of column names mapping 
#' (source = target).
#' @param use_col_names print column names.
#' @param class_sla list with 2 character elements: pass and fail. Each contains
#' HTML class names for passed and failed SLA.
#' @return tbl with all elements converted to strings
.textify_table <- function(tbl, sla_tbl, frmt, col_names, use_col_names, class_sla) {
    if (is.null(dim(sla_tbl)))
        sla_tbl <- matrix(sla_tbl, nrow = 1, byrow = TRUE)
    
    sla_m <- apply(sla_tbl, 2, .sla_classes, class_sla = class_sla)
    sla_m <- do.call(data.table, 
                     if (is.null(dim(sla_m))) as.list(sla_m) else list(sla_m))
    tbl_f <- mapply(frmt, tbl, sla_m, 
                    MoreArgs = list(wrap_text = "td"), SIMPLIFY = FALSE)
    tbl_txt <- do.call(data.table, tbl_f)
    tbl_txt <- .rename_table(tbl_txt, col_names)
    
    if (use_col_names) 
        rbind(as.list(frmt(colnames(tbl_txt), wrap_text = "th")), tbl_txt)
    else tbl_txt
}

#' Transponse data.table
#' 
#' @param tbl source data.table
#' @return transposed data.table
.transpose <- function(tbl) {
    tbl_T <- t(tbl)
    colnames(tbl_T) <- rownames(tbl)
    data.table(tbl_T)
}

#' Create HTML table from data.table
#' 
#' @param tbl source data.table
#' @param frmt formatter function created via call to .tag_wrapper
#' @param table_attrs string with HTML atributes of table
#' @return textual representation of tbl
.serialize_table <- function(tbl, frmt, table_attrs) {
    tbl_rs <- apply(t(tbl), 2, frmt, wrap_text = "tr", reduce_with = "")
    frmt(tbl_rs, table_attrs, wrap_text = "table", reduce_with = "")
}

#' Print data.table as HTML table.
#' 
#' @param tbl source data.table
#' @param sla_m logical SLA statuses matrix with same dims as tbl. 
#' @param use_names print column names.
#' @param transpose transpose table.
#' @param sortable mark table as sortable.
#' @param linkify_cols character list of column names that should be converted 
#' to html links.
#' @param col_names list or vector with source to target column names mapping 
#' (use NA to remove column).
#' @param class_sla list with 2 character elements: pass and fail. Each contains
#' HTML class names for passed and failed SLA.
#' @param class_sortable HTML class of sortable tables.
#' @param class_transposed HTML class of transposed tables.
#' @parma float_digits total number of digit after point to print in decimals.
#' @param placeholder text to print if table is not serializable.
#' @param order_by character name of column for ordering.
#' @param order_desc use descendant order when sorting.
print_table <- function(tbl, sla_m,
                        use_names = TRUE,  # AFTER TRANSPOSE IF SPECIFIED
                        transpose = FALSE,
                        sortable = TRUE,
                        linkify_cols = NULL,
                        col_names = list(),    # BEFORE TRANSPOSE IF SPECIFIED
                        class_sla = list(pass = "sla-pass", fail = "sla-fail"),
                        class_sortable = "sortable",
                        class_transposed = "transposed",
                        float_digits = getOption("digits"),
                        placeholder = "_Not Available_",
                        order_by = NULL,
                        order_desc = TRUE) {
    r <- if (!is.null(tbl) && nrow(tbl) > 0) {
        tbl_class <- ""
        frmt <- .tag_wrapper(float_digits = float_digits)
        
        if (!is.null(order_by)) {
            n_order <- order(tbl[[order_by]], decreasing = order_desc)
            tbl <- tbl[n_order]
            sla_m <- sla_m[n_order,]
        }
        tbl_txt <- .linkify_cols(tbl, linkify_cols, frmt)
        tbl_txt <- .textify_table(
            tbl_txt, sla_m, frmt, col_names, use_names, class_sla
        )
        
        if (transpose) {
            tbl_txt <- .transpose(tbl_txt)
            tbl_class <- c(tbl_class, class_transposed)
        }
        if (sortable) {
            tbl_class <- c(tbl_class, class_sortable)
        }
        tbl_class <- do.call(paste0, c(tbl_class, list(sep = " ")))
        tbl_attr <- sprintf("class='%s'", tbl_class)
        .serialize_table(tbl_txt, frmt, tbl_attr)
    } else placeholder
    cat(r)
}

### Plotting functions

#' Generic function for layered data charting. 
#' 
#' @param results source data.frame.
#' @param type chart function to apply.
#' @param options list of lists with unique arguments to type per each layer.
#' @param ... arguments common to all layers.
#' @param no.print do not print chart and return it as object.
#' @return if (no.print) ggplot2 chart else NULL.
chart <- function(results,
                  type, 
                  options,
                  no.print = FALSE,
                  ...) {
    if (!is.null(results) && nrow(results) > 1) { # do not plot single point
        common_options <- list(...)
        reduce_func <- function(curr_plot, curr_opt) {
            tmp <- list(result = results, init_plot = curr_plot)
            do.call(type, c(tmp, curr_opt, common_options))
        }
        r <- Reduce(reduce_func, options, NULL)
        # suppress all warnings thrown from chart construction functions
        if (!is.null(r)) {
            if (no.print) {
                r
            } else {
                suppressWarnings(print(r))
            }
        }
    }
}
