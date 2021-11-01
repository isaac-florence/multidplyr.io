#' @title Read delimited files to workers
#'
#' @name read_delim_distrib
#'
#' @description
#' Read a delimited file (e.g. a csv) onto **multidplyr** workers in parallel
#' powered by **readr** and **vroom**. For most arguments below, the parameters
#' are applied directly to - and documentation adapted from - readr::read_delim
#'
#' @param cluster a multidplyr cluster
#' @param filename a path and file of the delimited file to read onto workers
#' @param delim Single character used to separate fields within a record.
#' @param col_types `NULL` or a [cols()] specification.
#' See `vignette("readr")` for more details. Note that current strings with
#' compact col_type specifications are not supported.
#'
#'   If `NULL`, all column types will be imputed from 200 rows
#'   on the input interspersed throughout the file. This is convenient (and
#'   fast), but not robust. If the imputation fails, you'll need to supply the
#'   correct types yourself.
#'
#'   Column specifications created by [list()] or [cols()] must contain
#'   one column specification for each column. If you only want to read a
#'   subset of the columns, use [cols_only()].
#'
#' @param show_col_types Set to `FALSE` to silence col_type summary printing
#' @export
read_delim_distrib <- function(cluster,
                               filename,
                               delim = ",",
                               col_types = NULL,
                               show_col_types = TRUE
) {

  # prep filepath and check exists
  filename <- normalizePath(filename)
  stopifnot(file.exists(filename))

  # get n lines and apportion according to cluster size
  lines <- count_rows(filename) - 1
  portion_size <- ceiling(lines / length(cluster))

  readingcli <- cli::cli_process_start(
    msg = "Preparing to read {lines} rows across {length(cluster)} workers"
  )

  # set starting rows (with next value as final row+1)
  bookends <- seq(1, lines, portion_size)

  # If `col_types` not given generate one centrally, and standardise one given
  sample <- readr::read_delim(
    filename,
    delim = delim,
    n_max = 200,
    show_col_types = FALSE,
    col_types = col_types
  )
  col_types <- readr::spec(sample)

  if(show_col_types){
    v_summary_col_spec <- utils::getFromNamespace("summary.col_spec", "vroom")
    v_summary_col_spec(col_types)
  }


  # True colnames and col_types with generated colnames
  # to match readr output when colnames = FALSE
  true_names <- names(col_types$cols)
  names(col_types$cols) <- paste0("X", seq_along(col_types$cols))

  dfname <- multidplyr:::table_name()

  # assign each worker its bookend
  multidplyr::cluster_assign_each(cluster, bookends = bookends)

  # send commands to read worker's own portion of csv
  multidplyr::cluster_send(cluster, {
    x <- readr::read_delim(
      file = !!filename,
      delim = !!delim,
      col_types = !!col_types,
      skip = bookends,
      n_max = !!portion_size,
      col_names = FALSE
    )

    # Avoid "no visible global function definition for '!<-' note"
    assign("true_names", !!true_names)
    colnames(x) <- true_names
    assign(!!dfname, x)
    rm(x)
  })

  csv_party_df <- multidplyr::party_df(cluster, dfname)

  cli::cli_process_done(readingcli)

  return(csv_party_df)
}
