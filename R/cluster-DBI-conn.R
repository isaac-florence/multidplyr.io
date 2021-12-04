#' @title DBI SQL connection in cluster
#' @describeIn  cluster_DBI_con
#'
#' @description
#' In order to initiate individual connections per worker,
#' the connection details need to be made available to the workers,
#' and DI connection objects created (and subsequently disconnected)
#'
#' @details
#' Use this function after creating a new cluster (`multidplyr::new_cluster(n)`),
#' with the same details used to create **{DBI}** connections.
#'
#' This creates a DBI connection object (`con`) on each worker which will require
#' disconnection as with any **{DBI}** connection.
#'
#' @examples
#' \dontrun{
#'
#' cluster <- multidplyr::new_cluster()
#'
#' cluster <- cluster_DBI_con(cluster, odbc::odbc, dsn = "my_dsn_name")
#'
#' cluster <- cluster_DBI_discon(cluster)
#' }
#'
#' @param cluster `multidplyr_cluster` object
#' @param drv a DBI driver, e.g. `odbc::odbc()`
#' @param ... additional arguments passed to DBI
#'
#'
#' @importFrom rlang "!!"
#'
#' @export
cluster_DBI_con <- function(cluster, drv, ...) {
  dots <- enquos(...)

  # test connection
  DBI::dbCanConnect(drv, ...)

  # connect on workers
  multidplyr::cluster_send(cluster, {
    con <- DBI::dbConnect(!!drv, !!!dots)
  })

  # set attribute for active connection
  cluster <- structure(cluster, DBI = "<Connected>")

  invisible(cluster)
}



#' @describeIn cluster_DBI_con
#'
#' @description
#' Disconnect multidplyr cluster workers from DBI connection
#'
#' @param cluster `multidplyr_cluster` object has had DBI initiation
#'
#' @export
cluster_DBI_discon <- function(cluster) {
  if (is.null(attr(cluster, "DBI"))) {
    stop(cli::format_error("This multidplyr_cluster has no DBI connection initiation"))
  } else if (attr(cluster, "DBI")$msg == "<Disconnected>") {
    stop(cli::format_error("Already disconnected"))
  }

  multidplyr::cluster_send(cluster, {
    rm(list = c("con"))
  })

  structure(cluster, DBI = "<Disconnected>")
}
