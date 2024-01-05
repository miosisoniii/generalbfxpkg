

#' Load data from SharePoint
#'
#' This function retrieves the local path to either 'development' or 'production'
#' directories in SharePoint based on the specified 'type' parameter.
#'
#' @param type A character string specifying the path type, which can be either
#'   "dev" or "prod" (case-insensitive). Default is "dev".
#'   - "dev" or "Development": Returns the development path
#'   - "prod" or "Production": Returns the production path
#'
#' @return A character string containing the full path to either the
#'   'development' or 'production' SharePoint directory for the current user.
#' @export
#'
#' @examples
#' # Get the development path
#' dev_path <- load_sp("dev")
#' cat("Development path:\n", dev_path)
#'
#' # Get the production path
#' prod_path <- load_sp("prod")
#' cat("Production path:\n", prod_path)

load_sp <- function(type = c("dev", "prod")) {

  if (length(type) > 1) {
    warning("Arg `type` not supplied, defaulting to `dev`.")
    type <- "dev"

  } else {
    type <- match.arg(tolower(type),
                      choices = c("dev", "prod", "development", "production"),
                      several.ok = TRUE)
  }

  start_sp_path <- "C:/Users/"
  username <- tolower(Sys.info()['user'])
  bfx_sp_path <- "/path/to/files"

  if (type %in% c("dev", "development"))
    end_sp_path <- "bfxpkg/"
  if (type %in% c("prod", "production"))
    end_sp_path <- "Active Projects/"

  out_path <- paste0(start_sp_path, username, bfx_sp_path, end_sp_path)
  return(out_path)
}

