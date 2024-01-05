#' Define gating arguments based on input
#'
#' @param gating.args Either "CD8.FLOW", "CD4.FLOW" or a custom character vector
#'
#' @return Character string representing the gating arguments
#'
#' @examples
#' get_flow_gatingargs("CD8.FLOW")
#' get_flow_gatingargs("CD4.FLOW")
#' # get_flow_gatingargs("custom argument")
#'
#' @export
get_flow_gatingargs <- function(gating.args = c("CD4.FLOW", "CD8.FLOW")) {

  ## check arguments
  gating.args <- match.arg(arg = gating.args)

  ## these are for CD4/CD8 FLOW, but may need to add some custom gating arguments here.
  if (toupper(gating.args) == "CD8.FLOW") {
    gating.args <- "CD8/CD38:CD8/CD69:CD8/CD137:CD8/Gnly:CD8/GrzA:CD8/GrzB:CD8/Ki67:CD8/LAG3:CD8/PD1:CD8/Prf:CD8/TIM3"
  } else if (toupper(gating.args) == "CD4.FLOW") {
    gating.args <- "CD4/CD38:CD4/CD69:CD4/Gnly:CD4/GrzA:CD4/GrzB:CD4/Ki67:CD4/LAG3:CD4/PD1:CD4/Prf:CD4/TIM3"
  } else {
    gating.args <- gating.args
  }
  return(gating.args)
}

#' Extract Polyfunctional Flow Data
#'
#' @description
#' Extracts combination gate data from a FlowJo workspace. The function creates and extracts
#' combination gates using the specified gating arguments and returns a data frame.
#'
#' @details
#' This function uses parallel processing to extract data from multiple workspace files.
#' It uses the openCyto and CytoML packages to work with the FlowJo workspace and the associated FCS files,
#' and the foreach and doParallel packages to handle the parallel processing.
#'
#' @param workspace.files A character vector of file paths for the FlowJo workspace files to be processed.
#' @param fcs.path A string indicating the path to the associated FCS files.
#' @param gating.args A character string or vector indicating the gating arguments to use. This can either be "CD8.FLOW", "CD4.FLOW" or a custom character vector with the desired combination gates.
#' @param parent.gate A string specifying the parent gate for the extracted statistics. This can either be "CD3", "CD4", or "CD8".
#'
#' @return
#' A data frame where each row represents one population from one sample (one FCS file), with the columns being the FCS file name, the population, and the cell count for that population.
#'
# @importFrom parallel detectCores makeCluster
# @importFrom foreach foreach
# @importFrom doParallel registerDoParallel stopImplicitCluster
# @importFrom openCyto gs_add_gating_method
# @importFrom CytoML open_flowjo_xml flowjo_to_gatingset
# @importFrom flowWorkspace recompute gs_pop_get_count_fast
#' @importFrom cli cli_abort
#'
#' @examples
#' \dontrun{
#'   Flow_PolyF_Extract(workspace.files = c("workspace1.wsp", "workspace2.wsp"),
#'                      fcs.path = "/path/to/fcs/files/",
#'                      gating.args = "CD8.FLOW",
#'                      parent.gate = "CD3")
#' }
#' @export
# Flow_PolyF_Extract <- function(workspace.files, fcs.path = ".", gating.args, parent.gate = "CD3") {
#
#   ## code below could be used in a validation function...
#   ## check arg for wsp file vector
#   if (length(workspace.files) == 0) {
#     cli_abort(message = paste0("Filepath provided does not have any .wsp files present."))
#   }
#   ## check arg for fcs path exists
#   if (!file.exists(fcs.path)) {
#     cli_abort(message = paste0("File '", fcs.path, "' does not exist."))
#   }
#   ## check arg for gating args
#   gating.args <- match.arg(arg = gating.args,
#                            choices = get_flow_gatingargs())
#   ## check arg for parent gate
#   parent.gate <- match.arg(arg = parent.gate,
#                            choices = c("CD3", "CD4", "CD8"))
#
#   ## create implicit clusters
#   detectCores() |>
#     makeCluster() |>
#     registerDoParallel()
#
#   ## assign NULL to global variable 'i'
#   i <- NULL
#
#   ## create loop for parallel processing
#   loop_output <- foreach(
#     i = 1:length(workspace.files),
#     .combine = rbind) %dopar% {
#
#       gating_set_add <- open_flowjo_xml(workspace.files[i]) |>
#         flowjo_to_gatingset(name = "All Samples",
#                             path = fcs.path)
#
#       gs_add_gating_method(gs = gating_set_add,
#                            gating_method = "polyFunctions",
#                            parent = parent.gate,
#                            gating_args = gating.args)
#
#       ## recompute with new gating method
#       recompute(gating_set_add)
#
#
#       ## extract cell population count data
#       ### this object does not get returned.. is this automatically returned
#       ### in the for each loop?
#       # extracted_data <- gs_pop_get_count_fast(x = gating_set_add,
#       gs_pop_get_count_fast(x = gating_set_add,
#                                               format = "long",
#                                               statistic = "count",
#                                               xml = FALSE)
#     } #end foreach loop
#
#   ## close down the parallel cluster
#   stopImplicitCluster()
#
#   return(loop_output)
#
# } #end function
