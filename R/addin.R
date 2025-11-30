#' RStudio Addin: Convert Active Document to Tutorial
#'
#' Interactively converts the currently open document in RStudio.
#' @export
#' @importFrom rstudioapi getSourceEditorContext
launch_tutorizeR_addin <- function() {
    ctx <- rstudioapi::getSourceEditorContext()
    path <- ctx$path

    if (path == "") {
        stop("Please save the document before converting.", call. = FALSE)
    }

    convert_to_tutorial(path)
}
