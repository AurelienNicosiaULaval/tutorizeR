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

#' RStudio Addin: Convert Folder to Tutorial
#'
#' Interactively selects a folder and converts all contained .Rmd/.qmd files.
#' @export
#' @importFrom rstudioapi selectDirectory
launch_tutorizeR_folder_addin <- function() {
    folder <- rstudioapi::selectDirectory(
        caption = "Select Folder to Convert"
    )

    if (is.null(folder) || folder == "") {
        return(invisible(NULL))
    }

    convert_folder(folder)
}
