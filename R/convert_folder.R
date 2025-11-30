#' Convert all .Rmd/.qmd files in a folder
#'
#' @param dir        Path to the directory containing files.
#' @param pattern    Regex pattern for files to include. Default matches .Rmd and .qmd.
#' @param recursive  Logical, search recursively?
#' @param ...        Arguments passed to \code{convert_to_tutorial}.
#'
#' @return Invisibly, a vector of converted file paths.
#' @export
#' @importFrom cli cli_h1 cli_alert_info
convert_folder <- function(
    dir = ".",
    pattern = "\\.(Rmd|qmd)$",
    recursive = FALSE,
    ...
) {
    files <- list.files(
        dir,
        pattern = pattern,
        full.names = TRUE,
        recursive = recursive
    )

    if (length(files) == 0) {
        cli::cli_alert_info("No matching files found in {dir}")
        return(invisible(character(0)))
    }

    cli::cli_h1("Converting {length(files)} files")

    converted <- vapply(
        files,
        function(f) {
            convert_to_tutorial(f, ...)
        },
        character(1)
    )

    invisible(converted)
}
