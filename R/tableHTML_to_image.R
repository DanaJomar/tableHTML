#' Export tableHTML as Image or PDF
#'
#' \code{tableHTML_to_image} export a tableHTML object to PNG, JPEG, or PDF. It uses the webshot package to convert HTML to image / PDF.
#'
#' \code{tableHTML_to_image} uses the \code{webshot} packages to export a tableHTML object
#' as PNG, JPEG, or PDF. By default, it selects the \code{table} DOM element, so the table will
#' always be cropped.
#' If you are exporting to JPEG, note that the default background color will be black.
#' Check the examples to see how to do that.
#'
#' @param tableHTML A tableHTML object created by the tableHTML function.
#'
#' @param file output file name, character. The file should be .png, .jpeg, or .pdf. Details see [webshot::webshot()]
#'
#' @param selector character. CSS selector specifying a DOM element to extract. The default
#' is 'table'. Details see [webshot::webshot()]
#'
#' @param ... Additional arguments passed to \code{webshot}. Details see [webshot::webshot()]
#'
#' @inheritParams tableHTML
#' @inheritParams webshot::webshot
#'
#' @examples
#'
#' mtcars %>%
#'  tableHTML() %>%
#'  tableHTML_to_image()
#'
#'
#
#' # Exporting JPEG:
#' mtcars %>%
#'  tableHTML() %>%
#'  add_css_table(css = list('background-color', 'white')) %>%
#'  tableHTML_to_image(file = 'tableHTML_export.jpeg')
#'
#' @export
tableHTML_to_image <- function(tableHTML,
                               file = 'tableHTML_export.png',
                               selector = 'table',
                               ...) {

 #CHECKS----------------------------------------------------------------------------------------
 if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')

 if (!'webshot' %in% utils::installed.packages()) stop('The webshot package is required to run this function. Please install it using \ninstall.packages("webshot")')

 if (!any(sapply(c('.png', '.jpeg', '.pdf'), function(x) {endsWith(file, x)}))) {
  stop('file should end with .png, .jpeg, or .pdf')
 }

 if (endsWith(file, '.jpeg')) {
  warning('The default background colour of the tableHTML in the exported jpeg will be black. See details in ?tableHTML_to_image')
 }

 temp_file <- tempfile(pattern = 'tableHTML',
                       fileext = '.html')

 write_tableHTML(tableHTML,
                 file = temp_file,
                 complete_html = TRUE)

 webshot::webshot(url = temp_file,
                  file = file,
                  selector = selector,
                  ...)

 invisible(NULL)
}

