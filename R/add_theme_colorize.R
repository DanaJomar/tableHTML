#' Add a theme to a tableHTML with a total row.
#'
#' \code{add_theme_colorize} will add an Excel-like theme to tableHTML
#'  and hightlights one or more total-rows.
#'
#' \code{add_theme_colorize} will add an Excel-like theme to tableHTML.
#' Column widths are not provided with the theme.
#' Please use the width argument for column widths.
#'
#' @param tableHTML A tableHTML object.
#'
#' @param color A character vector to specify the desired color. It can contain
#' at most two colors.
#' Accepts color names (as listed by \code{\link[=grDevices]{colors()}}),
#' as well as hexadecimal representation of the form "#rrggbb".
#'
#' If two colors are chosen, the first color will be the dominant one, and row coloring
#' will alternate between the first and second color.
#'
#' @param total_rows A numeric atomic vector with the indices
#' of the total/subtotal rows. Default is \code{NULL} which means
#' no row will be highlighted.
#'
#' @param  id_column A boolean, if set to \code{TRUE} the first column will
#' be highlighted as an ID column.
#' Default is \code{FALSE}.
#'
#' @return A tableHTML object.
#'
#' @examples
#' # no total rows
#' mtcars %>%
#'  tableHTML(widths = c(140, rep(50, 11))) %>%
#'  add_theme_colorize()
#'
#' # one total row
#' x1 <- sample(1:100, 12)
#' x2 <- sample(1:100, 12)
#' x3 <- sample(1:100, 12)
#'
#' df <- data.frame(Month = month.abb, x1, x2, x3,
#'                  stringsAsFactors = FALSE)
#'
#' df[nrow(df) + 1, ] <- c('Total', sum(x1), sum(x2), sum(x3))
#'
#' df %>%
#'   tableHTML(widths = rep(50, 4), rownames = FALSE) %>%
#'   add_theme_colorize(total_rows = nrow(df),
#'                      color = 'darkred')
#'
#'
#' # multiple subtotal rows
#' df_q <- rbind(
#'   df[1:3, ],
#'   c('Sum1', sum(x1[1:3]), sum(x2[1:3]), sum(x3[1:3])),
#'   df[4:6, ],
#'   c('Sum2', sum(x1[4:6]), sum(x2[4:6]), sum(x3[4:6])),
#'   df[7:9, ],
#'   c('Sum3', sum(x1[7:9]), sum(x2[7:9]), sum(x3[7:9])),
#'   df[10:12, ],
#'   c('Sum4', sum(x1[10:12]), sum(x2[10:12]), sum(x3[10:12])))
#'
#' df_q %>%
#'   tableHTML(widths = rep(50, 5),
#'             rownames = FALSE,
#'             row_groups = list(c(4, 4, 4, 4),
#'                               c('Q1', 'Q2', 'Q3', 'Q4'))) %>%
#'   add_theme_colorize(color = '#009999',
#'                    total_rows = c(4, 8, 12, 16))
#'
#' # Two colors and an id_column
#' df_q %>%
#'   tableHTML(widths = rep(50, 5),
#'             rownames = FALSE,
#'             row_groups = list(c(4, 4, 4, 4),
#'                               c('Q1', 'Q2', 'Q3', 'Q4'))) %>%
#'   add_theme_colorize(color = c('pink3', 'yellow2'),
#'                    total_rows = c(4, 8, 12, 16), id_column = TRUE)
#'
#' @export
add_theme_colorize <- function(tableHTML, color = 'steelblue',
                             total_rows = NULL, id_column=FALSE)
{
 # extract attributes
 n_rows <- attr(tableHTML, "nrows")
 n_cols <- attr(tableHTML, "ncols")
 second_headers <- attr(tableHTML, "second_headers_data")
 exist_second_header <- !is.null(second_headers)
 rownames <- attr(tableHTML, "rownames")
 row_groups <- attr(tableHTML, "row_groups_data")
 exist_row_groups <- !is.null(row_groups)

 # checks
 if (!inherits(tableHTML, 'tableHTML'))
  stop('tableHTML needs to be a tableHTML object')

 if (!is.null(total_rows))
  if (!is.numeric(total_rows))
   stop('total_rows should be either NULL or a numeric vector')

 if (length(color) > 2)
  stop('color should be a vector of at most two colors')

 if (!is.logical(id_column) || is.na(id_column))
  stop('id_column should be TRUE or FALSE')

 # prepare colors
 if(length(color) == 1){
  color <- c(color, color)
 }
 # add attributes for testing
 attr(tableHTML, 'theme') <- list('total_rows' = total_rows,
                                  'colors' =  color,
                                  'id_column' = id_column)

 rgb_col <- col2rgb(color)
 color <-  paste0('rgba(', paste0(rgb_col[, 1], collapse = ','), ',1)')
 header_background <- paste0('rgba(', paste0(rgb_col[, 1], collapse = ','), ',0.7)')
 background_color_1 <- paste0('rgba(', paste0(rgb_col[, 1], collapse = ','), ',0.3)')
 background_color_2 <- paste0('rgba(', paste0(rgb_col[, 2], collapse = ','), ',0.1)')

 # row indices to style
 x_rows <- 1 + exist_second_header
 rows <- 1:(n_rows + x_rows)

 # column indices to style
 x_cols <- rownames + exist_row_groups
 cols <- 1:(n_cols + x_cols)

 # style the total rowsand separate them from the rest of the rows
 if(!is.null(total_rows)){
  total_rows <- total_rows + x_rows
  tableHTML <- tableHTML %>%
   add_css_row(list(c('background', 'color'),
                    c(color, 'white')),
               rows = total_rows)
  rows <- setdiff(rows, total_rows)
 }

 # style the rest of the table
 tableHTML <- tableHTML %>%
  add_css_table(css = list(c('border'), c(paste0('3px solid ', color)))) %>%
  add_css_header(css = list(c('background', 'color'),
                            c(header_background, 'white')),
                 headers = cols) %>%
  add_css_row(css = list(c('border-top'), c(paste0('3px solid ', color))),
              rows = rows) %>%
  add_css_row(css = list('background', background_color_2), rows = odd(rows)) %>%
  add_css_row(css = list('background', background_color_1), rows = even(rows)) %>%
  add_css_column(css = list(c('text-align'), c('center')),
                 columns = cols) %>%
  replace_html('border=1', '')

 # special cases
 if(rownames){
  tableHTML <- tableHTML %>%
   add_css_column(css = list(c('background', 'color'),
                             c(header_background, 'white')), columns = 0)
 }
 if(exist_second_header){
  tableHTML <- tableHTML %>%
   add_css_second_header(css = list(c('border', 'background', 'color'),
                                    c(paste0('2px solid ', color), header_background, 'white')),
                         second_headers = 1:length(second_headers[[1]])) %>%
   add_css_column(css = list(c('border-right'),
                             c(paste0('2px solid ', color))),
                  columns = cumsum(second_headers[[1]])-x_cols)
 }
 if(exist_row_groups){
  tableHTML <- tableHTML %>%
   add_css_column(css = list(c('background', 'color', 'border-right'),
                             c(header_background, 'white', paste0('2px solid ', color))),
                  columns = -1)
 }
 if(id_column){
  tableHTML <- tableHTML %>%
   add_css_column(css = list(c('background', 'color', 'border-right'),
                             c(header_background, 'white', paste0('2px solid ', color))),
                  columns = 1)
 }
 tableHTML
}
