#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to remove columns of matrix                      ##
##                                                             ##
##                                             2018-08-28      ##
##                                                             ##
##                                                             ##
#################################################################

#' @title Remove columns of a data set
#' @name lucC_remove_columns
#' @aliases lucC_remove_columns
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Remove columns from a data_mtx
#'
#' @usage lucC_remove_columns(data_mtx = NULL, name_columns = NULL)
#'
#' @param data_mtx      Matrix. A matrix with values obtained from predicates RECUR, EVOLVE, CONVERT or HOLDS
#' @param name_columns  Character. Name of columns to remove from data set
#'
#' @keywords datasets
#' @return Matrix with without selected columns
#' @importFrom ensurer ensure_that
#' @export
#'
#' @examples \dontrun{
#' library(lucCalculus)
#'
#' file <- c(system.file("extdata/raster/rasterSample.tif", package = "lucCalculus"))
#' rb_class <- raster::brick(file)
#' my_label <- c("Degradation", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
#'               "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water")
#' my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01",
#'                  "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01",
#'                  "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01",
#'                  "2016-09-01")
#'
#' a <- lucC_pred_holds(raster_obj = rb_class, raster_class = "Forest",
#'                      time_interval = c("2001-09-01","2008-09-01"),
#'                      relation_interval = "contains", label = my_label,
#'                      timeline = my_timeline)
#' head(a)
#'
#' # remove columns
#' lucC_remove_columns(data_mtx = a, name_columns = c("2003-09-01"))
#'
#'
#'}
#'

# plot maps with events
lucC_remove_columns <- function(data_mtx = NULL, name_columns = NULL){

  # Ensure if parameters exists
  # ensurer::ensure_that(data_mtx, !is.null(data_mtx),
  #                      err_desc = "data_mtx matrix, file must be defined!\nThis data can be obtained using predicates RECUR, HOLDS, EVOLVE and CONVERT.")
  ensurer::ensure_that(name_columns, !is.null(name_columns),
                       err_desc = "name_columns must be defined! Enter names of columns to remove!")
  #
  data_mtx <- as.data.frame(data_mtx)
  message(paste(c("Columns removed: ", as.character(name_columns), "\t"), collapse="\n"))

  if (!is.null(data_mtx)){
    # columns to delete
    drops <- as.character(name_columns)
    data_mtx_new <- data_mtx[ , !(names(data_mtx) %in% drops)]
    return(data_mtx_new)
  } else {
    message(paste(c("Data set is empty! ", as.character(name_columns), "\t"), collapse="\n"))
    data_mtx_new <- data_mtx
    return(data_mtx_new)
  }

}


#' @title Select columns of a data set
#' @name lucC_select_columns
#' @aliases lucC_select_columns
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Remove columns from a data_mtx
#'
#' @usage lucC_select_columns(data_mtx = NULL, name_columns = NULL)
#'
#' @param data_mtx      Matrix. A matrix with values obtained from predicates RECUR, EVOLVE, CONVERT or HOLDS
#' @param name_columns  Character. Name of columns to remove from data set
#'
#' @keywords datasets
#' @return Matrix with without selected columns
#' @importFrom ensurer ensure_that
#' @export
#'
#' @examples \dontrun{
#' library(lucCalculus)
#'
#' file <- c(system.file("extdata/raster/rasterSample.tif", package = "lucCalculus"))
#' rb_class <- raster::brick(file)
#' my_label <- c("Degradation", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
#'               "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water")
#' my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01",
#'                  "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01",
#'                  "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01",
#'                  "2016-09-01")
#'
#' a <- lucC_pred_holds(raster_obj = rb_class, raster_class = "Forest",
#'                      time_interval = c("2001-09-01","2008-09-01"),
#'                      relation_interval = "contains", label = my_label,
#'                      timeline = my_timeline)
#' head(a)
#'
#' # remove columns
#' lucC_select_columns(data_mtx = a, name_columns = c("2003-09-01"))
#'
#'
#'}
#'

# plot maps with events
lucC_select_columns <- function(data_mtx = NULL, name_columns = NULL){

  # Ensure if parameters exists
  ensurer::ensure_that(data_mtx, !is.null(data_mtx),
                       err_desc = "data_mtx matrix, file must be defined!\nThis data can be obtained using predicates RECUR, HOLDS, EVOLVE and CONVERT.")
  ensurer::ensure_that(name_columns, !is.null(name_columns),
                       err_desc = "name_columns must be defined! Enter names of columns to select!")
  #
  data_mtx <- as.data.frame(data_mtx)
  message(paste(c("Columns selected: ", as.character(name_columns), "\t"), collapse="\n"))

  # columns to delete
  holds <- c("x", "y", as.character(name_columns))
  data_mtx_new <- data_mtx[ , (names(data_mtx) %in% holds)]

  return(data_mtx_new)

}
