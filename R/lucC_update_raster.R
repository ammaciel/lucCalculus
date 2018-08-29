#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to replace pixels in a ReasterBrick              ##
##                                                             ##
##                                             2018-08-28      ##
##                                                             ##
##                                                             ##
#################################################################

#' @title Update a RasterBrick with pixel replaced
#' @name lucC_raster_update
#' @aliases lucC_raster_update
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Update a RasterBrick with new values of pixel discovered from LUC Calculus formalism
#'
#' @usage lucC_raster_update(raster_obj = NULL, data_mtx = NULL,
#' timeline = NULL, class_to_replace = NULL, new_pixel_value = 20)
#'
#' @param raster_obj       Raster. A raster brick with classified images
#' @param data_mtx         Matrix. A matrix with values obtained from predicates RECUR, EVOLVE, CONVERT or HOLDS
#' @param timeline         Character. A list of all dates of classified raster, timeline
#' @param class_to_replace Character. All labels of each value of pixel from classified raster
#' @param new_pixel_value  Integer. New pixel value to raster. Default is 20
#'
#' @keywords datasets
#' @return Matrix with raster and new pixel to create a RasterBrick reclassified
#' @importFrom ensurer ensure_that
#' @importFrom lubridate year
#' @importFrom dplyr mutate select
#' @importFrom tidyr gather spread
#' @importFrom raster rasterToPoints
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
#' a <- lucC_pred_recur(raster_obj = rb_class, raster_class = "Forest",
#'                      time_interval1 = c("2001-09-01","2001-09-01"),
#'                      time_interval2 = c("2002-09-01","2016-09-01"),
#'                      label = my_label, timeline = my_timeline)
#'
#' # update original RasterBrick with new class
#' num_label <- length(my_label) + 1
#' rb_class_new <- lucC_raster_update(raster_obj = rb_class,
#'                                    data_mtx = a,
#'                                    timeline = my_timeline,
#'                                    class_to_replace = "Forest",  # the same class previously
#'                                    new_pixel_value = num_label)  # new pixel value
#'
#' lucC_plot_bar_events(data_mtx = rb_class_new, pixel_resolution = 232, custom_palette = FALSE)
#'
#'}
#'

# update pixel in maps
lucC_raster_update <- function(raster_obj = NULL, data_mtx = NULL, timeline = NULL, class_to_replace = NULL, new_pixel_value = 20) {

  # Ensure if parameters exists
  ensurer::ensure_that(raster_obj, !is.null(raster_obj),
                       err_desc = "raster_obj data, file must be defined!\nA raster brick with classified images.")
  ensurer::ensure_that(data_mtx, !is.null(data_mtx),
                       err_desc = "data_mtx matrix, file must be defined!\nThis data can be obtained using predicates RECUR, HOLDS, EVOLVE and CONVERT.")
  ensurer::ensure_that(timeline, !is.null(timeline),
                       err_desc = "timeline must be defined!")
  ensurer::ensure_that(class_to_replace, !is.null(class_to_replace),
                       err_desc = "class_to_replace must be defined!")

  options(digits = 12)

  #-------------------- prepare rasterBrick --------------------------------
  df <- raster::rasterToPoints(raster_obj) %>%
    as.data.frame()

  rm(raster_obj)
  gc()

  # make column headings
  colnames(df) <- c("x", "y")

  # replace colnames to timeline
  colnames(df)[c(3:ncol(df))] <- as.character(lubridate::year(timeline))
  #raster_df <- reshape2::melt(as.data.frame(df), id.vars = c("x","y"))
  raster_df <- df %>%
    tidyr::gather(variable, value, -x, -y)

  rm(df)
  gc()

  #-------------------- prepare matrix with events --------------------------------
  data_mtx <- as.data.frame(data_mtx)
  data_mtx$x <- as.factor(data_mtx$x)
  data_mtx$y <- as.factor(data_mtx$y)

  # data matrix to new raster
  new_df <- data_mtx
  colnames(new_df)[c(3:ncol(new_df))] <- as.character(lubridate::year(colnames(new_df)[c(3:ncol(new_df))]))

  rm(data_mtx)
  gc()

  # replace new clase by new pixel value
  new_df[c(3:ncol(new_df))] <- ifelse(new_df[c(3:ncol(new_df))] == class_to_replace, new_pixel_value, "")

  #points_df <- reshape2::melt(new_df, id.vars = c("x","y"), na.rm = TRUE)
  points_df <- new_df %>%
    tidyr::gather(variable, value, -x, -y, na.rm = TRUE)

  rm(new_df)
  gc()

  # remove factor
  points_df$x = as.numeric(as.character(points_df$x)) #as.numeric(levels(points_df$x))[points_df$x]
  points_df$y = as.numeric(as.character(points_df$y))

  # ------------------ replace points_df in raster_df ---------------------
  a <- as.matrix(raster_df)
  b <- as.matrix(points_df)

  rm(raster_df, points_df)
  gc()

  # change original by new values - ok
  rows_both <- base::merge(a, b, by = c("x","y","variable"))
  rows_both[,] <- lapply(rows_both, function(x) {as.numeric(as.character(x))}) # remove factor

  rm(b)
  gc()

  rows_both2 <- rows_both %>%
    dplyr::mutate(value = .$value.y) %>%
    dplyr::select(-value.x, -value.y) %>%
    .[order(.$variable),] %>%
    as.matrix()

  # remove duplicated lines
  rows_both2 <- rows_both2[!duplicated(rows_both2), ]
  a <- as.data.frame(a)
  a[,] <- lapply(a, function(x) {as.numeric(as.character(x))}) # remove factor
  b <- as.data.frame(rows_both2)

  # replace in entire raster
  raster_rows_both <- merge(a, b, by = c("x" = "x", "y" = "y", "variable" = "variable"), all.x = TRUE)
  raster_rows_both[,] <- lapply(raster_rows_both, function(x) {as.numeric(as.character(x))}) # remove factor

  rm(a, b, rows_both, rows_both2)
  gc()

  raster_rows_both <- raster_rows_both %>%
    dplyr::mutate(value = ifelse(is.na(.$value.y), .$value.x, .$value.y)) %>%
    dplyr::select(-value.x, -value.y) %>%
    .[order(.$variable),]

  # remove duplicated lines
  raster_rows_both <- raster_rows_both[!duplicated(raster_rows_both), ]

  #raster_df_update <- reshape2::dcast(raster_rows_both, x+y ~ variable, value.var= "value")
  raster_df_update <- raster_rows_both %>%
    tidyr::spread(variable, value)

  colnames(raster_df_update)[c(3:ncol(raster_df_update))] <- as.character(timeline)

  rm(raster_rows_both)
  gc()

  return(raster_df_update)

}

