#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##  R script to split rasterBrick in blocks in accordance      ##
##  with the number of cells                                   ##
##                                                             ##
##                                             2018-08-28      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title Create blocks from RasterBrick in accordance with number of cells
#' @name lucC_blocks_raster_create
#' @aliases lucC_blocks_raster_create
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a data.frame with spatially aggregates the original raster it turns each aggregated cell into a polygon then the extent of each polygon is used to crop the original raster. \url{https://stackoverflow.com/questions/29784829/r-raster-package-split-image-into-multiples}
#' @usage lucC_blocks_raster_create (raster_obj = NULL, number_blocks_xy = 6,
#' path_save_RasterBlocks = NULL)
#'
#' @param raster_obj        Raster. A raster stack with classified images
#' @param number_blocks_xy  Numeric. A number of peaces to split raster, consider 2 peaces the raster will be cropped into 4 blocks. Default is 6
# @param save_images       Boolean. Write raster in file. Default is TRUE
#' @param path_save_RasterBlocks  Character. Name a path folder to SAVE RasterBlocks from GeoTIFF images. If  doesn't exist, a new directory is created
#'
#' @keywords datasets
#' @return List with all the pieces in case you want to keep them in the memory.
#' @importFrom ensurer ensure_that
#' @importFrom raster aggregate ncell rasterToPolygons extent crop writeRaster
#' @export
#'
#' @examples \dontrun{
#' library(lucCalculus)
#'
#' file <- c(system.file("extdata/raster/rasterItanhanga.tif", package = "lucCalculus"))
#' rb_class <- raster::brick(file)
#'
#' # blocks saved in folder
#' lucC_blocks_raster_create(raster_obj = rb_class, number_blocks_xy = 2,
#' path_save_RasterBlocks = NULL)
#'
#'
#'}
#'

lucC_blocks_raster_create <- function(raster_obj = NULL, number_blocks_xy = 6, path_save_RasterBlocks = NULL){

  # Ensure if parameters exists
  ensurer::ensure_that(raster_obj, !is.null(raster_obj), err_desc = "raster_obj rasterBrick must be defined!\n")
  ensurer::ensure_that(number_blocks_xy, !is.null(number_blocks_xy),
                       err_desc = "number_blocks_xy must be defined! Default is 6 by x and y, or 36 blocks\n")

  hori <- ceiling(ncol(raster_obj)/number_blocks_xy)
  vert <- ceiling(nrow(raster_obj)/number_blocks_xy)
  aggrega <- raster::aggregate(raster_obj, fact = c(hori,vert))
  aggrega[] <- 1:raster::ncell(aggrega)
  aggrega_poly <- raster::rasterToPolygons(aggrega)
  names(aggrega_poly) <- "polis"
  raster_list <- list()

  # crop original raster by extent from poligon
  for(i in 1:raster::ncell(aggrega)){
    ext <- raster::extent(aggrega_poly[aggrega_poly$polis==i,])
    raster_list[[i]] <- raster::crop(raster_obj, ext)
  }

  if (!is.null(path_save_RasterBlocks)){
    path <- file.path(paste0(path_save_RasterBlocks, "/Blocks_RasterBrick", sep = ""))
  }else{
    path <- file.path(paste0(getwd(), "/Blocks_RasterBrick", sep = ""))
  }

  # Create directory if doesn't exist
  if (dir.exists(path)){
    message("\nThis directory already exist, data will be overwrited! \n")
    path <- path
  } else {
    dir.create(path)
    path_raster_folder <- path
  }

  message("Saving... \n")

  # save images in directory
  for(i in 1:length(raster_list)){
    raster::writeRaster(raster_list[[i]], filename=paste0(path, "/Raster_Block_", i, sep=""),
                              format="GTiff", datatype="INT1U", overwrite=TRUE)
  }


  message("\nRaster splitted in ", length(raster_list), " blocks saved in path ", path, "\n")

}



#' @title Merge blocks of RasterBrick or Rasters in accordance with number of blocks
#' @name lucC_blocks_raster_merge
#' @aliases lucC_blocks_raster_merge
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Merge GeoTIFF splitted into parts. \url{https://stackoverflow.com/questions/29784829/r-raster-package-split-image-into-multiples}
#' @usage lucC_blocks_raster_merge (path_open_GeoTIFFs = NULL, number_raster = 4,
#' pattern_name = NULL, is.rasterBrick = FALSE)
#'
#' @param path_open_GeoTIFFs   Character. Name a path folder to OPEN raster images data.
#' @param number_raster        Integer. Number of GeoTIFF files.
#' @param pattern_name         Character. A pattern in name of GeoTIFF to mosaic them
#' @param is.rasterBrick       Boolean. If TRUE GeoTIFF is a RasterBrick, FALSE is a single layer. Default is FALSE
#'
#' @keywords datasets
#' @return RasterBrick Mosaic.
#' @importFrom ensurer ensure_that
#' @importFrom raster brick mosaic writeRaster
#' @export
#'
#' @examples \dontrun{
#' library(lucCalculus)
#'
#' file <- c(system.file("extdata/raster/rasterItanhanga.tif", package = "lucCalculus"))
#' rb_class <- raster::brick(file)
#'
#' # blocks saved in folder
#' lucC_blocks_raster_create(raster_obj = rb_class, number_blocks_xy = 2, save_images = TRUE)
#'
#' lucC_blocks_raster_merge(path_open_GeoTIFFs = paste0(getwd(), "/Blocks_RasterBrick", sep = ""),
#'                          number_raster = 4, pattern_name = "Raster_Block_", is.rasterBrick = TRUE)
#'
#'}
#'

lucC_blocks_raster_merge <- function(path_open_GeoTIFFs = NULL, number_raster = 4, pattern_name = NULL, is.rasterBrick = FALSE){

 # Ensure if parameters exists
  ensurer::ensure_that(path_open_GeoTIFFs, !is.null(path_open_GeoTIFFs),
                       err_desc = "path_open_GeoTIFFs file, must be defined, without last /! Enter a path to OPEN your GeoTIFF images.")

  message("Verifying if GeoTIFF image exist ...")
  # all files in folder
  all.files <- list.files(path_open_GeoTIFFs, full.names = TRUE, pattern = paste0(pattern_name, "[0-9]\\.tif$", sep = ""))
  if( length(all.files) > 0){
    cat(all.files, sep = "\n")
  } else
    stop("There is no path or pattern_name file!\n")

  # read each piece back in R
  list <- list()
  if(isTRUE(is.rasterBrick)){
    for(i in 1:number_raster){ # change this 9 depending on your number of pieces
      rx <- raster::brick(paste0(path_open_GeoTIFFs,"/", pattern_name, i,".tif", sep=""))
      list[[i]] <- rx
    }
  } else if(is.rasterBrick == FALSE){
    for(i in 1:number_raster){ # change this 9 depending on your number of pieces
      rx <- raster::raster(paste0(path_open_GeoTIFFs,"/", pattern_name, i,".tif", sep=""))
      list[[i]] <- rx
    }
  }

  message("\nStart merge rasters ...\n\n")
  # mosaic them and save output
  list$fun <- max
  rast.mosaic <- do.call(raster::mosaic, list)

  message("Save merged raster ...\n")
  raster::writeRaster(rast.mosaic, filename = paste0(path_open_GeoTIFFs,"/Mosaic_", pattern_name, sep=""),
              format="GTiff", datatype="INT1U", overwrite=TRUE)

  message("\nMosaic finished!\n")
}





