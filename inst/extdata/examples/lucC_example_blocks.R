# install package from github repository
#devtools::install_github("e-sensing/lucCalculus")

# load library
library(lucCalculus)

# define raster brick
file <- system.file("extdata/raster/rasterItanhanga.tif", package = "lucCalculus")

# load raster brick with classified images
rb_class <- raster::brick(file)

# define timeline for each classified image
my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01",
                 "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01",
                 "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01",
                 "2016-09-01")

# define a set of labels for each class, or pixel number, of the images
my_label <- c("Degradation", "Double_cropping", "Single_cropping", "Forest", "Pasture",
              "Pasture", "Pasture", "Double_cropping", "Double_cropping",
              "Double_cropping", "Double_cropping", "Double_cropping",
              "Single_cropping", "Single_cropping", "Water", "Water")

# plot raster brick
lucC_plot_raster(raster_obj = rb_class, timeline = my_timeline, label = my_label,
                 custom_palette = FALSE, plot_ncol = 4)

# run predicate RECUR on a raster brick to discover all 'Forest' class
# that appear in a second time interval in non-consecutive
forest_recur <- lucC_pred_recur(raster_obj = rb_class, raster_class = "Forest",
                                time_interval1 = c("2001-09-01","2001-09-01"),
                                time_interval2 = c("2002-09-01","2016-09-01"),
                                label = my_label, timeline = my_timeline)

# leaves only forest that recur after 2001
forest_recur <- forest_recur %>%
  lucC_remove_columns(data_mtx = ., name_columns = c("2001-09-01"))

# plot forest_recur over raster
lucC_plot_raster_result(raster_obj = rb_class, data_mtx = forest_recur,
                        timeline = my_timeline, label = my_label, plot_ncol = 4,
                        custom_palette = FALSE, shape_point = ".")


#--------------------------------------------
# a function to divide in convenient blocksizes with respect to memory
bs <- raster::blockSize(rb_class)
# a fucntion to apply the recur function to each bloc
processBlock <- function(i){
  # print(paste("Iteration", i))
  dumBrick <- raster::crop(rb_class, raster::extent(rb_class, bs$row[i], bs$row[i]+bs$nrows[i]-1))
  forest_recur2 <- lucC_pred_recur(raster_obj = dumBrick,
                                   raster_class = "Forest",
                                   time_interval1 = c("2001-09-01","2001-09-01"),
                                   time_interval2 = c("2002-09-01","2016-09-01"),
                                   label = my_label, timeline = my_timeline)

  return(forest_recur2)
}

# Apply using parallel (or simply lapply)
system.time(outList <- parallel::mclapply(X = 1:bs$n, FUN = processBlock, mc.cores = 3))

# merge all blocks together
snr <- do.call(rbind, outList)

# and plot the result
lucC_plot_raster_result(raster_obj = rb_class, data_mtx = snr,
                        timeline = my_timeline, label = my_label, plot_ncol = 4,
                        custom_palette = FALSE, shape_point = ".")

# create images
for (i in 1:length(outList)) {
  # message("Prepare image 2 ...\n")
  lucC_save_raster_result(raster_obj = rb_class, data_mtx = outList[[i]], timeline = my_timeline, label = my_label,
                          path_raster_folder = paste("/home/inpe/Desktop/TESTE/block_", i,"/", sep=""), as_RasterBrick = TRUE)

  # message("Prepare image 2 ...\n")
  lucC_save_raster_result(raster_obj = rb_class, data_mtx = outList[[i]], timeline = my_timeline, label = my_label,
                          path_raster_folder = paste("/home/inpe/Desktop/TESTE/block_", i, "/", sep=""), as_RasterBrick = FALSE)

  cat("---------------\n")
}





#--------------------------------------------
# load library
library(lucCalculus)

# define raster brick
file <- "~/Desktop/A_MT_Sec_Cerrado_15classes.tif"

# load raster brick with classified images
rb_class <- raster::brick(file)

# define timeline for each classified image
my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2005-09-01",
                 "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01",
                 "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01",
                 "2014-09-01", "2015-09-01", "2016-09-01")

# define a set of labels for each class, or pixel number, of the images
my_label <- c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy", "Soy",
              "Soy", "Soy", "Soy", "Sugarcane", "Urban_Area", "Water",
              "Secondary_Vegetation", "Degradation", "Secondary_Cerrado")

raster::plot(rb_class)

# # plot raster brick
#lucC_plot_raster(raster_obj = rb_class, timeline = my_timeline, label = my_label,
#                 custom_palette = FALSE, plot_ncol = 4)

#--------------------------------------------
# a function to divide in convenient blocksizes with respect to memory
bs <- raster::blockSize(rb_class)

#--------------------------------------------
# Mato Grosso test
# a fucntion to apply the recur function to each bloc
processBlock <- function(i){
  #i=1
  #    print(paste("Iteration", i))
  dumBrick <- raster::crop(rb_class, raster::extent(rb_class, bs$row[i], bs$row[i]+bs$nrows[i]-1, 1)) #, dim(rb_class)[2]))

  class1 <- c("Forest")
  classes <- c("Pasture", "Soy", "Cerrado", "Degradation", "Secondary_Vegetation", "Secondary_Cerrado") #

  direct_transi.df <- NULL

  message("Start Convert ...\n")
  # along of all classes
  # system.time(
  for(x in 2:length(timeline)){
    t_1 <- timeline[x-1]
    t_2 <- timeline[x]
    cat(paste0("--", t_1, ", ", t_2, sep = ""), "\n")

    # moves across all classes
    for(i in seq_along(classes)){
      cat(classes[i], collapse = " ", "\n")
      temp <- lucC_pred_convert(raster_obj = dumBrick, raster_class1 = class1,
                                time_interval1 = c(t_1,t_1), relation_interval1 = "equals",
                                raster_class2 = classes[i],
                                time_interval2 = c(t_2,t_2), relation_interval2 = "equals",
                                label = label, timeline = timeline)

      direct_transi.df <- lucC_merge(direct_transi.df, temp)
    }
    cat("\n")
  }

  return(direct_transi.df)
}


# Apply using parallel (or simply lapply)
system.time(outList <- parallel::mclapply(X = 1:bs$n, FUN = processBlock, mc.cores = 3))

# merge all blocks together
snr <- do.call(rbind, outList)


# create images
for (i in 1:length(outList)) {
  # message("Prepare image 2 ...\n")
  lucC_save_raster_result(raster_obj = rb_class, data_mtx = outList[[i]], timeline = my_timeline, label = my_label,
                          path_raster_folder = paste("/home/inpe/Desktop/TESTE/block_", i,"/", sep=""), as_RasterBrick = TRUE)

  # message("Prepare image 2 ...\n")
  lucC_save_raster_result(raster_obj = rb_class, data_mtx = outList[[i]], timeline = my_timeline, label = my_label,
                          path_raster_folder = paste("/home/inpe/Desktop/TESTE/block_", i, "/", sep=""), as_RasterBrick = FALSE)

  cat("---------------\n")
}


