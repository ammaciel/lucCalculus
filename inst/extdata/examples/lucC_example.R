library(lucCalculus)

#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata with SITS
#----------------------------

# create a RasterBrick from individual raster GeoTIFF classified previously
lucC_create_RasterBrick(path_open_GeoTIFFs = c(system.file("extdata/raster/rasterSample", package = "lucCalculus")),
                        path_save_RasterBrick = getwd())


# ------------- define variables to use in sits -------------
# open files
file <- paste0(getwd(),"/rasterSample.tif", sep = "")

# create timeline with classified data
my_timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

# raster object
rb_class <- raster::brick(file)

# ------------- define variables to plot raster -------------
# original label - see QML file, same order
my_label <- as.character(c("Degradation", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
                        "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water"))

# original colors set - see QML file, same order
my_colors <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6",
            "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1" )

# plot rasterBrick
lucC_plot_raster(raster_obj = rb_class,
                 timeline = my_timeline, label = my_label,
                 custom_palette = TRUE, RGB_color = my_colors, plot_ncol = 4)


#----------------------------
# 2- LUC Calculus with Allen's interval relations
#----------------------------

#------------- tests - intervals before, meets and follows -- Allen's relations
a <- lucC_pred_holds(raster_obj = rb_class, raster_class = "Forest",
                     time_interval = c("2001-09-01","2007-09-01"),
                     relation_interval = "equals", label = my_label, timeline = my_timeline)
a


b <- lucC_pred_holds(raster_obj = rb_class, raster_class = "Degradation",
                     time_interval = c("2012-09-01","2013-09-01"),
                     relation_interval = "contains", label = my_label, timeline = my_timeline)
b

# before
c <- lucC_relation_before(a, b)
#c <- lucC_relation_after(b, a)
#c <- lucC_relation_meets(a, b)
#c <- lucC_relation_met_by(b, a)
#c <- lucC_relation_starts(a, b)
#c <- lucC_relation_started_by(b, a)
#c <- lucC_relation_finishes(b, a)
#c <- lucC_relation_finished_by(a, b)
#c <- lucC_relation_during(a, b)
#c <- lucC_relation_equals(a, b)

lucC_plot_sequence_events(c, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(c, custom_palette = FALSE, pixel_resolution = 232, side_by_side = TRUE, legend_text = "Legend")

lucC_plot_raster_result(raster_obj = rb_class, data_mtx = c, timeline = my_timeline,
                        label = my_label, custom_palette = TRUE,
                        RGB_color = my_colors, relabel = FALSE) #, shape_point = "#")


#----------------------------
# 3- LUC Calculus - verify for secondary vegetation
#----------------------------
# 1. RECUR predicate indicates a class that appear again
forest_recur <- lucC_pred_recur(raster_obj = rb_class, raster_class = "Forest",
                                    time_interval1 = c("2001-09-01","2001-09-01"),
                                    time_interval2 = c("2003-09-01","2016-09-01"),
                                    label = my_label, timeline = my_timeline)
head(forest_recur)

#-------------------
# plot some results from RECUR
lucC_plot_sequence_events(forest_recur, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(forest_recur, custom_palette = FALSE, legend_text = "Legend:")

lucC_plot_raster_result(raster_obj = rb_class, data_mtx = forest_recur,
                        timeline = my_timeline, label = my_label, custom_palette = TRUE,
                        RGB_color = my_colors, relabel = FALSE) #, shape_point = "#")
#-------------------

# 2. EVOLVE to verify Forest class that occurs after a different class in 2001
forest_evolve <- NULL

# classes without Forest based on original label
classes <- as.character(c("Degradation", "Fallow_Cotton", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water"))

system.time(
# percor all classes
for(i in seq_along(classes)){
  print(classes[i])
  temp <- lucC_pred_evolve(raster_obj = rb_class, raster_class1 = classes[i],
                           time_interval1 = c("2001-09-01","2001-09-01"), relation_interval1 = "equals",
                           raster_class2 = "Forest",
                           time_interval2 = c("2002-09-01","2016-09-01"), relation_interval2 = "contains",
                           label = my_label, timeline = my_timeline)

  forest_evolve <- lucC_merge(forest_evolve, temp)
}
)
rm(temp, i)
gc()
#df <- forest_evolve
#-------------------
# plot some results from EVOLVE
lucC_plot_sequence_events(forest_evolve, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(forest_evolve, custom_palette = FALSE, legend_text = "Legend:")

lucC_plot_raster_result(raster_obj = rb_class, data_mtx = forest_evolve,
                        timeline = my_timeline, label = my_label, custom_palette = TRUE,
                        RGB_color = my_colors, relabel = FALSE) #, shape_point = "#")
#-------------------

# 3. Merge both forest_recur and forest_evolve datas
system.time(forest_secondary <- lucC_merge(forest_evolve, forest_recur))
head(forest_secondary)

# plot
lucC_plot_bar_events(forest_secondary, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

#rm(forest_recur, forest_evolve)

# plot
lucC_plot_bar_events(forest_secondary, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 4. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
lucC_plot_raster_result(raster_obj = rb_class,
                        data_mtx = forest_secondary, #forest_sec,
                        timeline = my_timeline,
                        label = my_label, custom_palette = TRUE,
                        RGB_color = my_colors, relabel = FALSE) #, shape_point = ".")

# create images output
lucC_save_raster_result(raster_obj = rb_class,
                        data_mtx = forest_secondary,       # without 2001
                        timeline = my_timeline, label = my_label,
                        path_raster_folder = getwd())         # new pixel value


#----------------------------
# 4 - Update original raster to add new pixel value
#----------------------------

num_label <- length(my_label) + 1
# 1. update original RasterBrick with new class
rb_sits_new <- lucC_raster_update(raster_obj = rb_class,
                                  data_mtx = forest_secondary,       # without 2001
                                  timeline = my_timeline,
                                  class_to_replace = "Forest",  # only class Forest
                                  new_pixel_value = num_label)         # new pixel value

head(rb_sits_new)

lucC_plot_bar_events(data_mtx = rb_sits_new, pixel_resolution = 232, custom_palette = FALSE)

# 2. save the update matrix as GeoTIFF RasterBrick
lucC_save_GeoTIFF(raster_obj = rb_class,
                  data_mtx = rb_sits_new,
                  path_raster_folder = paste0(getwd(),"/raster_sampleSecVeg", sep = ""),
                  as_RasterBrick = FALSE )

#------------
# create a RasterBrick from individual raster GeoTIFF, case saved as separate layers
lucC_create_RasterBrick(path_open_GeoTIFFs = paste0(getwd(),"/raster_sampleSecVeg", sep = ""),
                        path_save_RasterBrick = getwd())

# open files
file <- paste0(getwd(),"/raster_sampleSecVeg.tif", sep = "")

# create timeline with classified data from SVM method
my_timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

# new variable with raster object
rb_class2 <- raster::brick(file)

# ------------- define variables to plot raster -------------
# original label - see QML file, same order
my_label2 <- as.character(c("Degradation", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water", "Secondary_Vegetation"))

# original colors set - see QML file, same order
my_colors2 <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1", "red" )

# plot rasterBrick
lucC_plot_raster(raster_obj = rb_class2,
                 timeline = my_timeline, label = my_label2,
                 custom_palette = TRUE, RGB_color = my_colors2, plot_ncol = 6)


#----------------------------
# 5- Discover Forest and Secondary vegetation - LUC Calculus
#----------------------------

secondary.mtx <- lucC_pred_holds(raster_obj = rb_class2, raster_class = "Secondary_Vegetation",
                                 time_interval = c("2001-09-01","2016-09-01"),
                                 relation_interval = "contains", label = my_label2, timeline = my_timeline)
head(secondary.mtx)

forest.mtx <- lucC_pred_holds(raster_obj = rb_class2, raster_class = "Forest",
                              time_interval = c("2001-09-01","2016-09-01"),
                              relation_interval = "contains", label = my_label2, timeline = my_timeline)
head(forest.mtx)

Forest_secondary.mtx <- lucC_merge(secondary.mtx, forest.mtx)
head(Forest_secondary.mtx)

# plot results
lucC_plot_bar_events(data_mtx = Forest_secondary.mtx,
                     pixel_resolution = 232, custom_palette = FALSE, side_by_side = TRUE)

# Compute values
measuresFor_Sec <- lucC_result_measures(data_mtx = Forest_secondary.mtx, pixel_resolution = 232)
measuresFor_Sec


gc()
