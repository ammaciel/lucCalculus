library(lucCalculus)

# always
options(digits = 12)

#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata with SITS
#----------------------------

# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = c(system.file("extdata/raster/rasterItanhanga", package = "lucCalculus")),
                        path_save_RasterBrick = getwd())

# ------------- define variables to use in sits -------------
# open files
file <- paste0(getwd(),"/rasterItanhanga.tif")
file

# create timeline with classified data from SVM method
timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01")
timeline

# new variable
rb_class <- raster::brick(file)
rb_class

# ------------- define variables to plot raster -------------
# original label - see QML file, same order
label <- c("Degradation", "Double_cropping", "Single_cropping", "Forest", "Pasture", "Pasture", "Pasture", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Single_cropping", "Single_cropping", "Water", "Water")
label

# colors
colors_1 <- c("#BEEE53", "#cd6155", "#e6b0aa", "#228b22", "#7ecfa4", "#afe3c8",  "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff")
colors_1

# plot raster brick
lucC_plot_raster(raster_obj = rb_class,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 5)

# rb_class
# layers <- c(1, 3, 5, 7, 9, 11, 13, 15)
# rb_class_2years <- raster::subset(rb_class, layers)
# rb_class_2years
#
# # create timeline with classified data from SVM method
# timeline_n <- c("2001-09-01", "2003-09-01", "2005-09-01", "2007-09-01", "2009-09-01", "2011-09-01", "2013-09-01", "2015-09-01")
# timeline_n
#
# # png(filename = "~/Desktop/fig_TESE/fig_ita_land_use2D.png", width = 6.7, height = 5.4, units = 'in', res = 300)
# lucC_plot_raster(raster_obj = rb_class_2years,
#                  timeline = timeline_n, label = label,
#                  custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 3,
#                  relabel = TRUE, original_labels = c("Degradation", "Double_cropping", "Single_cropping", "Forest", "Pasture"), new_labels =  c("Degradation", "Double cropping", "Single cropping", "Forest", "Pasture") )
# # dev.off()

#----------------------------
# IJGIS
#----------------------------
# 1- Discover Forest Recur its natural vegetation - LUC Calculus
#----------------------------

system.time(
  forest_recur <- lucC_pred_recur(raster_obj = rb_class, raster_class = "Forest",
                                  time_interval1 = c("2001-09-01","2001-09-01"),
                                  time_interval2 = c("2002-09-01","2016-09-01"),
                                  label = label, timeline = timeline, remove_column = FALSE)
)

head(forest_recur)

lucC_plot_bar_events(forest_recur, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")


# 4. Remove column 2001 because it' is not used to replace pixels's only support column
forest_re <- lucC_remove_columns(data_mtx = forest_recur, name_columns = c("2001-09-01"))
head(forest_re)

lucC_plot_bar_events(forest_re, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
lucC_plot_raster_result(raster_obj = rb_class,
                        data_mtx = forest_re,
                        timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE, shape_point = ".")
# Save results
# create images output
# lucC_save_raster_result(raster_obj = rb_class,
#                         data_mtx = forest_re,       # without 2001
#                         timeline = timeline, label = label, path_raster_folder = "~/Desktop/rasterItanhanga_RECUR", as_RasterBrick = FALSE)


#----------------------------
# 2. Verify if occur forest EVOLVE from a different class in 2001
#----------------------------

forest_evolve <- NULL
# classes without Forest
#classes <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask"))
classes <- c("Degradation", "Double_cropping", "Single_cropping", "Pasture", "Pasture", "Pasture", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Single_cropping", "Single_cropping", "Water", "Water")

# percor all classes
system.time(
  for(i in seq_along(classes)){
    print(classes[i])
    temp <- lucC_pred_evolve(raster_obj = rb_class, raster_class1 = classes[i],
                             time_interval1 = c("2001-09-01","2001-09-01"), relation_interval1 = "equals",
                             raster_class2 = "Forest",
                             time_interval2 = c("2002-09-01","2016-09-01"), relation_interval2 = "contains",
                             label = label, timeline = timeline, remove_column = FALSE)

    forest_evolve <- lucC_merge(forest_evolve, temp)
  }
)

head(forest_evolve)

# 3. Merge both forest_recur and forest_evolve datas
forest_secondary <- lucC_merge(forest_evolve, forest_recur)
head(forest_secondary)

lucC_plot_bar_events(forest_secondary, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 4. Remove column 2001 because it' is not used to replace pixels's only support column
forest_sec <- lucC_remove_columns(data_mtx = forest_secondary, name_columns = c("2001-09-01"))
head(forest_sec)

lucC_plot_bar_events(forest_sec, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
lucC_plot_raster_result(raster_obj = rb_class,
                        data_mtx = forest_sec,
                        timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE, shape_point = ".")


# # create images output - only sec veg
# lucC_save_raster_result(raster_obj = rb_class,
#                         data_mtx = forest_sec,       # without 2001
#                         timeline = timeline, label = label, path_raster_folder = "inst/extdata/raster/rasterItanhangaSecVeg") # new pixel value

#----------------------------
# only recur and evolve single
forest_recur2 <- forest_recur
forest_evolve2 <- forest_evolve

# replace classes vy new value
forest_recur2[c(3:ncol(forest_recur2))] <- ifelse(forest_recur2[c(3:ncol(forest_recur2))] == "Forest", "Forest recurrence","")
forest_evolve2[c(3:ncol(forest_evolve2))] <- ifelse(forest_evolve2[c(3:ncol(forest_evolve2))] == "Forest", "Land use evolution","")

recur_evolve <- forest_evolve2 %>%
  lucC_merge(., forest_recur2) %>%
  lucC_remove_columns(.,name_columns = "2001-09-01")

lucC_plot_bar_events(recur_evolve, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:", side_by_side = TRUE)

#----------------------------
# 3- Update original raster to add new pixel value
#----------------------------

rm(forest_evolve, forest_recur, forest_secondary)
gc()

n_label <- length(label) + 1

# 1. update original RasterBrick with new class
rb_class_new <- lucC_raster_update(raster_obj = rb_class,
                                   data_mtx = forest_sec,       # without 2001
                                   timeline = timeline,
                                   class_to_replace = "Forest",  # only class Forest
                                   new_pixel_value = n_label)         # new pixel value

head(rb_class_new)

lucC_plot_bar_events(data_mtx = rb_class_new, pixel_resolution = 232, custom_palette = FALSE)

# 2. save the update matrix as GeoTIFF images
lucC_save_GeoTIFF(raster_obj = rb_class,
                  data_mtx = rb_class_new,
                  path_raster_folder = paste0(getwd(),"/rasterItanhangaSecVeg"),
                  as_RasterBrick = FALSE)
#path_raster_folder = "~/Desktop/rasterItanhangaSecVeg", as_RasterBrick = FALSE)



#----------------------------
# 4- Open idividual images reclassified and create a RasterBrick with each one and metadata ith SITS
#----------------------------

library(lucCalculus)

# always
options(digits = 12)

# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = paste0(getwd(),"/rasterItanhangaSecVeg"),
                        path_save_RasterBrick = getwd())

# ------------- define variables to use in sits -------------
# open files with new pixel secondary vegetation
file <- c(paste0(getwd(),"/rasterItanhangaSecVeg.tif"))
file

# create timeline with classified data from SVM method
timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01")
timeline

# new variable
rb_class2 <- raster::brick(file)
rb_class2

# new class Seconary vegetation
label2 <- c("Degradation", "Double_cropping", "Single_cropping", "Forest", "Pasture", "Pasture", "Pasture", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Single_cropping", "Single_cropping", "Water", "Water", "Secondary_vegetation")
label2

# colors
colors_2 <- c("#BEEE53" , "#cd6155", "#e6b0aa", "#228b22", "#7ecfa4", "#1e174d", "#afe3c8", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff") # "#b3cc33" "#228b22", "#7ecfa4", "blue"

# plot raster brick
lucC_plot_raster(raster_obj = rb_class2,
                 timeline = timeline, label = label2,
                 custom_palette = TRUE, RGB_color = colors_2, plot_ncol = 4)


#----------------------------
# 4.1- Only forest and secondary vegetation - LUC Calculus
#----------------------------

secondary.mtx <- lucC_pred_holds(raster_obj = rb_class2, raster_class = "Secondary_vegetation",
                                 time_interval = c("2001-09-01","2016-09-01"),
                                 relation_interval = "contains", label = label2, timeline = timeline)
head(secondary.mtx)

forest.mtx <- lucC_pred_holds(raster_obj = rb_class2, raster_class = "Forest",
                              time_interval = c("2001-09-01","2016-09-01"),
                              relation_interval = "contains", label = label2, timeline = timeline)
head(forest.mtx)

Forest_secondary.mtx <- lucC_merge(secondary.mtx, forest.mtx)
head(Forest_secondary.mtx)

# plot results
#png(filename = "~/Desktop/fig_TESE/ita_bar_for_SV.png", width = 6.5, height = 4.5, units = 'in', res = 300)
lucC_plot_bar_events(data_mtx = Forest_secondary.mtx, custom_palette = TRUE, RGB_color = c("black", "gray60"), #c("#228b22", "#7ecfa4"),
                     pixel_resolution = 231.656, side_by_side = TRUE,
                     relabel = TRUE, original_labels = c("Forest", "Secondary_vegetation"),
                     new_labels = c("Forest", "Secondary vegetation"))
# forest evolved and recur
#dev.off()


#----------------------------
# 5- Discover Land use transitions - LUC Calculus
#----------------------------
# create timeline with classified data from SVM method
timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01")
timeline

label2 <- c("Degradation", "Double_cropping", "Single_cropping", "Forest", "Pasture", "Pasture", "Pasture", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Single_cropping", "Single_cropping", "Water", "Water", "Secondary_vegetation")

class1 <- c("Forest")
classes <- c("Degradation", "Double_cropping", "Pasture", "Single_cropping") #

direct_transi.df <- NULL

# along of all classes
system.time(
  for(x in 2:length(timeline)){
    t_1 <- timeline[x-1]
    t_2 <- timeline[x]
    cat(paste0("--", t_1, ", ", t_2, sep = ""), "\n")

    # moves across all classes
    for(i in seq_along(classes)){
      cat(classes[i], collapse = " ", "\n")
      temp <- lucC_pred_convert(raster_obj = rb_class2, raster_class1 = class1,
                                time_interval1 = c(t_1,t_1), relation_interval1 = "equals",
                                raster_class2 = classes[i],
                                time_interval2 = c(t_2,t_2), relation_interval2 = "equals",
                                label = label2, timeline = timeline)
      direct_transi.df <- lucC_merge(direct_transi.df, temp)
    }
    cat("\n")
  }
)

Forest_others <- direct_transi.df
head(Forest_others)
str(Forest_others)

# plot results
lucC_plot_frequency_events(data_mtx = Forest_others,
                           pixel_resolution = 232, custom_palette = FALSE)

lucC_plot_bar_events(Forest_others, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:", side_by_side = FALSE)

# replace classes by new value
Forest_others[c(3:ncol(Forest_others))] <- ifelse(Forest_others[c(3:ncol(Forest_others))] == "Degradation", "Forest_Degradation",
                                                  ifelse(Forest_others[c(3:ncol(Forest_others))] == "Double_cropping", "Forest_Double_cropping",
                                                         ifelse(Forest_others[c(3:ncol(Forest_others))] == "Pasture", "Forest_Pasture",
                                                                ifelse(Forest_others[c(3:ncol(Forest_others))] == "Single_cropping", "Forest_Single_cropping"
                                                                       , ""))))

lucC_plot_bar_events(Forest_others, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:", side_by_side = FALSE)


#png(filename = "~/Desktop/fig_TESE/ch4_ita_bar_all.png", width = 7.5, height = 6, units = 'in', res = 300) # 3 columns to legend
lucC_plot_bar_events(Forest_others, pixel_resolution = 231.6465, custom_palette = TRUE, RGB_color = c("#6e8b3d", "#a2cd5a", "#006400", "#b4eeb4"), legend_text = "Land use transitions:", relabel = TRUE, original_labels = c("Forest_Degradation", "Forest_Double_cropping", "Forest_Pasture", "Forest_Single_cropping"), new_labels = c("Degradation", "Forest to Double Cropping", "Forest to Pasture", "Forest to Single Cropping"), side_by_side = FALSE)
#dev.off()


#--------------
class1 <- c("Pasture")
classes <- c("Double_cropping", "Secondary_vegetation", "Single_cropping") #

direct_transi.df <- NULL

# along of all classes
system.time(
  for(x in 2:length(timeline)){
    t_1 <- timeline[x-1]
    t_2 <- timeline[x]
    cat(paste0(t_1, ", ", t_2, sep = ""), "\n")

    # moves across all classes
    for(i in seq_along(classes)){
      cat(classes[i], collapse = " ", "\n")
      temp <- lucC_pred_convert(raster_obj = rb_class2, raster_class1 = class1,
                                time_interval1 = c(t_1,t_1), relation_interval1 = "equals",
                                raster_class2 = classes[i],
                                time_interval2 = c(t_2,t_2), relation_interval2 = "equals",
                                label = label2, timeline = timeline)

      if (!is.null(temp)) {
        tempP <- lucC_remove_columns(data_mtx = temp, name_columns = as.character(t_1))
      } else{
        tempP <- temp
      }

      direct_transi.df <- lucC_merge(direct_transi.df, tempP)
    }
    cat("\n")
  }
)

Pasture_others <- direct_transi.df
head(Pasture_others)
str(Pasture_others)

# plot results
lucC_plot_bar_events(Pasture_others, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:", side_by_side = FALSE)

# replace classes vy new value
Pasture_others[c(3:ncol(Pasture_others))] <- ifelse(Pasture_others[c(3:ncol(Pasture_others))] == "Double_cropping", "Pasture_Double_cropping",
                                                    ifelse(Pasture_others[c(3:ncol(Pasture_others))] == "Secondary_vegetation", "Pasture_Secondary_vegetation",
                                                           ifelse(Pasture_others[c(3:ncol(Pasture_others))] == "Single_cropping", "Pasture_Single_cropping"
                                                                  , "")))

lucC_plot_bar_events(Pasture_others, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:", side_by_side = FALSE)




#--------------
class1 <- c("Secondary_vegetation")
classes <- c("Double_cropping", "Pasture", "Single_cropping") #

direct_transi.df <- NULL

# along of all classes
system.time(
  for(x in 2:length(timeline)){
    t_1 <- timeline[x-1]
    t_2 <- timeline[x]
    cat(paste0(t_1, ", ", t_2, sep = ""), "\n")

    # moves across all classes
    for(i in seq_along(classes)){
      cat(classes[i], collapse = " ", "\n")
      temp <- lucC_pred_convert(raster_obj = rb_class2, raster_class1 = class1,
                                time_interval1 = c(t_1,t_1), relation_interval1 = "equals",
                                raster_class2 = classes[i],
                                time_interval2 = c(t_2,t_2), relation_interval2 = "equals",
                                label = label2, timeline = timeline)

      if (!is.null(temp)) {
        tempP <- lucC_remove_columns(data_mtx = temp, name_columns = as.character(t_1))
      } else{
        tempP <- temp
      }

      direct_transi.df <- lucC_merge(direct_transi.df, tempP)
    }
    cat("\n")
  }
)

SV_others <- direct_transi.df
head(SV_others)
str(SV_others)

# plot results
lucC_plot_bar_events(SV_others, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:", side_by_side = FALSE)

# replace classes vy new value
SV_others[c(3:ncol(SV_others))] <- ifelse(SV_others[c(3:ncol(SV_others))] == "Double_cropping", "SecV_Double_cropping",
                                          ifelse(SV_others[c(3:ncol(SV_others))] == "Pasture", "SecV_Pasture",
                                                 ifelse(SV_others[c(3:ncol(SV_others))] == "Single_cropping", "SecV_Single_cropping"
                                                        , "")))

lucC_plot_bar_events(SV_others, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:", side_by_side = FALSE)


#--------------
class1 <- c("Single_cropping")
classes <- c("Double_cropping") #

direct_transi.df <- NULL

# along of all classes
system.time(
  for(x in 2:length(timeline)){
    t_1 <- timeline[x-1]
    t_2 <- timeline[x]
    cat(paste0(t_1, ", ", t_2, sep = ""), "\n")

    # moves across all classes
    for(i in seq_along(classes)){
      cat(classes[i], collapse = " ", "\n")
      temp <- lucC_pred_convert(raster_obj = rb_class2, raster_class1 = class1,
                                time_interval1 = c(t_1,t_1), relation_interval1 = "equals",
                                raster_class2 = classes[i],
                                time_interval2 = c(t_2,t_2), relation_interval2 = "equals",
                                label = label2, timeline = timeline)

      if (!is.null(temp)) {
        tempP <- lucC_remove_columns(data_mtx = temp, name_columns = as.character(t_1))
      } else{
        tempP <- temp
      }

      direct_transi.df <- lucC_merge(direct_transi.df, tempP)
    }
    cat("\n")
  }
)

SC_DC <- direct_transi.df
head(SC_DC)
str(SC_DC)

# plot results
lucC_plot_bar_events(SC_DC, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:", side_by_side = FALSE)

# replace classes vy new value
SC_DC[c(3:ncol(SC_DC))] <- ifelse(SC_DC[c(3:ncol(SC_DC))] == "Double_cropping", "SingleC_Double_cropping","")

lucC_plot_bar_events(SC_DC, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:", side_by_side = FALSE)

final_output <- Forest_others %>%
  lucC_merge(., Pasture_others) %>%
  lucC_merge(., SV_others) %>%
  lucC_merge(., SC_DC)

#--------------
head(final_output)

lucC_plot_bar_events(final_output, custom_palette = FALSE, pixel_resolution = 231.6465, legend_text = "Legend:", side_by_side = FALSE)

#png(filename = "~/Desktop/fig_TESE/ch4_ita_bar_all.png", width = 7.5, height = 6, units = 'in', res = 300) # 3 columns to legend
lucC_plot_bar_events(final_output, pixel_resolution = 231.6465, custom_palette = TRUE, RGB_color = c("#6e8b3d", "#a2cd5a", "#006400", "#b4eeb4","#7e1416","#ee2c2c","#ffb6c1", "#000080", "#6495ed", "#00bfff", "#daa520"), legend_text = "Land use transitions:", relabel = TRUE, original_labels = c("Forest_Degradation", "Forest_Double_cropping", "Forest_Pasture", "Forest_Single_cropping", "Pasture_Double_cropping", "Pasture_Secondary_vegetation", "Pasture_Single_cropping", "Sec_Double_cropping", "SecV_Pasture", "SecV_Single_cropping", "SecV_Single_cropping" ), new_labels = c("Degradation", "Forest to Double Cropping", "Forest to Pasture", "Forest to Single Cropping", "Pasture to Double Cropping", "Pasture to Secondary Vegetation", "Pasture to Single Cropping", "Secondary Vegetation to Double Cropping", "Secondary Vegetation to Pasture", "Secondary Vegetation to Single Cropping", "Single Cropping to Double Cropping"), side_by_side = FALSE)
#dev.off()



