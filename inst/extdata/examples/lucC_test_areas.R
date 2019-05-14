library(lucCalculus)

# always
options(digits = 12)

#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata with SITS
#----------------------------

# # create a RasterBrick from individual raster saved previously
# lucC_create_RasterBrick(path_open_GeoTIFFs = "/home/inpe/Downloads/MATO_GROSSO_9classes_SVM_Smooth/Filtro/", path_save_RasterBrick = "/home/inpe/Downloads/MATO_GROSSO_9classes_SVM_Smooth/Filtro")

# ------------- define variables to use in sits -------------
# Test with Alta Floresta municipality

# open files
#file <- "/home/inpe/Desktop/TESTE/MATO_GROSSO_9classes_SVM_Smooth/Filtro/AltaFloresta_brick.tif"
#file <- "/home/inpe/Desktop/TESTE/MATO_GROSSO_9classes_SVM_Smooth/Filtro/NovaNazare_brick.tif"
#file <- "/home/inpe/Desktop/TESTE/MATO_GROSSO_9classes_SVM_Smooth/Filtro/NovaMutum_brick.tif"
file <- "/home/inpe/Desktop/TESTE/MATO_GROSSO_9classes_SVM_Smooth/Filtro/Cotriguacu_brick.tif"
file

# new variable
my_timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01", "2017-09-01"))

# raster object
rb_class <- raster::brick(file)

# ------------- define variables to plot raster -------------
# original label - see QML file, same order
my_label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
                           "Soy_Fallow", "Soy_Millet", "Soy_Sunflower"))

# original colors set - see QML file, same order
my_colors <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6",
               "#e5c6a0", "#b69872", "#b68549")

# plot raster brick
#png(filename = "~/Desktop/TESTE/AltaFloresta_brick.png", width = 8, height = 6.4, units = 'in', res = 300)
lucC_plot_raster(raster_obj = rb_class,
                 timeline = my_timeline, label = my_label,
                 custom_palette = TRUE, RGB_color = my_colors, plot_ncol = 6)
#dev.off()

#------------------------
#----------------- Forest
# F C. F F
#-----------------

# auxiliary variables
my_timeline1 <- my_timeline
# correção para floresta
change_cer_for.df <- NULL
# raster input
raster.data <- rb_class
# set of other classes

# disregard the first and last timeline
for(x in 2:length(head(my_timeline1, 15))){
#  x = 2
  t_1 <- my_timeline1[x-1] # 2001
  t_2 <- my_timeline1[x]   # 2002
  t_3 <- my_timeline1[x+1] # 2003
  t_4 <- my_timeline1[x+2] # 2004
  cat(paste0(t_1, ", ", t_2, ", ", t_3, ", ", t_4, sep = ""), "\n")

  # along of all other classes
  #for(z in seq_along(classes)){
  #  z = 1
    #print(classes[z])

    class1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Forest",
                                time_interval = c(t_1,t_1),
                                relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

    class2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado",
                                time_interval = c(t_2,t_2),
                                relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

    class3.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Forest", #classes[z],
                                time_interval = c(t_3,t_4),
                                relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

    # Allen's relation MEETS, just pick up locations with relationship
    ForCerr.temp <- lucC_relation_meets(class1.df, class2.df)
    ForRest.temp <- lucC_relation_meets(class2.df, class3.df)

    # intersection between two times
    temp <- lucC_occurs(ForCerr.temp, ForRest.temp)

    if (!is.null(temp)) {
      # select only the column, into data.frame, with different class between two Pasture classes
      tempF <- lucC_select_columns(data_mtx = temp, name_columns = t_2)
    } else {
      tempF <- NULL
    }
    # add to final data.frame
    change_cer_for.df <- lucC_merge(change_cer_for.df, tempF)
  }

change_cer_for.df

# png(filename = "~/Desktop/TESTE/1_forest_FCFF.png", width = 8, height = 6.4, units = 'in', res = 300)
# lucC_plot_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, custom_palette = TRUE, RGB_color = my_colors, shape_point = ".", plot_ncol = 6)
# dev.off()

file_name <- "1_forest_FCFF"

message("Prepare image ...\n")
lucC_save_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, path_raster_folder = paste0("~/Desktop/TESTE/", file_name, sep = ""), as_RasterBrick = FALSE)


forest <- lucC_result_measures(data_mtx = change_cer_for.df, pixel_resolution = 232)
forest

write.table(forest, file = "~/Desktop/TESTE/1_forest_FCFF.csv", sep = ",", quote = FALSE, row.names = FALSE)

# clear environment, except these elements
rm(list=ls()[!(ls() %in% c('my_timeline', "my_label", "my_colors", "rb_class"))])
gc()


#------------------------
#----------------- Forest
# F F C. F
#-----------------

# auxiliary variables
my_timeline1 <- my_timeline
# correção para floresta
change_cer_for.df <- NULL
# raster input
raster.data <- rb_class
# set of other classes

# disregard the first and last timeline
for(x in 3:length(head(my_timeline1, 16))){
  #  x = 2
  t_1 <- my_timeline1[x-2] # 2001
  t_2 <- my_timeline1[x-1] # 2002
  t_3 <- my_timeline1[x]   # 2003
  t_4 <- my_timeline1[x+1] # 2004
  cat(paste0(t_1, ", ", t_2, ", ", t_3, ", ", t_4, sep = ""), "\n")

  # along of all other classes
  #for(z in seq_along(classes)){
  #  z = 1
  #print(classes[z])

  class1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Forest",
                               time_interval = c(t_1,t_2),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado",
                               time_interval = c(t_3,t_3),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class3.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Forest", #classes[z],
                               time_interval = c(t_4,t_4),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

  # Allen's relation MEETS, just pick up locations with relationship
  ForCerr.temp <- lucC_relation_meets(class1.df, class2.df)
  ForRest.temp <- lucC_relation_meets(class2.df, class3.df)

  # intersection between two times
  temp <- lucC_occurs(ForCerr.temp, ForRest.temp)

  if (!is.null(temp)) {
    # select only the column, into data.frame, with different class between two Pasture classes
    tempF <- lucC_select_columns(data_mtx = temp, name_columns = t_3)
  } else {
    tempF <- NULL
  }
  # add to final data.frame
  change_cer_for.df <- lucC_merge(change_cer_for.df, tempF)
}

change_cer_for.df

# png(filename = "~/Desktop/TESTE/2_forest_FFCF.png", width = 8, height = 6.4, units = 'in', res = 300)
# lucC_plot_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, custom_palette = TRUE, RGB_color = my_colors, shape_point = ".", plot_ncol = 6)
# dev.off()

file_name <- "2_forest_FFCF"

message("Prepare image ...\n")
lucC_save_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, path_raster_folder = paste0("~/Desktop/TESTE/", file_name, sep = ""), as_RasterBrick = FALSE)


forest <- lucC_result_measures(data_mtx = change_cer_for.df, pixel_resolution = 232)
forest

write.table(forest, file = "~/Desktop/TESTE/2_forest_FFCF.csv", sep = ",", quote = FALSE, row.names = FALSE)


# clear environment, except these elements
rm(list=ls()[!(ls() %in% c('my_timeline', "my_label", "my_colors", "rb_class"))])
gc()


#------------------------
#----------------- Forest
# F F F C.
#-----------------

# auxiliary variables
my_timeline1 <- my_timeline
# correção para floresta
change_cer_for.df <- NULL
# raster input
raster.data <- rb_class
# set of other classes

# disregard the first and last timeline
for(x in 4:length(head(my_timeline1, 17))){
  #  x = 2
  t_1 <- my_timeline1[x-3] # 2001
  t_2 <- my_timeline1[x-2] # 2002
  t_3 <- my_timeline1[x-1] # 2003
  t_4 <- my_timeline1[x]   # 2004
  cat(paste0(t_1, ", ", t_2, ", ", t_3, ", ", t_4, sep = ""), "\n")

  # along of all other classes
  #for(z in seq_along(classes)){
  #  z = 1
  #print(classes[z])

  class1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Forest",
                               time_interval = c(t_1,t_3),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado", #classes[z],
                               time_interval = c(t_4,t_4),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

  # # Allen's relation MEETS, just pick up locations with relationship
  # ForCerr.temp <- lucC_relation_meets(class1.df, class2.df)
  # ForRest.temp <- lucC_relation_meets(class2.df, class3.df)

  # intersection between two times
  temp <- lucC_occurs(class1.df, class2.df)

  if (!is.null(temp)) {
    # select only the column, into data.frame, with different class between two Pasture classes
    tempF <- lucC_select_columns(data_mtx = temp, name_columns = t_4)
  } else {
    tempF <- NULL
  }
  # add to final data.frame
  change_cer_for.df <- lucC_merge(change_cer_for.df, tempF)
}

change_cer_for.df

# png(filename = "~/Desktop/TESTE/3_forest_FFFC.png", width = 8, height = 6.4, units = 'in', res = 300)
# lucC_plot_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, custom_palette = TRUE, RGB_color = my_colors, shape_point = ".", plot_ncol = 6)
# dev.off()

file_name <- "3_forest_FFFC"

message("Prepare image ...\n")
lucC_save_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, path_raster_folder = paste0("~/Desktop/TESTE/", file_name, sep = ""), as_RasterBrick = FALSE)

forest <- lucC_result_measures(data_mtx = change_cer_for.df, pixel_resolution = 232)
forest

write.table(forest, file = "~/Desktop/TESTE/3_forest_FFFC.csv", sep = ",", quote = FALSE, row.names = FALSE)

# clear environment, except these elements
rm(list=ls()[!(ls() %in% c('my_timeline', "my_label", "my_colors", "rb_class"))])
gc()


#------------------------
#----------------- Forest
# F C. F C. F
#-----------------

# auxiliary variables
my_timeline1 <- my_timeline
# correção para floresta
change_cer_for.df <- NULL
# raster input
raster.data <- rb_class
# set of other classes

# disregard the first and last timeline
for(x in 2:length(head(my_timeline1, 14))){
  #  x = 2
  t_1 <- my_timeline1[x-1]  # 2001
  t_2 <- my_timeline1[x]    # 2002
  t_3 <- my_timeline1[x+1]  # 2003
  t_4 <- my_timeline1[x+2]  # 2004
  t_5 <- my_timeline1[x+3]  # 2005
  cat(paste0(t_1, ", ", t_2, ", ", t_3, ", ", t_4, ", ", t_5, sep = ""), "\n")

  # along of all other classes
  #for(z in seq_along(classes)){
  #  z = 1
  #print(classes[z])

  class1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Forest",
                               time_interval = c(t_1,t_1),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado",
                               time_interval = c(t_2,t_2),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class3.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Forest", #classes[z],
                               time_interval = c(t_3,t_3),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

  class4.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado", #classes[z],
                               time_interval = c(t_4,t_4),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

  class5.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Forest", #classes[z],
                               time_interval = c(t_5,t_5),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

  # Allen's relation MEETS, just pick up locations with relationship
  ForCerr.temp <- lucC_relation_meets(class1.df, class2.df)
  ForRest.temp <- lucC_relation_meets(class2.df, class3.df)
  ForRest1.temp <- lucC_relation_meets(class3.df, class4.df)
  ForRest2.temp <- lucC_relation_meets(class4.df, class5.df)

  # intersection between two times
  temp1 <- lucC_occurs(ForCerr.temp, ForRest.temp)
  temp2 <- lucC_occurs(ForRest1.temp, ForRest2.temp)

  temp <- lucC_occurs(temp1, temp2)

  if (!is.null(temp)) {
    # select only the column, into data.frame, with different class between two Pasture classes
    tempF <- lucC_select_columns(data_mtx = temp, name_columns = c(t_2, t_4))
  } else {
    tempF <- NULL
  }
  # add to final data.frame
  change_cer_for.df <- lucC_merge(change_cer_for.df, tempF)
}

change_cer_for.df

# png(filename = "~/Desktop/TESTE/4_forest_FCFCF.png", width = 8, height = 6.4, units = 'in', res = 300)
# lucC_plot_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, custom_palette = TRUE, RGB_color = my_colors, shape_point = ".", plot_ncol = 6)
# dev.off()

file_name <- "4_forest_FCFCF"

message("Prepare image ...\n")
lucC_save_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, path_raster_folder = paste0("~/Desktop/TESTE/", file_name, sep = ""), as_RasterBrick = FALSE)

forest <- lucC_result_measures(data_mtx = change_cer_for.df, pixel_resolution = 232)
forest

write.table(forest, file = "~/Desktop/TESTE/4_forest_FCFCF.csv", sep = ",", quote = FALSE, row.names = FALSE)


# clear environment, except these elements
rm(list=ls()[!(ls() %in% c('my_timeline', "my_label", "my_colors", "rb_class"))])
gc()


#--------------------------------------------------------------------------------

#------------------------
#----------------- Pasto
# P C. P P
#-----------------

# auxiliary variables
my_timeline1 <- my_timeline
# correção para floresta
change_cer_for.df <- NULL
# raster input
raster.data <- rb_class
# set of other classes

# disregard the first and last timeline
for(x in 2:length(head(my_timeline1, 15))){
  #  x = 2
  t_1 <- my_timeline1[x-1] # 2001
  t_2 <- my_timeline1[x]   # 2002
  t_3 <- my_timeline1[x+1] # 2003
  t_4 <- my_timeline1[x+2] # 2004
  cat(paste0(t_1, ", ", t_2, ", ", t_3, ", ", t_4, sep = ""), "\n")

  # along of all other classes
  #for(z in seq_along(classes)){
  #  z = 1
  #print(classes[z])

  class1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture",
                               time_interval = c(t_1,t_1),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado",
                               time_interval = c(t_2,t_2),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class3.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture", #classes[z],
                               time_interval = c(t_3,t_4),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

  # Allen's relation MEETS, just pick up locations with relationship
  ForCerr.temp <- lucC_relation_meets(class1.df, class2.df)
  ForRest.temp <- lucC_relation_meets(class2.df, class3.df)

  # intersection between two times
  temp <- lucC_occurs(ForCerr.temp, ForRest.temp)

  if (!is.null(temp)) {
    # select only the column, into data.frame, with different class between two Pasture classes
    tempF <- lucC_select_columns(data_mtx = temp, name_columns = t_2)
  } else {
    tempF <- NULL
  }
  # add to final data.frame
  change_cer_for.df <- lucC_merge(change_cer_for.df, tempF)
}

change_cer_for.df

# png(filename = "~/Desktop/TESTE/5_pasture_PCPP.png", width = 8, height = 6.4, units = 'in', res = 300)
# lucC_plot_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, custom_palette = TRUE, RGB_color = my_colors, shape_point = ".", plot_ncol = 6)
# dev.off()

file_name <- "5_pasture_PCPP"

message("Prepare image ...\n")
lucC_save_raster_result(raster_obj = rb_class, data_mtx = as.data.frame(change_cer_for.df), timeline = my_timeline, label = my_label, path_raster_folder = paste0("~/Desktop/TESTE/", file_name, sep = ""), as_RasterBrick = FALSE)


forest <- lucC_result_measures(data_mtx = change_cer_for.df, pixel_resolution = 232)
forest

write.table(forest, file = "~/Desktop/TESTE/5_pasture_PCPP.csv", sep = ",", quote = FALSE, row.names = FALSE)

# clear environment, except these elements
#rm(list=ls()[!(ls() %in% c('my_timeline', "my_label", "my_colors", "rb_class"))])
#gc()



#------------------------
#----------------- Pasto
# P P C. P
#-----------------

# auxiliary variables
my_timeline1 <- my_timeline
# correção para floresta
change_cer_for.df <- NULL
# raster input
raster.data <- rb_class
# set of other classes

# disregard the first and last timeline
for(x in 3:length(head(my_timeline1, 16))){
  #  x = 2
  t_1 <- my_timeline1[x-2] # 2001
  t_2 <- my_timeline1[x-1] # 2002
  t_3 <- my_timeline1[x]   # 2003
  t_4 <- my_timeline1[x+1] # 2004
  cat(paste0(t_1, ", ", t_2, ", ", t_3, ", ", t_4, sep = ""), "\n")

  # along of all other classes
  #for(z in seq_along(classes)){
  #  z = 1
  #print(classes[z])

  class1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture",
                               time_interval = c(t_1,t_2),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado",
                               time_interval = c(t_3,t_3),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class3.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture", #classes[z],
                               time_interval = c(t_4,t_4),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

  # Allen's relation MEETS, just pick up locations with relationship
  ForCerr.temp <- lucC_relation_meets(class1.df, class2.df)
  ForRest.temp <- lucC_relation_meets(class2.df, class3.df)

  # intersection between two times
  temp <- lucC_occurs(ForCerr.temp, ForRest.temp)

  if (!is.null(temp)) {
    # select only the column, into data.frame, with different class between two Pasture classes
    tempF <- lucC_select_columns(data_mtx = temp, name_columns = t_3)
  } else {
    tempF <- NULL
  }
  # add to final data.frame
  change_cer_for.df <- lucC_merge(change_cer_for.df, tempF)
}

change_cer_for.df

# png(filename = "~/Desktop/TESTE/6_pasture_PPCP.png", width = 8, height = 6.4, units = 'in', res = 300)
# lucC_plot_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, custom_palette = TRUE, RGB_color = my_colors, shape_point = ".", plot_ncol = 6)
# dev.off()

file_name <- "6_pasture_PPCP"

message("Prepare image ...\n")
lucC_save_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, path_raster_folder = paste0("~/Desktop/TESTE/", file_name, sep = ""), as_RasterBrick = FALSE)

forest <- lucC_result_measures(data_mtx = change_cer_for.df, pixel_resolution = 232)
forest

write.table(forest, file = "~/Desktop/TESTE/6_pasture_PPCP.csv", sep = ",", quote = FALSE, row.names = FALSE)

# clear environment, except these elements
rm(list=ls()[!(ls() %in% c('my_timeline', "my_label", "my_colors", "rb_class"))])
gc()



#------------------------
#----------------- Pasto
# P C. P C.
#-----------------

# auxiliary variables
my_timeline1 <- my_timeline
# correção para floresta
change_cer_for.df <- NULL
# raster input
raster.data <- rb_class
# set of other classes

# disregard the first and last timeline
for(x in 2:length(head(my_timeline1, 15))){
  #  x = 2
  t_1 <- my_timeline1[x-1]  # 2001
  t_2 <- my_timeline1[x]    # 2002
  t_3 <- my_timeline1[x+1]  # 2003
  t_4 <- my_timeline1[x+2]  # 2004
  cat(paste0(t_1, ", ", t_2, ", ", t_3, ", ", t_4, sep = ""), "\n")

  # along of all other classes
  #for(z in seq_along(classes)){
  #  z = 1
  #print(classes[z])

  class1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture",
                               time_interval = c(t_1,t_1),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado",
                               time_interval = c(t_2,t_2),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class3.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture", #classes[z],
                               time_interval = c(t_3,t_3),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

  class4.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado", #classes[z],
                               time_interval = c(t_4,t_4),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

  # Allen's relation MEETS, just pick up locations with relationship
  ForCerr.temp <- lucC_relation_meets(class1.df, class2.df)
  ForRest.temp <- lucC_relation_meets(class2.df, class3.df)
  ForRest1.temp <- lucC_relation_meets(class3.df, class4.df)

  # intersection between two times
  temp1 <- lucC_occurs(ForCerr.temp, ForRest.temp)
  temp <- lucC_occurs(temp1, ForRest1.temp)

  if (!is.null(temp)) {
    # select only the column, into data.frame, with different class between two Pasture classes
    tempF <- lucC_select_columns(data_mtx = temp, name_columns = c(t_2, t_4))
  } else {
    tempF <- NULL
  }
  # add to final data.frame
  change_cer_for.df <- lucC_merge(change_cer_for.df, tempF)
}

change_cer_for.df

# png(filename = "~/Desktop/TESTE/7_pasture_PCPC.png", width = 8, height = 6.4, units = 'in', res = 300)
# lucC_plot_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, custom_palette = TRUE, RGB_color = my_colors, shape_point = ".", plot_ncol = 6)
# dev.off()

file_name <- "7_pasture_PCPC"

message("Prepare image ...\n")
lucC_save_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, path_raster_folder = paste0("~/Desktop/TESTE/", file_name, sep = ""), as_RasterBrick = FALSE)

forest <- lucC_result_measures(data_mtx = change_cer_for.df, pixel_resolution = 232)
forest

write.table(forest, file = "~/Desktop/TESTE/7_pasture_PCPC.csv", sep = ",", quote = FALSE, row.names = FALSE)


# clear environment, except these elements
rm(list=ls()[!(ls() %in% c('my_timeline', "my_label", "my_colors", "rb_class"))])
gc()


#------------------------
#----------------- Pasto
# C. P C. P
#-----------------

# auxiliary variables
my_timeline1 <- my_timeline
# correção para floresta
change_cer_for.df <- NULL
# raster input
raster.data <- rb_class
# set of other classes

# disregard the first and last timeline
for(x in 2:length(head(my_timeline1, 15))){
  #  x = 2
  t_1 <- my_timeline1[x-1]  # 2001
  t_2 <- my_timeline1[x]    # 2002
  t_3 <- my_timeline1[x+1]  # 2003
  t_4 <- my_timeline1[x+2]  # 2004
  cat(paste0(t_1, ", ", t_2, ", ", t_3, ", ", t_4, sep = ""), "\n")

  # along of all other classes
  #for(z in seq_along(classes)){
  #  z = 1
  #print(classes[z])

  class1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado",
                               time_interval = c(t_1,t_1),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture",
                               time_interval = c(t_2,t_2),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class3.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado", #classes[z],
                               time_interval = c(t_3,t_3),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

  class4.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture", #classes[z],
                               time_interval = c(t_4,t_4),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

  # Allen's relation MEETS, just pick up locations with relationship
  ForCerr.temp <- lucC_relation_meets(class1.df, class2.df)
  ForRest.temp <- lucC_relation_meets(class2.df, class3.df)
  ForRest1.temp <- lucC_relation_meets(class3.df, class4.df)

  # intersection between two times
  temp1 <- lucC_occurs(ForCerr.temp, ForRest.temp)
  temp <- lucC_occurs(temp1, ForRest1.temp)

  if (!is.null(temp)) {
    # select only the column, into data.frame, with different class between two Pasture classes
    tempF <- lucC_select_columns(data_mtx = temp, name_columns = c(t_1, t_3))
  } else {
    tempF <- NULL
  }
  # add to final data.frame
  change_cer_for.df <- lucC_merge(change_cer_for.df, tempF)
}

change_cer_for.df

# png(filename = "~/Desktop/TESTE/8_pasture_CPCP.png", width = 8, height = 6.4, units = 'in', res = 300)
# lucC_plot_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, custom_palette = TRUE, RGB_color = my_colors, shape_point = ".", plot_ncol = 6)
# dev.off()

file_name <- "8_pasture_CPCP"

message("Prepare image ...\n")
lucC_save_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, path_raster_folder = paste0("~/Desktop/TESTE/", file_name, sep = ""), as_RasterBrick = FALSE)

forest <- lucC_result_measures(data_mtx = change_cer_for.df, pixel_resolution = 232)
forest

write.table(forest, file = "~/Desktop/TESTE/8_pasture_CPCP.csv", sep = ",", quote = FALSE, row.names = FALSE)


# clear environment, except these elements
rm(list=ls()[!(ls() %in% c('my_timeline', "my_label", "my_colors", "rb_class"))])
gc()



#------------------------
#----------------- Pasto
# P P C. C. P
#-----------------

# auxiliary variables
my_timeline1 <- my_timeline
# correção para floresta
change_cer_for.df <- NULL
# raster input
raster.data <- rb_class
# set of other classes

# disregard the first and last timeline
for(x in 3:length(head(my_timeline1, 15))){
  #  x = 2
  t_1 <- my_timeline1[x-2] # 2001
  t_2 <- my_timeline1[x-1] # 2002
  t_3 <- my_timeline1[x]   # 2003
  t_4 <- my_timeline1[x+1] # 2004
  t_5 <- my_timeline1[x+2] # 2005
  cat(paste0(t_1, ", ", t_2, ", ", t_3, ", ", t_4, ", ", t_5, sep = ""), "\n")

  # along of all other classes
  #for(z in seq_along(classes)){
  #  z = 1
  #print(classes[z])

  class1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture",
                               time_interval = c(t_1,t_2),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado",
                               time_interval = c(t_3,t_4),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class3.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture", #classes[z],
                               time_interval = c(t_5,t_5),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

  # Allen's relation MEETS, just pick up locations with relationship
  ForCerr.temp <- lucC_relation_meets(class1.df, class2.df)
  ForRest.temp <- lucC_relation_meets(class2.df, class3.df)

  # intersection between two times
  temp <- lucC_occurs(ForCerr.temp, ForRest.temp)

  if (!is.null(temp)) {
    # select only the column, into data.frame, with different class between two Pasture classes
    tempF <- lucC_select_columns(data_mtx = temp, name_columns = c(t_3,t_4))
  } else {
    tempF <- NULL
  }
  # add to final data.frame
  change_cer_for.df <- lucC_merge(change_cer_for.df, tempF)
}

change_cer_for.df

# png(filename = "~/Desktop/TESTE/9_pasture_PPCCP.png", width = 8, height = 6.4, units = 'in', res = 300)
# lucC_plot_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, custom_palette = TRUE, RGB_color = my_colors, shape_point = ".", plot_ncol = 6)
# dev.off()

file_name <- "9_pasture_PPCCP"

message("Prepare image ...\n")
lucC_save_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, path_raster_folder = paste0("~/Desktop/TESTE/", file_name, sep = ""), as_RasterBrick = FALSE)

forest <- lucC_result_measures(data_mtx = change_cer_for.df, pixel_resolution = 232)
forest

write.table(forest, file = "~/Desktop/TESTE/9_pasture_PPCCP.csv", sep = ",", quote = FALSE, row.names = FALSE)

# clear environment, except these elements
rm(list=ls()[!(ls() %in% c('my_timeline', "my_label", "my_colors", "rb_class"))])
gc()



#---------------------------------------------------------------------------------
#----------------- Cerrado
# C C P. C
#-----------------

# auxiliary variables
my_timeline1 <- my_timeline
# correção para floresta
change_cer_for.df <- NULL
# raster input
raster.data <- rb_class
# set of other classes

# disregard the first and last timeline
for(x in 3:length(head(my_timeline1, 16))){
  #  x = 2
  t_1 <- my_timeline1[x-2] # 2001
  t_2 <- my_timeline1[x-1] # 2002
  t_3 <- my_timeline1[x]   # 2003
  t_4 <- my_timeline1[x+1] # 2004
  cat(paste0(t_1, ", ", t_2, ", ", t_3, ", ", t_4, sep = ""), "\n")

  # along of all other classes
  #for(z in seq_along(classes)){
  #  z = 1
  #print(classes[z])

  class1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado",
                               time_interval = c(t_1,t_2),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture",
                               time_interval = c(t_3,t_3),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class3.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado", #classes[z],
                               time_interval = c(t_4,t_4),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

  # Allen's relation MEETS, just pick up locations with relationship
  ForCerr.temp <- lucC_relation_meets(class1.df, class2.df)
  ForRest.temp <- lucC_relation_meets(class2.df, class3.df)

  # intersection between two times
  temp <- lucC_occurs(ForCerr.temp, ForRest.temp)

  if (!is.null(temp)) {
    # select only the column, into data.frame, with different class between two Pasture classes
    tempF <- lucC_select_columns(data_mtx = temp, name_columns = t_3)
  } else {
    tempF <- NULL
  }
  # add to final data.frame
  change_cer_for.df <- lucC_merge(change_cer_for.df, tempF)
}

change_cer_for.df

# png(filename = "~/Desktop/TESTE/10_cerrado_CCPC.png", width = 8, height = 6.4, units = 'in', res = 300)
# lucC_plot_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, custom_palette = TRUE, RGB_color = my_colors, shape_point = ".", plot_ncol = 6)
# dev.off()

file_name <- "10_cerrado_CCPC"

message("Prepare image ...\n")
lucC_save_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, path_raster_folder = paste0("~/Desktop/TESTE/", file_name, sep = ""), as_RasterBrick = FALSE)

forest <- lucC_result_measures(data_mtx = change_cer_for.df, pixel_resolution = 232)
forest

write.table(forest, file = "~/Desktop/TESTE/10_cerrado_CCPC.csv", sep = ",", quote = FALSE, row.names = FALSE)

# clear environment, except these elements
rm(list=ls()[!(ls() %in% c('my_timeline', "my_label", "my_colors", "rb_class"))])
gc()



#------------------------
#----------------- Cerrado
# P P C. C.
#-----------------

# auxiliary variables
my_timeline1 <- my_timeline
# correção para floresta
change_cer_for.df <- NULL
# raster input
raster.data <- rb_class
# set of other classes

# disregard the first and last timeline
for(x in 3:length(head(my_timeline1, 16))){
  #  x = 2
  t_1 <- my_timeline1[x-2] # 2001
  t_2 <- my_timeline1[x-1] # 2002
  t_3 <- my_timeline1[x]   # 2003
  t_4 <- my_timeline1[x+1] # 2004
  cat(paste0(t_1, ", ", t_2, ", ", t_3, ", ", t_4, sep = ""), "\n")

  # along of all other classes
  #for(z in seq_along(classes)){
  #  z = 1
  #print(classes[z])

  class1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture",
                               time_interval = c(t_1,t_2),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado",
                               time_interval = c(t_3,t_4),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  # Allen's relation MEETS, just pick up locations with relationship
  #ForCerr.temp <- lucC_relation_meets(class1.df, class2.df)

  # intersection between two times
  temp <- lucC_occurs(class1.df, class2.df)

  if (!is.null(temp)) {
    # select only the column, into data.frame, with different class between two Pasture classes
    tempF <- lucC_select_columns(data_mtx = temp, name_columns = c(t_3,t_4))
  } else {
    tempF <- NULL
  }
  # add to final data.frame
  change_cer_for.df <- lucC_merge(change_cer_for.df, tempF)
}

change_cer_for.df

# png(filename = "~/Desktop/TESTE/11_cerrado_PPCC.png", width = 8, height = 6.4, units = 'in', res = 300)
# lucC_plot_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, custom_palette = TRUE, RGB_color = my_colors, shape_point = ".", plot_ncol = 6)
# dev.off()

file_name <- "11_cerrado_PPCC"

message("Prepare image ...\n")
lucC_save_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, path_raster_folder = paste0("~/Desktop/TESTE/", file_name, sep = ""), as_RasterBrick = FALSE)

forest <- lucC_result_measures(data_mtx = change_cer_for.df, pixel_resolution = 232)
forest

write.table(forest, file = "~/Desktop/TESTE/11_cerrado_PPCC.csv", sep = ",", quote = FALSE, row.names = FALSE)

# clear environment, except these elements
rm(list=ls()[!(ls() %in% c('my_timeline', "my_label", "my_colors", "rb_class"))])
gc()



#----------------- Cerrado
# C C S C.
#-----------------
#my_label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
#                           "Soy_Fallow", "Soy_Millet", "Soy_Sunflower"))
my_label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soybean", "Soybean",
                           "Soybean", "Soybean", "Soybean"))

# auxiliary variables
my_timeline1 <- my_timeline
# correção para floresta
change_cer_for.df <- NULL
# raster input
raster.data <- rb_class
# set of other classes

# disregard the first and last timeline
for(x in 3:length(head(my_timeline1, 16))){
  #  x = 2
  t_1 <- my_timeline1[x-2] # 2001
  t_2 <- my_timeline1[x-1] # 2002
  t_3 <- my_timeline1[x]   # 2003
  t_4 <- my_timeline1[x+1] # 2004
  cat(paste0(t_1, ", ", t_2, ", ", t_3, ", ", t_4, sep = ""), "\n")

  # along of all other classes
  #for(z in seq_along(classes)){
  #  z = 1
  #print(classes[z])

  class1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado",
                               time_interval = c(t_1,t_2),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Soybean",
                               time_interval = c(t_3,t_3),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class3.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado", #classes[z],
                               time_interval = c(t_4,t_4),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x

  # Allen's relation MEETS, just pick up locations with relationship
  ForCerr.temp <- lucC_relation_meets(class1.df, class2.df)
  ForRest.temp <- lucC_relation_meets(class2.df, class3.df)

  # intersection between two times
  temp <- lucC_occurs(ForCerr.temp, ForRest.temp)

  if (!is.null(temp)) {
    # select only the column, into data.frame, with different class between two Pasture classes
    tempF <- lucC_select_columns(data_mtx = temp, name_columns = t_4)
  } else {
    tempF <- NULL
  }
  # add to final data.frame
  change_cer_for.df <- lucC_merge(change_cer_for.df, tempF)
}

change_cer_for.df

# png(filename = "~/Desktop/TESTE/12_cerrado_CCSC.png", width = 8, height = 6.4, units = 'in', res = 300)
# lucC_plot_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, custom_palette = TRUE, RGB_color = my_colors, shape_point = ".", plot_ncol = 6)
# dev.off()

file_name <- "12_cerrado_CCSC"

message("Prepare image ...\n")
lucC_save_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, path_raster_folder = paste0("~/Desktop/TESTE/", file_name, sep = ""), as_RasterBrick = FALSE)

forest <- lucC_result_measures(data_mtx = change_cer_for.df, pixel_resolution = 232)
forest

write.table(forest, file = "~/Desktop/TESTE/12_cerrado_CCSC.csv", sep = ",", quote = FALSE, row.names = FALSE)

# clear environment, except these elements
rm(list=ls()[!(ls() %in% c('my_timeline', "my_label", "my_colors", "rb_class"))])
gc()



#----------------- Cerrado
# S S C. C.
#-----------------
#my_label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
#                           "Soy_Fallow", "Soy_Millet", "Soy_Sunflower"))
my_label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soybean", "Soybean",
                           "Soybean", "Soybean", "Soybean"))

# auxiliary variables
my_timeline1 <- my_timeline
# correção para floresta
change_cer_for.df <- NULL
# raster input
raster.data <- rb_class
# set of other classes

# disregard the first and last timeline
for(x in 3:length(head(my_timeline1, 16))){
  #  x = 2
  t_1 <- my_timeline1[x-2] # 2001
  t_2 <- my_timeline1[x-1] # 2002
  t_3 <- my_timeline1[x]   # 2003
  t_4 <- my_timeline1[x+1] # 2004
  cat(paste0(t_1, ", ", t_2, ", ", t_3, ", ", t_4, sep = ""), "\n")

  # along of all other classes
  #for(z in seq_along(classes)){
  #  z = 1
  #print(classes[z])

  class1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Soybean",
                               time_interval = c(t_1,t_2),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado",
                               time_interval = c(t_3,t_4),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  # Allen's relation MEETS, just pick up locations with relationship
  #ForCerr.temp <- lucC_relation_meets(class1.df, class2.df)

  # intersection between two times
  temp <- lucC_occurs(class1.df, class2.df)

  if (!is.null(temp)) {
    # select only the column, into data.frame, with different class between two Pasture classes
    tempF <- lucC_select_columns(data_mtx = temp, name_columns = c(t_3,t_4))
  } else {
    tempF <- NULL
  }
  # add to final data.frame
  change_cer_for.df <- lucC_merge(change_cer_for.df, tempF)
}

change_cer_for.df

# png(filename = "~/Desktop/TESTE/13_cerrado_SSCC.png", width = 8, height = 6.4, units = 'in', res = 300)
# lucC_plot_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, custom_palette = TRUE, RGB_color = my_colors, shape_point = ".", plot_ncol = 6)
# dev.off()

file_name <- "13_cerrado_SSCC"

message("Prepare image ...\n")
lucC_save_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, path_raster_folder = paste0("~/Desktop/TESTE/", file_name, sep = ""), as_RasterBrick = FALSE)

forest <- lucC_result_measures(data_mtx = change_cer_for.df, pixel_resolution = 232)
forest

write.table(forest, file = "~/Desktop/TESTE/13_cerrado_SSCC.csv", sep = ",", quote = FALSE, row.names = FALSE)

# clear environment, except these elements
rm(list=ls()[!(ls() %in% c('my_timeline', "my_label", "my_colors", "rb_class"))])
gc()



#----------------- Cerrado
# F F C. C.
#-----------------
#my_label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
#                           "Soy_Fallow", "Soy_Millet", "Soy_Sunflower"))
my_label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soybean", "Soybean",
                           "Soybean", "Soybean", "Soybean"))

# auxiliary variables
my_timeline1 <- my_timeline
# correção para floresta
change_cer_for.df <- NULL
# raster input
raster.data <- rb_class
# set of other classes

# disregard the first and last timeline
for(x in 3:length(head(my_timeline1, 16))){
  #  x = 2
  t_1 <- my_timeline1[x-2] # 2001
  t_2 <- my_timeline1[x-1] # 2002
  t_3 <- my_timeline1[x]   # 2003
  t_4 <- my_timeline1[x+1] # 2004
  cat(paste0(t_1, ", ", t_2, ", ", t_3, ", ", t_4, sep = ""), "\n")

  # along of all other classes
  #for(z in seq_along(classes)){
  #  z = 1
  #print(classes[z])

  class1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Forest",
                               time_interval = c(t_1,t_2),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  class2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado",
                               time_interval = c(t_3,t_4),
                               relation_interval = "equals", label = my_label, timeline = my_timeline) # 200x-1

  # Allen's relation MEETS, just pick up locations with relationship
  #ForCerr.temp <- lucC_relation_meets(class1.df, class2.df)

  # intersection between two times
  temp <- lucC_occurs(class1.df, class2.df)

  if (!is.null(temp)) {
    # select only the column, into data.frame, with different class between two Pasture classes
    tempF <- lucC_select_columns(data_mtx = temp, name_columns = c(t_3,t_4))
  } else {
    tempF <- NULL
  }
  # add to final data.frame
  change_cer_for.df <- lucC_merge(change_cer_for.df, tempF)
}

change_cer_for.df

# png(filename = "~/Desktop/TESTE/14_cerrado_FFCC.png", width = 8, height = 6.4, units = 'in', res = 300)
# lucC_plot_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, custom_palette = TRUE, RGB_color = my_colors, shape_point = ".", plot_ncol = 6)
# dev.off()

file_name <- "14_cerrado_FFCC"

message("Prepare image ...\n")
lucC_save_raster_result(raster_obj = rb_class, data_mtx = change_cer_for.df, timeline = my_timeline, label = my_label, path_raster_folder = paste0("~/Desktop/TESTE/", file_name, sep = ""), as_RasterBrick = FALSE)

forest <- lucC_result_measures(data_mtx = change_cer_for.df, pixel_resolution = 232)
forest

write.table(forest, file = "~/Desktop/TESTE/14_cerrado_FFCC.csv", sep = ",", quote = FALSE, row.names = FALSE)

# clear environment, except these elements
rm(list=ls()[!(ls() %in% c('my_timeline', "my_label", "my_colors", "rb_class"))])
gc()


