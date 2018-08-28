testthat::context("Test Allen's Predicates")

testthat::test_that("Predicate: Before, After, Meets, Met By, Follows and Precedes", {
  testthat::skip_on_cran()
  file <- c(system.file("extdata/raster/rasterSample.tif", package = "lucCalculus"))
  rb_class <- raster::brick(file)
  my_label <- c("Degradation", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
                "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water")
  my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01",
                   "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01",
                   "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01",
                   "2016-09-01")

  a <- lucC_pred_holds(raster_obj = rb_class, raster_class = "Forest",
                       time_interval = c("2001-09-01","2002-09-01"),
                       relation_interval = "equals", label = my_label,
                       timeline = my_timeline)

  b <- lucC_pred_holds(raster_obj = rb_class, raster_class = "Degradation",
                       time_interval = c("2003-09-01","2007-09-01"),
                       relation_interval = "contains", label = my_label,
                       timeline = my_timeline)

  # before
  testthat::expect_true(nrow(lucC_relation_before(first_raster = a, second_raster = b)) > 0)

  # after
  testthat::expect_message(lucC_relation_after(first_raster = a, second_raster = b))

  # meets
  testthat::expect_true(nrow(lucC_relation_meets(first_raster = a, second_raster = b)) > 0)

  # met_by
  testthat::expect_message(lucC_relation_met_by(first_raster = a, second_raster = b))

  # follows
  testthat::expect_true(nrow(lucC_relation_follows(first_raster = a, second_raster = b)) > 0)

  # precedes
  testthat::expect_message(lucC_relation_precedes(first_raster = a, second_raster = b))

  bar <- lucC_plot_bar_events(data_mtx = b, custom_palette = FALSE, side_by_side = TRUE)
  testthat::expect_true(is.ggplot(bar))

})




library(lucCalculus)

file <- c(system.file("extdata/raster/rasterSample.tif", package = "lucCalculus"))
rb_class <- raster::brick(file)
my_label <- c("Degradation", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
              "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water")
my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01",
                 "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01",
                 "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01",
                 "2016-09-01")

lucC_plot_raster(raster_obj = rb_class, timeline = my_timeline, label = my_label,
                 custom_palette = FALSE, plot_ncol = 4)

a <- lucC_pred_holds(raster_obj = rb_class, raster_class = "Forest",
                     time_interval = c("2004-09-01","2007-09-01"),
                     relation_interval = "equals", label = my_label,
                     timeline = my_timeline)

b <- lucC_pred_holds(raster_obj = rb_class, raster_class = "Degradation",
                     time_interval = c("2001-09-01","2003-09-01"),
                     relation_interval = "contains", label = my_label,
                     timeline = my_timeline)

# before
c <- lucC_relation_after(first_raster = a, second_raster = b)
c



