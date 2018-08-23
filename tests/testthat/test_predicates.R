testthat::context("Test Predicates")

testthat::test_that("HOLDS predicate", {
  testthat::skip_on_cran()
  file <- c(system.file("extdata/raster/rasterSample.tif", package = "lucCalculus"))
  rb_class <- raster::brick(file)
  my_label <- c("Degradation", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
                "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water")
  my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01",
                   "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01",
                   "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01",
                   "2016-09-01")
  # if return matrix
  forest_holds <- lucC_pred_holds(raster_obj = rb_class, raster_class = "Forest",
                                  time_interval = c("2001-09-01","2016-09-01"),
                                  label = my_label, timeline = my_timeline,
                                  relation_interval = "equals" )
  testthat::expect_true(nrow(forest_holds) > 0)

  # interval is correct
  testthat::expect_error(lucC_pred_holds(raster_obj = rb_class, raster_class = "Forest",
                                  time_interval = c("2003-09-01","2001-09-01"),
                                  label = my_label, timeline = my_timeline,
                                  relation_interval = "equals" ))
  # wrong way of writting - contains
  testthat::expect_error(lucC_pred_holds(raster_obj = rb_class, raster_class = "Forest",
                                         time_interval = c("2003-09-01","2001-09-01"),
                                         label = my_label, timeline = my_timeline,
                                         relation_interval = "contais" ))

})



testthat::test_that("RECUR predicate", {
  testthat::skip_on_cran()
  file <- c(system.file("extdata/raster/rasterSample.tif", package = "lucCalculus"))
  rb_class <- raster::brick(file)
  my_label <- c("Degradation", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
                "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water")
  my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01",
                   "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01",
                   "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01",
                   "2016-09-01")
  # if return matrix
  forest_recur <- lucC_pred_recur(raster_obj = rb_class, raster_class = "Forest",
                                  time_interval1 = c("2001-09-01","2001-09-01"),
                                  time_interval2 = c("2002-09-01","2016-09-01"),
                                  label = my_label, timeline = my_timeline )
  testthat::expect_true(nrow(forest_recur) > 0)
  # 15 years (less 2001) and 2 columns with latitude and longitude
  testthat::expect_true(ncol(forest_recur) == 17)

  # if does not remove the columns of first interval
  forest_recur <- lucC_pred_recur(raster_obj = rb_class, raster_class = "Forest",
                                  time_interval1 = c("2001-09-01","2001-09-01"),
                                  time_interval2 = c("2002-09-01","2016-09-01"),
                                  label = my_label, timeline = my_timeline,
                                  remove_column = FALSE )
  # 16 years and 2 columns with latitude and longitude
  testthat::expect_true(ncol(forest_recur) == 18)
})


testthat::test_that("RECUR predicate that does not allows overlapp and  first interval not occurs after second interval", {
  testthat::skip_on_cran()
  file <- c(system.file("extdata/raster/rasterSample.tif", package = "lucCalculus"))
  rb_class <- raster::brick(file)
  my_label <- c("Degradation", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
                "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water")
  my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01",
                   "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01",
                   "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01",
                   "2016-09-01")
  #overlap
  testthat::expect_error(forest_recur <- lucC_pred_recur(raster_obj = rb_class, raster_class = "Forest",
                                                         time_interval1 = c("2001-09-01","2002-09-01"),
                                                         time_interval2 = c("2002-09-01","2016-09-01"),
                                                         label = my_label, timeline = my_timeline ))

  # overlap
  testthat::expect_error(forest_recur <- lucC_pred_recur(raster_obj = rb_class, raster_class = "Forest",
                                                         time_interval1 = c("2001-09-01","2003-09-01"),
                                                         time_interval2 = c("2002-09-01","2016-09-01"),
                                                         label = my_label, timeline = my_timeline ))
  # first interval after second
  testthat::expect_error(forest_recur <- lucC_pred_recur(raster_obj = rb_class, raster_class = "Forest",
                                                         time_interval1 = c("2005-09-01","2006-09-01"),
                                                         time_interval2 = c("2001-09-01","2004-09-01"),
                                                         label = my_label, timeline = my_timeline ))

})


testthat::test_that("EVOLVE predicate", {
  testthat::skip_on_cran()
  file <- c(system.file("extdata/raster/rasterSample.tif", package = "lucCalculus"))
  rb_class <- raster::brick(file)
  my_label <- c("Degradation", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
                "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water")
  my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01",
                   "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01",
                   "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01",
                   "2016-09-01")
  # if return matrix
  output_evolve <- lucC_pred_evolve(raster_obj = rb_class, raster_class1 = "Forest",
                                    time_interval1 = c("2001-09-01","2001-09-01"),
                                    raster_class2 = "Degradation",
                                    time_interval2 = c("2002-09-01","2016-09-01"),
                                    label = my_label, timeline = my_timeline )
  testthat::expect_true(nrow(output_evolve) > 0)
  # 15 years (less 2001) and 2 columns with latitude and longitude
  testthat::expect_true(ncol(output_evolve) == 17)

  # if does not remove the columns of first interval
  output_evolve <- lucC_pred_evolve(raster_obj = rb_class, raster_class1 = "Forest",
                                    time_interval1 = c("2001-09-01","2001-09-01"),
                                    raster_class2 = "Degradation",
                                    time_interval2 = c("2002-09-01","2016-09-01"),
                                    label = my_label, timeline = my_timeline,
                                    remove_column = FALSE)
  # 16 years and 2 columns with latitude and longitude
  testthat::expect_true(ncol(output_evolve) == 18)
})


testthat::test_that("EVOLVE predicate that does not allows overlapp and first interval not occurs after second interval", {
  testthat::skip_on_cran()
  file <- c(system.file("extdata/raster/rasterSample.tif", package = "lucCalculus"))
  rb_class <- raster::brick(file)
  my_label <- c("Degradation", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
                "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water")
  my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01",
                   "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01",
                   "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01",
                   "2016-09-01")
  #overlap
  testthat::expect_error(output_evolve <- lucC_pred_evolve(raster_obj = rb_class, raster_class1 = "Forest",
                                                          time_interval1 = c("2001-09-01","2002-09-01"),
                                                          raster_class2 = "Degradation",
                                                          time_interval2 = c("2002-09-01","2016-09-01"),
                                                          label = my_label, timeline = my_timeline ))

  # overlap
  testthat::expect_error(output_evolve <- lucC_pred_evolve(raster_obj = rb_class, raster_class1 = "Forest",
                                                           time_interval1 = c("2001-09-01","2002-09-01"),
                                                           raster_class2 = "Degradation",
                                                           time_interval2 = c("2002-09-01","2016-09-01"),
                                                           label = my_label, timeline = my_timeline ))
  # first interval after second
  testthat::expect_error(output_evolve <- lucC_pred_evolve(raster_obj = rb_class, raster_class1 = "Forest",
                                                           time_interval1 = c("2001-09-01","2002-09-01"),
                                                           raster_class2 = "Degradation",
                                                           time_interval2 = c("2002-09-01","2016-09-01"),
                                                           label = my_label, timeline = my_timeline ))

})



testthat::test_that("CONVERT predicate", {
  testthat::skip_on_cran()
  file <- c(system.file("extdata/raster/rasterSample.tif", package = "lucCalculus"))
  rb_class <- raster::brick(file)
  my_label <- c("Degradation", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
                "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water")
  my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01",
                   "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01",
                   "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01",
                   "2016-09-01")
  # if return matrix
  output_convert <- lucC_pred_convert(raster_obj = rb_class, raster_class1 = "Forest",
                                    time_interval1 = c("2001-09-01","2001-09-01"),
                                    raster_class2 = "Degradation",
                                    time_interval2 = c("2002-09-01","2002-09-01"),
                                    label = my_label, timeline = my_timeline )
  testthat::expect_true(nrow(output_convert) > 0)
  # 1 years (less 2001) and 2 columns with latitude and longitude
  testthat::expect_true(ncol(output_convert) == 3)

  # if does not remove the columns of first interval
  output_convert <- lucC_pred_convert(raster_obj = rb_class, raster_class1 = "Forest",
                                    time_interval1 = c("2001-09-01","2001-09-01"),
                                    raster_class2 = "Degradation",
                                    time_interval2 = c("2002-09-01","2002-09-01"),
                                    label = my_label, timeline = my_timeline,
                                    remove_column = FALSE)
  # 2 years and 2 columns with latitude and longitude - convert MEETS
  testthat::expect_true(ncol(output_convert) == 4)

  output_convert <- lucC_pred_convert(raster_obj = rb_class, raster_class1 = "Forest",
                                      time_interval1 = c("2001-09-01","2002-09-01"),
                                      raster_class2 = "Degradation",
                                      time_interval2 = c("2003-09-01","2005-09-01"),
                                      label = my_label, timeline = my_timeline,
                                      remove_column = FALSE )
  # 5 years and 2 columns with latitude and longitude - convert MEETS
  testthat::expect_true(ncol(output_convert) == 7)

})


testthat::test_that("CONVERT predicate that does not allows overlapp and first interval not occurs after second interval", {
  testthat::skip_on_cran()
  file <- c(system.file("extdata/raster/rasterSample.tif", package = "lucCalculus"))
  rb_class <- raster::brick(file)
  my_label <- c("Degradation", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
                "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water")
  my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01",
                   "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01",
                   "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01",
                   "2016-09-01")
  #overlap
  testthat::expect_error(output_convert <- lucC_pred_convert(raster_obj = rb_class, raster_class1 = "Forest",
                                                           time_interval1 = c("2001-09-01","2002-09-01"),
                                                           raster_class2 = "Degradation",
                                                           time_interval2 = c("2002-09-01","2002-09-01"),
                                                           label = my_label, timeline = my_timeline ))

  # with exist a gap between two intervals
  testthat::expect_error(output_convert <- lucC_pred_convert(raster_obj = rb_class, raster_class1 = "Forest",
                                                           time_interval1 = c("2001-09-01","2001-09-01"),
                                                           raster_class2 = "Degradation",
                                                           time_interval2 = c("2003-09-01","2007-09-01"),
                                                           label = my_label, timeline = my_timeline ))
  # first interval after second
  testthat::expect_error(output_convert <- lucC_pred_convert(raster_obj = rb_class, raster_class1 = "Forest",
                                                           time_interval1 = c("2003-09-01","2003-09-01"),
                                                           raster_class2 = "Degradation",
                                                           time_interval2 = c("2002-09-01","2002-09-01"),
                                                           label = my_label, timeline = my_timeline ))

})





