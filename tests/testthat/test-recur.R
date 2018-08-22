testthat::context("Data input")
testthat::test_that("Open a file of example", {
  testthat::skip_on_cran()
  library(base)
  library(lucCalculus)
  file <- system.file("extdata/raster/rasterItanhanga.tif", package = "lucCalculus")
  rb_class <- raster::brick(file)
  my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01",
                   "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01",
                   "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01",
                   "2016-09-01")
  my_label <- c("Degradation", "Double_cropping", "Single_cropping", "Forest", "Pasture",
                "Pasture", "Pasture", "Double_cropping", "Double_cropping",
                "Double_cropping", "Double_cropping", "Double_cropping",
                "Single_cropping", "Single_cropping", "Water", "Water")
  forest_recur <- lucC_pred_recur(raster_obj = rb_class, raster_class = "Forest",
                                  time_interval1 = c("2001-09-01","2001-09-01"),
                                  time_interval2 = c("2002-09-01","2016-09-01"),
                                  label = my_label, timeline = my_timeline)
  testthat::expect_true(nrow(forest_recur) > 0)
})
