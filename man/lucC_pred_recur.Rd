% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lucC_predicates_raster.R
\docType{data}
\name{lucC_pred_recur}
\alias{lucC_pred_recur}
\title{Predicate Recur}
\usage{
lucC_pred_recur (raster_obj = NULL, raster_class = NULL,
time_interval1 = c("2001-01-01", "2001-01-01"),
time_interval2 = c("2002-01-01", "2005-01-01"),
label = NULL, timeline = NULL, remove_column = TRUE)
}
\arguments{
\item{raster_obj}{Raster. A raster brick with classified images}

\item{raster_class}{Character. Name of the class of interest, such as 'Forest', to research}

\item{time_interval1}{Interval. A first time interval to verify if class is over or not}

\item{time_interval2}{Interval. A second and non-overlapped time interval to verify if class is over or not}

\item{label}{Character Vector. All labels of each value of pixel from classified raster}

\item{timeline}{Character. A list of all dates of classified raster, timeline}

\item{remove_column}{Boolean. Remove matrix values relating to the first time interval, only values of second interval are returned. Default is TRUE}
}
\value{
Matrix with all states which holds during a time interval
}
\description{
Provide a predicate RECUR which evaluates as true when a location holds
a class \code{c_i}, e.g. 'Forest', during two non-continuous distinct intervals
\code{t_i} and \code{t_j}. Return a matrix with values within defined interval
}
\examples{
\dontrun{
library(lucCalculus)

file <- c(system.file("extdata/raster/rasterSample.tif", package = "lucCalculus"))
rb_class <- raster::brick(file)
my_label <- c("Degradation", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton",
              "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water")
my_timeline <- c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01",
                 "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01",
                 "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01",
                 "2016-09-01")

b <- lucC_pred_recur(raster_obj = rb_class, raster_class = "Forest",
                     time_interval1 = c("2001-09-01","2001-09-01"),
                     time_interval2 = c("2002-09-01","2016-09-01"),
                     label = my_label, timeline = my_timeline)

lucC_plot_raster_result(raster_obj = rb_class, data_mtx = b,
                        timeline = my_timeline, label = my_label,
                        custom_palette = FALSE)

}

}
\author{
Adeline M. Maciel
}
\keyword{datasets}
