#' Plot the ONI values by water year
#'
#'
#'



enso_plot <- function(){

  # ONI values by watery year from 1951 - 2010
  oni_intensity <- c(0.041666667,0.4,0.525,0,-0.75,-0.858333333,0.475,1.083333333,0.25,0.025,0.058333333,-0.191666667,
                     0.25,0.183333333,0.225,0.9,-0.258333333,-0.133333333,0.733333333,0.091666667,-0.933333333,0.191666667,
                     0.341666667,-1.183333333,-0.833333333,-0.65,0.516666667,0.125,0.075,0.383333333,-0.241666667,0.441666667,
                     1.233333333,-0.5,-0.733333333,-0.108333333,1.216666667,-0.066666667,-0.991666667,0.175,
                     0.441666667,0.983333333,0.258333333,0.258333333,0.325,-0.608333333,0.466666667,0.9,-1.233333333,
                     -1.033333333,-0.391666667,0.258333333,0.466666667,0.375,0.35,-0.3,-0.025,-1,-0.175,0.258333333)

  # water years vector
  water_years <- seq(1951, 2010, by = 1)

  # Establish Nino, Nina, Enso, and Neutral/Weak
  nino <- c(1953,1958,1966,1969,1977,1983,1987,1992,1998)
  nina <- c(1955,1956,1971,1974,1975,1976,1985,1989,1996,1999,2000,2008)
  neutral <- c(1951,1952,1954,1957,1959,1960,1961,1962,1963,1964,1965,1967,1968,1970,1972,1973,1978,1979,1980,1981,1982,1984,1986,1988,1990,1991,1993,1994,1995,1997,2001,2002,2003,2004,2005,2006,2007,2009,2010)

  # create a water years dataframe
  df <- as.data.frame(water_years)

  # add the ONI intensity
  df$ONI <- oni_intensity

  # empty column to save ENSO category
  df$ENSO <- NA

  # Assign ENSO categories
  df[df$water_years %in% neutral,3] <- "Neutral"
  df[df$water_years %in% nino,3] <- "El Nino"
  df[df$water_years %in% nina,3] <- "La Nina"

  # create a line plot with colored points for the category
  plot1 <- ggplot2::ggplot(df, ggplot2::aes(water_years,ONI)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(water_years,ONI,col=factor(ENSO), group = 1), size = 4) +
    ggplot2::scale_color_manual(values=c("blue","red","black")) +
    ggplot2::ggtitle("ONI Values Averaged by Water Year") +
    ggplot2::xlab("Water Year") +
    ggplot2::ylab("ONI") +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  # return the plot
  return(plot1)
}
