#' Calculate the most prevalent timing of peakflow events between conditions.
#'
#' @param project_directory The directory used to save output files.



peakflow_timing <- function(project_directory){

  # Establish Nino, Nina, Enso, and Neutral/Weak
  nino <- c(1953,1958,1966,1969,1977,1983,1987,1992,1998)
  nina <- c(1955,1956,1971,1974,1975,1976,1985,1989,1996,1999,2000,2008)
  neutral <- c(1951,1952,1954,1957,1959,1960,1961,1962,1963,1964,1965,1967,1968,1970,1972,1973,1978,1979,1980,1981,1982,1984,1986,1988,1990,1991,1993,1994,1995,1997,2001,2002,2003,2004,2005,2006,2007,2009,2010)

  # load the maximum date df from csv file
  maxdate_df <- read.csv(paste0(project_directory,"maxdate_df.csv"), stringsAsFactors = F, row.names = 1)

  # vector of water years
  water_years <- seq(1952, 2011, by = 1)

  # rename the maximum date columns with water years
  colnames(maxdate_df) <- water_years-1

  # for every column convert to date format
  for (column in colnames(maxdate_df)){
    maxdate_df[,column] <- as.Date(maxdate_df[,column], format = "%m-%d")
  }

  # separate the ENSO categories
  maxdate_nino <- maxdate_df[,as.character(sort(nino))]
  maxdate_nina <- maxdate_df[,as.character(sort(nina))]
  maxdate_neutral <- maxdate_df[,as.character(sort(neutral))]

  # water year converts to list of days
  water_year <- seq(as.Date("2021-01-01"),as.Date("2021-12-31"), by = "day")

  # convert days to df for plotting
  max_barplot <- as.data.frame(water_year)

  # stack new df x3 to account for 3 categories
  max_barplot <- rbind(max_barplot, max_barplot, max_barplot)

  # add two empty columns
  max_barplot$Count <- NA
  max_barplot$ENSO <- NA

  # for Neutral list the count of every date
  for (each_row in 1:length(max_barplot$water_year)){
    if (each_row <= 365){
      max_barplot$Count[each_row] <- length(na.omit(unlist(maxdate_neutral)[unlist(maxdate_neutral) == max_barplot$water_year[each_row]]))
      max_barplot$ENSO[each_row] <- "Neutral"
    }

    # for Nino list the count of every date
    if ((each_row > 365) & (each_row <= 730)){
      max_barplot$Count[each_row] <- length(na.omit(unlist(maxdate_nino)[unlist(maxdate_nino) == max_barplot$water_year[each_row]]))
      max_barplot$ENSO[each_row] <- "Nino"
    }

    # for Nina list the count of every date
    if ((each_row > 730) & (each_row <= 1095)){
      max_barplot$Count[each_row] <- length(na.omit(unlist(maxdate_nina)[unlist(maxdate_nina) == max_barplot$water_year[each_row]]))
      max_barplot$ENSO[each_row] <- "Nina"
    }
  }

  # create a histogram of the peak flow timing by ENSO category
  plot1 <- ggplot2::ggplot(data = max_barplot, ggplot2::aes(x = water_year, y = Count, fill = ENSO, group = 3)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::facet_grid(ENSO~., scales = "free") +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Count of Days") +
    ggplot2::ggtitle("Timing of the 15-Day Maximum Flow") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_smooth(span=0.15)

  # return the plot from the function
  return(plot1)
}
