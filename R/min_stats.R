#' 7-Day Minimum Discharge Statistics
#'
#' ANOVA or Kruskal-Wallis ranked test of the z-scored min discharge values.
#'
#' @param project_directory The directory used to save output files.
#' @param normal TRUE or FALSE will determine if an ANOVA or Kruskal-Wallis test is used
#'
#' @return A list of values



min_stats <- function(project_directory, normal = FALSE){

  # Establish Nino, Nina, Enso, and Neutral/Weak
  nino <- c(1953,1958,1966,1969,1977,1983,1987,1992,1998)
  nina <- c(1955,1956,1971,1974,1975,1976,1985,1989,1996,1999,2000,2008)
  neutral <- c(1951,1952,1954,1957,1959,1960,1961,1962,1963,1964,1965,1967,1968,1970,1972,1973,1978,1979,1980,1981,1982,1984,1986,1988,1990,1991,1993,1994,1995,1997,2001,2002,2003,2004,2005,2006,2007,2009,2010)

  # generate a sequential list of years
  water_years <- seq(1952, 2011, by = 1)

  # read in the z scored data
  data_z <- read.csv(paste0(project_directory, "min_z.csv"))

  # Rename the columns
  colnames(data_z) <- as.character(water_years-1)

  # Separate data into three treatments
  data_nino <- data_z[,as.character(sort(nino))]
  data_nina <- data_z[,as.character(sort(nina))]
  data_neutral <- data_z[,as.character(sort(neutral))]

  # Setup the nino treatment for data
  data_nino_stats <- as.data.frame(matrix(nrow = length(data_nino)*length(data_nino$`1953`), ncol = 2))
  data_nino_stats$V1 <- as.vector(as.matrix(data_nino))
  data_nino_stats$V2 <- "El Niño"

  # Setup the nina treatment for data
  data_nina_stats <- as.data.frame(matrix(nrow = length(data_nina)*length(data_nina$`1955`), ncol = 2))
  data_nina_stats$V1 <- as.vector(as.matrix(data_nina))
  data_nina_stats$V2 <- "La Niña"

  # Setup the neutral treatment for data
  data_neutral_stats <- as.data.frame(matrix(nrow = length(data_neutral)*length(data_neutral$`1951`), ncol = 2))
  data_neutral_stats$V1 <- as.vector(as.matrix(data_neutral))
  data_neutral_stats$V2 <- "Neutral"

  # data stats dataframe
  data_stats <- rbind(data_nino_stats, data_nina_stats, data_neutral_stats)

  # rename the columns
  colnames(data_stats) <- c("value","enso")

  # if the data are not normal use non-parametric statistics
  if (normal == FALSE){

    # load FSA
    library(FSA)

    # factor the categories
    data_stats$enso <- factor(data_stats$enso, levels = c("La Niña","Neutral","El Niño"))

    # Kruskal-Wallis ranked test
    data_aov <- kruskal.test(value~enso, data_stats)

    # print the results
    print(data_aov)

    # Dunn post hoc test
    posthoc_results <- dunnTest(value~enso, data_stats)

    # print post hoc results
    print(posthoc_results)

    # rank the data to boxplot
    data_stats$ranked <- rank(data_stats$value)

    # boxplot of ranked data
    boxplot(data_stats$ranked~data_stats$enso, col = c("salmon","snow3","skyblue"), main = "Z-scored Min Ranks", xlab = "ENSO Category", ylab = "Z-scored Discharge Value")
  }

  # if the data are normal use ANOVA
  if (normal == TRUE){

    # factor the categories
    data_stats$enso <- factor(data_stats$enso, levels = c("La Niña","Neutral","El Niño"))

    # prep the data for an anova
    data_aov <- aov(value~enso, data_stats)

    # anova
    data_results <- anova(data_aov)

    # print anova results
    print(data_results)

    # Tukey Honest Significant DIfference
    posthoc_results <- TukeyHSD(data_aov)

    # print the results
    print(posthoc_results)

    # boxplot of the data
    boxplot(data_stats$value~data_stats$enso, col = c("salmon","snow3","skyblue"), main = "Z-scored Min Values", xlab = "ENSO Category", ylab = "Z-scored Discharge Value")
  }

  # return a list of values
  return(list(data_stats,data_aov,posthoc_results,data_nino,data_nina,data_neutral))
}
