#' ANOVA of the z-scored min discharge values.
#'
#' @param project_directory The directory used to save output files.



min_kw <- function(project_directory){
  library(FSA)
  # Establish Nino, Nina, Enso, and Neutral/Weak
  nino <- c(1951,1963,1968,1986,1994,2002,2009,1957,1965,1972,1987,1991,1982,1997)
  nina <- c(1955,1970,1995,1973,1975,1988,1998,1999,2007,2010)
  neutral <- c(1952,1953,1954,1956,1958,1959,1960,1961,1962,1964,1966,1967,1969,1971,1974,1976,1977,1978,1979,1980,1981,1983,1984,1985,1989,1990,1992,1993,1996,2000,2001,2003,2004,2005,2006,2008)
  water_years <- seq(1952, 2011, by = 1)
  min_z <- read.csv(paste0(project_directory, "min_z.csv"))
  colnames(min_z) <- as.character(water_years-1)
  # Separate min into three treatments
  min_nino <- min_z[,as.character(sort(nino))]
  min_nina <- min_z[,as.character(sort(nina))]
  min_neutral <- min_z[,as.character(sort(neutral))]
  # Setup the nino treatment for min
  min_nino_anova <- as.data.frame(matrix(nrow = length(min_nino)*length(min_nino$`1951`), ncol = 2))
  min_nino_anova$V1 <- as.vector(as.matrix(min_nino))
  min_nino_anova$V2 <- "El Niño"
  # Setup the nina treatment for min
  min_nina_anova <- as.data.frame(matrix(nrow = length(min_nina)*length(min_nina$`1955`), ncol = 2))
  min_nina_anova$V1 <- as.vector(as.matrix(min_nina))
  min_nina_anova$V2 <- "La Niña"
  # Setup the neutral treatment for min
  min_neutral_anova <- as.data.frame(matrix(nrow = length(min_neutral)*length(min_neutral$`1956`), ncol = 2))
  min_neutral_anova$V1 <- as.vector(as.matrix(min_neutral))
  min_neutral_anova$V2 <- "Neutral"
  # min anova dataframe
  min_anova <- rbind(min_nino_anova, min_nina_anova, min_neutral_anova)
  colnames(min_anova) <- c("value","enso")
  # min anova and results
  min_anova$enso <- factor(min_anova$enso, levels = c("La Niña","Neutral","El Niño"))
  min_aov <- kruskal.test(value~enso, min_anova)
  print(min_aov)
  dunn_results <- dunnTest(value~enso, min_anova)
  print(dunn_results)
  boxplot(min_anova$value~min_anova$enso, col = c("salmon","snow3","skyblue"), main = "Z-scored Minimum Values", xlab = "ENSO Category", ylab = "Z-scored Discharge Value")
  return(list(min_anova,min_aov,dunn_results,min_nino,min_nina,min_neutral))
}
