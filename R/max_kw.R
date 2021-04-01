#' ANOVA of the z-scored max discharge values.
#'
#' @param project_directory The directory used to save output files.



max_kw <- function(project_directory){
  library(FSA)
  # Establish Nino, Nina, Enso, and Neutral/Weak
  nino <- c(1951,1963,1968,1986,1994,2002,2009,1957,1965,1972,1987,1991,1982,1997)
  nina <- c(1955,1970,1995,1973,1975,1988,1998,1999,2007,2010)
  neutral <- c(1952,1953,1954,1956,1958,1959,1960,1961,1962,1964,1966,1967,1969,1971,1974,1976,1977,1978,1979,1980,1981,1983,1984,1985,1989,1990,1992,1993,1996,2000,2001,2003,2004,2005,2006,2008)
  water_years <- seq(1952, 2011, by = 1)
  max_z <- read.csv(paste0(project_directory, "max_z.csv"))
  colnames(max_z) <- as.character(water_years-1)
  # Separate max into three treatments
  max_nino <- max_z[,as.character(sort(nino))]
  max_nina <- max_z[,as.character(sort(nina))]
  max_neutral <- max_z[,as.character(sort(neutral))]
  # Setup the nino treatment for max
  max_nino_anova <- as.data.frame(matrix(nrow = length(max_nino)*length(max_nino$`1951`), ncol = 2))
  max_nino_anova$V1 <- as.vector(as.matrix(max_nino))
  max_nino_anova$V2 <- "El Niño"
  # Setup the nina treatment for max
  max_nina_anova <- as.data.frame(matrix(nrow = length(max_nina)*length(max_nina$`1955`), ncol = 2))
  max_nina_anova$V1 <- as.vector(as.matrix(max_nina))
  max_nina_anova$V2 <- "La Niña"
  # Setup the neutral treatment for max
  max_neutral_anova <- as.data.frame(matrix(nrow = length(max_neutral)*length(max_neutral$`1956`), ncol = 2))
  max_neutral_anova$V1 <- as.vector(as.matrix(max_neutral))
  max_neutral_anova$V2 <- "Neutral"
  # max anova dataframe
  max_anova <- rbind(max_nino_anova, max_nina_anova, max_neutral_anova)
  colnames(max_anova) <- c("value","enso")
  # max anova and results
  max_anova$enso <- factor(max_anova$enso, levels = c("La Niña","Neutral","El Niño"))
  max_aov <- kruskal.test(value~enso, max_anova)
  print(max_aov)
  dunn_results <- dunnTest(value~enso, max_anova)
  print(dunn_results)
  boxplot(max_anova$value~max_anova$enso, col = c("salmon","snow3","skyblue"), main = "Z-scored Maximum Values", xlab = "ENSO Category", ylab = "Z-scored Discharge Value")
  return(list(max_anova,max_aov,dunn_results,max_nino,max_nina,max_neutral))
}
