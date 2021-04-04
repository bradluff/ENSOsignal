#' Total Annual Discharge ANOVA
#'
#' ANOVA of the z-scored sum discharge values.
#'
#' @param project_directory The directory used to save output files.
#'
#' @return A list of values



# the function
sum_kw <- function(project_directory){
  library(FSA)
  # Establish Nino, Nina, Enso, and Neutral/Weak
  nino <- c(1953,1958,1966,1969,1977,1983,1987,1992,1998)
  nina <- c(1955,1956,1971,1974,1975,1976,1985,1989,1996,1999,2000,2008)
  neutral <- c(1951,1952,1954,1957,1959,1960,1961,1962,1963,1964,1965,1967,1968,1970,1972,1973,1978,1979,1980,1981,1982,1984,1986,1988,1990,1991,1993,1994,1995,1997,2001,2002,2003,2004,2005,2006,2007,2009,2010)
  # generate a sequential list of years
  water_years <- seq(1952, 2011, by = 1)
  # generate a sequential list of years
  sum_z <- read.csv(paste0(project_directory, "sum_z.csv"))
  # Rename the columns
  colnames(sum_z) <- as.character(water_years-1)
  # Separate sum into three treatments
  sum_nino <- sum_z[,as.character(sort(nino))]
  sum_nina <- sum_z[,as.character(sort(nina))]
  sum_neutral <- sum_z[,as.character(sort(neutral))]
  # Setup the nino treatment for sum
  sum_nino_anova <- as.data.frame(matrix(nrow = length(sum_nino)*length(sum_nino$`1953`), ncol = 2))
  sum_nino_anova$V1 <- as.vector(as.matrix(sum_nino))
  sum_nino_anova$V2 <- "El Niño"
  # Setup the nina treatment for sum
  sum_nina_anova <- as.data.frame(matrix(nrow = length(sum_nina)*length(sum_nina$`1955`), ncol = 2))
  sum_nina_anova$V1 <- as.vector(as.matrix(sum_nina))
  sum_nina_anova$V2 <- "La Niña"
  # Setup the neutral treatment for sum
  sum_neutral_anova <- as.data.frame(matrix(nrow = length(sum_neutral)*length(sum_neutral$`1951`), ncol = 2))
  sum_neutral_anova$V1 <- as.vector(as.matrix(sum_neutral))
  sum_neutral_anova$V2 <- "Neutral"
  # Sum anova dataframe
  sum_anova <- rbind(sum_nino_anova, sum_nina_anova, sum_neutral_anova)
  colnames(sum_anova) <- c("value","enso")
  # Anova and results
  sum_anova$enso <- factor(sum_anova$enso, levels = c("La Niña","Neutral","El Niño"))
  sum_aov <- kruskal.test(value~enso, sum_anova)
  print(sum_aov)
  dunn_results <- dunnTest(value~enso, sum_anova)
  print(dunn_results)
  sum_anova$ranked <- rank(sum_anova$value)
  boxplot(sum_anova$ranked~sum_anova$enso, col = c("salmon","snow3","skyblue"), main = "Z-scored Sum Ranks", xlab = "ENSO Category", ylab = "Z-scored Discharge Value")
  return(list(sum_anova,sum_aov,dunn_results,sum_nino,sum_nina,sum_neutral))
}

