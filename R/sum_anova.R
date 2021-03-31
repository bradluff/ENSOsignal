#' Total Annual Discharge ANOVA
#'
#' ANOVA of the z-scored sum discharge values.
#'
#' @param project_directory The directory used to save output files.
#'
#' @return A list of values
#' @export detrend



# the function
sum_anova <- function(project_directory){
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
  sum_nino_anova <- as.data.frame(matrix(nrow = length(sum_nino)*length(sum_nino$`1951`), ncol = 2))
  sum_nino_anova$V1 <- as.vector(as.matrix(sum_nino))
  sum_nino_anova$V2 <- "El Niño"
  # Setup the nina treatment for sum
  sum_nina_anova <- as.data.frame(matrix(nrow = length(sum_nina)*length(sum_nina$`1955`), ncol = 2))
  sum_nina_anova$V1 <- as.vector(as.matrix(sum_nina))
  sum_nina_anova$V2 <- "La Niña"
  # Setup the neutral treatment for sum
  sum_neutral_anova <- as.data.frame(matrix(nrow = length(sum_neutral)*length(sum_neutral$`1956`), ncol = 2))
  sum_neutral_anova$V1 <- as.vector(as.matrix(sum_neutral))
  sum_neutral_anova$V2 <- "Neutral"
  # Sum anova dataframe
  sum_anova <- rbind(sum_nino_anova, sum_nina_anova, sum_neutral_anova)
  colnames(sum_anova) <- c("value","enso")
  # Anova and results
  sum_aov <- aov(value~enso, sum_anova)
  sum_results <- anova(sum_aov)
  print(TukeyHSD(sum_aov))
  sum_anova$enso <- factor(sum_anova$enso, levels = c("La Niña","Neutral","El Niño"))
  # suggest graphics
  boxplot(sum_anova$value~sum_anova$enso, col = c("salmon","snow3","skyblue"), main = "Z-scored Sum Values", xlab = "ENSO Category", ylab = "Z-scored Discharge Value")
  return(list(sum_anova,sum_aov,sum_results,sum_nino,sum_nina,sum_neutral))
}

