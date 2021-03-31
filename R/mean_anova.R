#' ANOVA of the z-scored mean discharge values.
#'
#' @param project_directory The directory used to save output files.
#' @export detrend



mean_anova <- function(project_directory){
  water_years <- seq(1952, 2011, by = 1)
  mean_z <- read.csv(paste0(project_directory, "mean_z.csv"))
  colnames(mean_z) <- as.character(water_years-1)
  # Separate mean into three treatments
  mean_nino <- mean_z[,as.character(sort(nino))]
  mean_nina <- mean_z[,as.character(sort(nina))]
  mean_neutral <- mean_z[,as.character(sort(neutral))]
  # Setup the nino treatment for mean
  mean_nino_anova <- as.data.frame(matrix(nrow = length(mean_nino)*length(mean_nino$`1951`), ncol = 2))
  mean_nino_anova$V1 <- as.vector(as.matrix(mean_nino))
  mean_nino_anova$V2 <- "El Niño"
  # Setup the nina treatment for mean
  mean_nina_anova <- as.data.frame(matrix(nrow = length(mean_nina)*length(mean_nina$`1955`), ncol = 2))
  mean_nina_anova$V1 <- as.vector(as.matrix(mean_nina))
  mean_nina_anova$V2 <- "La Niña"
  # Setup the neutral treatment for mean
  mean_neutral_anova <- as.data.frame(matrix(nrow = length(mean_neutral)*length(mean_neutral$`1956`), ncol = 2))
  mean_neutral_anova$V1 <- as.vector(as.matrix(mean_neutral))
  mean_neutral_anova$V2 <- "Neutral"
  # mean anova dataframe
  mean_anova <- rbind(mean_nino_anova, mean_nina_anova, mean_neutral_anova)
  colnames(mean_anova) <- c("value","enso")
  # mean anova and results
  mean_aov <- aov(value~enso, mean_anova)
  mean_results <- anova(mean_aov)
  print(TukeyHSD(mean_aov))
  mean_anova$enso <- factor(mean_anova$enso, levels = c("La Niña","Neutral","El Niño"))
  boxplot(mean_anova$value~mean_anova$enso, col = c("salmon","snow3","skyblue"), main = "Z-scored Mean Values", xlab = "ENSO Category", ylab = "Z-scored Discharge Value")
  return(list(mean_anova,mean_aov,mean_results,mean_nino,mean_nina,mean_neutral))
}
