#' ANOVA of the z-scored min discharge values.
#'
#' @param project_directory The directory used to save output files.
#' @export detrend



min_anova <- function(project_directory){
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
  min_aov <- aov(value~enso, min_anova)
  min_results <- anova(min_aov)
  print(TukeyHSD(min_aov))
  min_anova$enso <- factor(min_anova$enso, levels = c("La Niña","Neutral","El Niño"))
  boxplot(min_anova$value~min_anova$enso, col = c("salmon","snow3","skyblue"), main = "Z-scored Minimum Values", xlab = "ENSO Category", ylab = "Z-scored Discharge Value")
  return(list(min_anova,min_aov,min_results,min_nino,min_nina,min_neutral))
}
