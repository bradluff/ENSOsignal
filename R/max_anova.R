#' ANOVA of the z-scored max discharge values.
#'
#' @param project_directory The directory used to save output files.
#' @export detrend



max_anova <- function(project_directory){
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
  max_aov <- aov(value~enso, max_anova)
  max_results <- anova(max_aov)
  print(TukeyHSD(max_aov))
  max_anova$enso <- factor(max_anova$enso, levels = c("La Niña","Neutral","El Niño"))
  boxplot(max_anova$value~max_anova$enso, col = c("salmon","snow3","skyblue"), main = "Z-scored Maximum Values", xlab = "ENSO Category", ylab = "Z-scored Discharge Value")
  return(list(max_anova,max_aov,max_results,max_nino,max_nina,max_neutral))
}
