#' Calculate the most prevalent timing of peakflow events between conditions.
#'
#' @param project_directory The directory used to save output files.
#' @export detrend



peakflow_timing <- function(project_directory){
  require(ggplot2)
  maxdate_df <- read.csv(paste0(project_directory,"maxdate_df.csv"), stringsAsFactors = F, row.names = 1)
  water_years <- seq(1952, 2011, by = 1)
  colnames(maxdate_df) <- water_years-1
  for (column in colnames(maxdate_df)){
    maxdate_df[,column] <- as.Date(maxdate_df[,column], format = "%m-%d")
  }
  maxdate_nino <- maxdate_df[,as.character(sort(nino))]
  maxdate_nina <- maxdate_df[,as.character(sort(nina))]
  maxdate_neutral <- maxdate_df[,as.character(sort(neutral))]
  water_year <- seq(as.Date("2020-01-01"),as.Date("2020-12-31"), by = "day")
  max_barplot <- as.data.frame(water_year)
  max_barplot <- rbind(max_barplot, max_barplot, max_barplot)
  max_barplot$Count <- NA
  max_barplot$ENSO <- NA
  for (each_row in 1:length(max_barplot$water_year)){
    if (each_row <= 365){
      max_barplot$Count[each_row] <- length(na.omit(unlist(maxdate_neutral)[unlist(maxdate_neutral) == max_barplot$water_year[each_row]]))
      max_barplot$ENSO[each_row] <- "Neutral"
    }
    if ((each_row > 365) & (each_row <= 730)){
      max_barplot$Count[each_row] <- length(na.omit(unlist(maxdate_nino)[unlist(maxdate_nino) == max_barplot$water_year[each_row]]))
      max_barplot$ENSO[each_row] <- "Nino"
    }
    if ((each_row > 730) & (each_row <= 1095)){
      max_barplot$Count[each_row] <- length(na.omit(unlist(maxdate_nina)[unlist(maxdate_nina) == max_barplot$water_year[each_row]]))
      max_barplot$ENSO[each_row] <- "Nina"
    }
  }
  plot1 <- ggplot(data = max_barplot, aes(x = water_year, y = Count, fill = ENSO)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Date") +
    ylab("Count of Timing Days") +
    ggtitle("Timing of the 15-Day Maximum Flow")
  return(plot1)
}
