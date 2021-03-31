#' Calculate the most prevalent timing of low flow events between conditions.
#'
#' @param project_directory The directory used to save output files.
#' @export detrend



lowflow_timing <- function(project_directory){
  require(ggplot2)
  mindate_df <- read.csv(paste0(project_directory,"mindate_df.csv"), stringsAsFactors = F, row.names = 1)
  water_years <- seq(1952, 2011, by = 1)
  colnames(mindate_df) <- water_years-1
  for (column in colnames(mindate_df)){
    mindate_df[,column] <- as.Date(mindate_df[,column],format = "%m-%d")
  }
  mindate_nino <- mindate_df[,as.character(sort(nino))]
  mindate_nina <- mindate_df[,as.character(sort(nina))]
  mindate_neutral <- mindate_df[,as.character(sort(neutral))]
  water_year <- seq(as.Date("2020-01-01"),as.Date("2020-12-31"), by = "day")
  min_barplot <- as.data.frame(water_year)
  min_barplot <- rbind(min_barplot, min_barplot, min_barplot)
  min_barplot$Count <- NA
  min_barplot$ENSO <- NA
  for (each_row in 1:length(min_barplot$water_year)){
    if (each_row <= 365){
      min_barplot$Count[each_row] <- length(na.omit(unlist(mindate_neutral)[unlist(mindate_neutral) == min_barplot$water_year[each_row]]))
      min_barplot$ENSO[each_row] <- "Neutral"
    }
    if ((each_row > 365) & (each_row <= 730)){
      min_barplot$Count[each_row] <- length(na.omit(unlist(mindate_nino)[unlist(mindate_nino) == min_barplot$water_year[each_row]]))
      min_barplot$ENSO[each_row] <- "Nino"
    }
    if ((each_row > 730) & (each_row <= 1095)){
      min_barplot$Count[each_row] <- length(na.omit(unlist(mindate_nina)[unlist(mindate_nina) == min_barplot$water_year[each_row]]))
      min_barplot$ENSO[each_row] <- "Nina"
    }
  }
  plot1 <- ggplot(data = min_barplot, aes(x = water_year, y = Count, fill = ENSO)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Date") +
    ylab("Count of Timing Days") +
    ggtitle("Timing of the 7-Day Minimum Flow")
  return(plot1)
}
