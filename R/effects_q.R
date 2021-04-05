#' Calculate the effects
#'
#' @param project_directory The directory used to save output files.
#' @param gage_list The gages used in vector form.
#' @param sum_stats_results The object returned from the function sum_anova.
#' @param mean_stats_results The object returned from the function mean_anova.
#' @param min_stats_results The object returned from the function min_anova.
#' @param max_stats_results The object returned from the function max_anova.



effects_q <- function(project_directory, gage_list, sum_stats_results, mean_stats_results, min_stats_results, max_stats_results){

  # Calculate sum effects
  sum_nino <- sum_stats_results[[4]]
  sum_nina <- sum_stats_results[[5]]
  sum_neutral <- sum_stats_results[[6]]
  sum_nino$effects <- rowMeans(sum_nino)
  sum_nina$effects <- rowMeans(sum_nina)
  sum_neutral$effects <- rowMeans(sum_neutral)

  # calculate mean effects
  mean_nino <- mean_stats_results[[4]]
  mean_nina <- mean_stats_results[[5]]
  mean_neutral <- mean_stats_results[[6]]
  mean_nino$effects <- rowMeans(mean_nino)
  mean_nina$effects <- rowMeans(mean_nina)
  mean_neutral$effects <- rowMeans(mean_neutral)

  # calculate max effects
  max_nino <- max_stats_results[[4]]
  max_nina <- max_stats_results[[5]]
  max_neutral <- max_stats_results[[6]]
  max_nino$effects <- rowMeans(max_nino)
  max_nina$effects <- rowMeans(max_nina)
  max_neutral$effects <- rowMeans(max_neutral)

  # calculate min effects
  min_nino <- min_stats_results[[4]]
  min_nina <- min_stats_results[[5]]
  min_neutral <- min_stats_results[[6]]
  min_nino$effects <- rowMeans(min_nino)
  min_nina$effects <- rowMeans(min_nina)
  min_neutral$effects <- rowMeans(min_neutral)

  # Effects dataframe
  effects_df <- as.data.frame(cbind(gage_list,
                                    sum_nino$effects,sum_nina$effects,sum_neutral$effects,
                                    mean_nino$effects,mean_nina$effects,mean_neutral$effects,
                                    max_nino$effects,max_nina$effects,max_neutral$effects,
                                    min_nino$effects,min_nina$effects,min_neutral$effects))

  # Name columns
  colnames(effects_df) <- c("gage",
                            "sum_nino","sum_nina","sum_neutral",
                            "mean_nino","mean_nina","mean_neutral",
                            "max_nino","max_nina","max_neutral",
                            "min_nino","min_nina","min_neutral")

  # write out effects df to project directory
  write.csv(effects_df, paste0(project_directory,"signals_df.csv"))

  # return the effects df from the function
  return(effects_df)
}
