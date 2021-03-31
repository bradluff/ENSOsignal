#' Calculate the effects
#'
#' @param project_directory The directory used to save output files.
#' @param gage_list The gages used in vector form.
#' @param sum_anova_results The object returned from the function sum_anova.
#' @param mean_anova_results The object returned from the function mean_anova.
#' @param min_anova_results The object returned from the function min_anova.
#' @param max_anova_results The object returned from the function max_anova.
#' @export detrend



effects_q <- function(project_directory, gage_list, sum_anova_results, mean_anova_results, min_anova_results, max_anova_results){
  # Calculate effects
  sum_nino <- sum_anova_results[[4]]
  sum_nina <- sum_anova_results[[5]]
  sum_neutral <- sum_anova_results[[6]]
  sum_nino$effects <- rowMeans(sum_nino)
  sum_nina$effects <- rowMeans(sum_nina)
  sum_neutral$effects <- rowMeans(sum_neutral)
  mean_nino <- mean_anova_results[[4]]
  mean_nina <- mean_anova_results[[5]]
  mean_neutral <- mean_anova_results[[6]]
  mean_nino$effects <- rowMeans(mean_nino)
  mean_nina$effects <- rowMeans(mean_nina)
  mean_neutral$effects <- rowMeans(mean_neutral)
  max_nino <- max_anova_results[[4]]
  max_nina <- max_anova_results[[5]]
  max_neutral <- max_anova_results[[6]]
  max_nino$effects <- rowMeans(max_nino)
  max_nina$effects <- rowMeans(max_nina)
  max_neutral$effects <- rowMeans(max_neutral)
  min_nino <- min_anova_results[[4]]
  min_nina <- min_anova_results[[5]]
  min_neutral <- min_anova_results[[6]]
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
  # write out effects df
  write.csv(effects_df, paste0(project_directory,"signals_df.csv"))
  return(effects_df)
}
