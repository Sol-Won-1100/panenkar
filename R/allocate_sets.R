
#' Allocate Sets
#' 
#' Create a variable which splits a data frame or tibble into different sets 
#' based on a supplied vector of probabilties which correspond to the size of 
#' each set
#' 
#' @param results results database, likely filtered to only contain a single
#' competition_id
#' @param set_sizes a vector of decimals, each of which corresponds to the 
#' proportions of the rows of results to be allocated to that set. The names of
#' each element if included.
#' 
#' @return results with a column named set
#' 
#' @export

allocate_sets <- function(results, 
                          set_sizes = c(set1 = 0.1, set2 = 0.3, set3 = 0.6)) {
  
  if (is.null(names(set_sizes))) {
    names(set_sizes) <- paste0("set", 1:length(set_sizes))
  }
  
  num_matches <- nrow(results)
  
  set_sizes <- set_sizes/sum(set_sizes)
  set_percentile_end <- set_sizes
  
  for (i in 2:length(set_percentile_end)) {
    set_percentile_end[i] <- set_percentile_end[i] + set_percentile_end[i - 1]
  }
  
  set_percentile_start <- c(0, set_percentile_end[-length(set_percentile_end)])
  
  sets_start_row <- 1:num_matches %>% 
    quantile(set_percentile_start) %>% 
    round()
  
  results$set <- ""
  
  for (i in 1:length(sets_start_row)) {
    results <- results %>% 
      mutate(set = if_else(row_number() >= sets_start_row[i], 
                           names(set_sizes)[i], set))
  }
  
  return(results)
}
