

build_bet_placement_matrix <- function(probs, odds, min_advantage, max_odds) {
  
  ## Error handling
  
  if (!is.matrix(probs) & !is.data.frame(probs)) {
    
    stop ("'probs' must be a matrix, data.frame or tibble.")
    
  }
  
  if (!is.matrix(odds) & !is.data.frame(odds)) {
    
    stop ("'odds' must be a matrix, data.frame or tibble.")
    
  } 
  
  num_matches <- nrow(probs)
  num_outcomes <- ncol(probs)
  
  if (nrow(odds) != num_matches) {
    
    stop(glue("'probs' and 'odds' must have the same number of rows: {num_matches} != {nrow(odds)}."))
    
  }
  
  if (ncol(odds) != num_outcomes) {
    
    stop(glue("'probs' and 'odds' must have the same number of cols: {num_outcomes} != {ncol(odds)}."))
    
  }
  
  
  if (!is.numeric(min_advantage)) {
    
    stop("'min_advantage' must be numeric.")
    
  }
  
  if (length(min_advantage) != 1) {
    
    stop(glue("'min_advantage' must have length 1 not {length(min_advantage)}"))
    
  } 
  
  if (min_advantage >= 1 | min_advantage <= - 1) {
    
    stop(glue("'min_advantage' must be -1 < min_advantage < 1 not {min_advantage}."))
    
  }
  
  ## Build the matrix
  
  # Advantage is true probs x bookmaker odds - 1 = advantage and we want to bet on the outcome with the maximum 
  # advantage
  
  advantage_matrix <- calc_advantage(probs, odds) #  
  max_advantage_by_match <- row_max(as.data.frame(advantage_matrix), append_col = FALSE)

  bet_placement_matrix <- matrix(0, nrow = num_matches, ncol = num_outcomes) # Initialize
  
  if (is.na(max_odds)) {
    
    max_odds <- max(odds, na.rm = TRUE)
    
  }
  
  # We initialized the bet placement with 0s before. This goes through it and adds in a 1 to indicate a bet will be 
  # placed. The bet placement matrix is the same shape as the odds / closing odds / probs matrices. Bets are placed
  # when an advantage is detected. Where multiple outcomes have an advantage, the maximum advantage is picked.
  
  for (i in seq_along(1:num_matches)) {
    
    for (j in seq_along(1:num_outcomes)) {
      
      advantage <- advantage_matrix[i, j]
      
      if (advantage == max_advantage_by_match[i] & advantage >= min_advantage & odds[i, j] < max_odds) {
        
        bet_placement_matrix[i, j] <- 1
        
      }
      
    }
    
  }
  
  return(bet_placement_matrix)
  
}
