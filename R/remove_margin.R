
#' @title Remove Margin
#' @description Remove the bookmakers margin from a set of odds
#' @param x a vector, data.frame, tibble or matrix of decimal odds
#' @param method one of 'proportional' or 'straight'. Default: 'proportional'
#' @return OUTPUT_DESCRIPTION
#' @details the 'proportional' method detailed here: https://www.football-data.co.uk/The_Wisdom_of_the_Crowd_updated.pdf 
#' but can also be the 'straight' method. The  proportional method is thought to be superior, straight method is just 
#' divide everything by the over round.
#' @rdname remove_margin
#' @export 

remove_margin <- function(x, method = "proportional") {
  
  if (!is.character(method)) {
    
    stop("'method' must be of class character.")
    
  }
  
  if (length(method) != 1) {
    
    stop(glue("'method' must be of length 1 not {length(method)}."))
    
  }
  
  if (method != "proportional" & method != "straight") {
    
    stop(glue("'method' must be one of 'proportional' or 'straight' not '{method}'."))
    
  }
  
  if (!is_plain_vector(x) & !is_matrix_df_tibble(x)) {
    
    stop("x must be a plain vector, matrix, data.frame or tibble")
    
  }  
  
  x_structure <- class(x)[1]

  if (is_plain_vector(x)) {

    x <- matrix(x, nrow = 1)
    x_structure <- "vector"
    
  }

  if (ncol(x) < 2) stop ("'x' must have at least 2 outcomes")
  
  if (is.data.frame(x)) {
    
    x <- as.matrix(x)
    
  }
  
  if (!is.numeric(x)) stop ("'x' must be numeric")

 # cols_with_nas <- unique(which(is.na(x), arr.ind = TRUE)[, "col"]) 
  
  if (length(x[x <= 1 & !is.na(x)]) != 0) stop ("'x' values must be > 1")
  
  if (method == "proportional") {
    
    # Matches with no margin are not suitable for using the proportional method and the implied_probabilities
    # function from the implied package throws an error when you try and use them. So we have 2 groups. Group 1 must use
    # the straight method and group 2 can use the proportional method. Group 2 is split into 2 further sub groups, 
    # 1 is standard and 1 is problematic

    matches_with_method_required <- x %>%
      as.data.frame() %>%
      as_tibble() %>%
      mutate(row_sums = rowSums(1 / x), 
             row_num = 1:n(),
             method_group = case_when(
               is.na(row_sums) ~ "proportional",
               row_sums > 1 ~ "proportional",
               row_sums <= 1 ~ "straight"
             ),
             method_group = factor(method_group, levels = c("straight", "proportional")))
    
    matches_group1 <- filter(matches_with_method_required, method_group == "straight")
    matches_group2 <- filter(matches_with_method_required, method_group == "proportional")
    
    if (nrow(matches_group1) > 0) { 
   
      x_straight <- x[matches_group1$row_num, ,drop = FALSE]
 
      x_straight_margins <- matrix(rep(rowSums(1 / x_straight), ncol(x)), byrow = FALSE, nrow = nrow(x_straight))
      
      x_straight_no_margin <- ((1 / x_straight) / x_straight_margins) %>%
        as.data.frame() %>%
        as_tibble() %>%
        mutate(row_num = matches_group1$row_num)
      
      x <- x[matches_group2$row_num, ,drop = FALSE]
      
    } 
    
    
    
    suppressWarnings(
      implied_probs_output <- implied_probabilities(x, method = "wpo")
    )
    
    # Some matches with very low or high odds will not work with the proportion method but dont throw an error. Find 
    # these and pass them through the function using the straight method
    
    problem_matches <- which(implied_probs_output$problematic == TRUE)
    
    prob_no_margin <- implied_probs_output$probabilities
    
    if (length(problem_matches) > 0) {
      
      prob_no_margin[problem_matches,] <- implied_probabilities(x[problem_matches,])$probabilities
      
    }
    
    # Finally join them all together
    
    if (nrow(matches_group1) > 0) { # Length 2 means some in the "straight" group were detected - need to bind
      
      prob_no_margin <- prob_no_margin %>%
        as.data.frame() %>%
        as_tibble() %>%
        mutate(row_num = matches_group2$row_num) %>%
        bind_rows(x_straight_no_margin) %>%
        arrange(row_num) %>%
        select(-row_num) %>%
        as.matrix()
      
    }
    
  } else {
   
    x_margins <- matrix(rep(rowSums(1 / x), ncol(x)), byrow = FALSE, nrow = nrow(x))
    
    prob_no_margin <- ((1 / x) / x_margins)
    
  }
  
  
  # 
  # 
  # 
  # 
  # x <- t(as.matrix(x))
  # 
  # prob_with_margin <- 1 / x
  # 
  # margin_m <- prob_with_margin %>% 
  #   colSums() %>% 
  #   subtract(1) %>%
  #   rep(each = nrow(x)) %>% 
  #   matrix(nrow = nrow(x), byrow = FALSE)
  # 
  # if (method == "straight") {
  #   
  #   prob_no_margin <- prob_with_margin / (1 + margin_m)
  #   
  # } else  {
  #   
  #   # Proportional method
  #   
  #   num_selections <- nrow(x)
  #   prob_no_margin <- (prob_with_margin * num_selections - margin_m) / num_selections
  #   
  #   
  # } 
  # 
  # prob_no_margin <- t(prob_no_margin)
  
  if (x_structure == "data.frame") {

    return(as.data.frame(prob_no_margin))
    
  } else if (x_structure == "tbl_df") {
    
    return(tibble(as.data.frame(prob_no_margin)))
    
  } else if (x_structure == "vector") {
    
    return(c(prob_no_margin))
    
  } else {
    
    return(prob_no_margin)
    
  }

}

