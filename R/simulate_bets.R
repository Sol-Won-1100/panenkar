
#' @title Simulate Bets
#' @description Simulate advantage bets
#' @param probs Tibble, data.frame or matrix of estimated match probabilities. Columns are outcomes, rows are matches.
#' @param odds Tibble, data.frame or matrix of bookmakers odds. Columns are outcomes, rows are matches.
#' @param outcomes Factor, the levels of which correspond to a column in probs and odds. They must all be ordered in the
#'                 same way.
#' @param closing_odds Tibble, data.frame or matrix of bookmakers odds. These are the odds at close and not necessarily
#'                     bet on. These are used to check the closing line value (CLV) of the bets made.
#' @param min_advantage Minimum advantage needed before a bet is placed. Default: 0.1.
#' @param start_bank Starting bankroll units. Default: 100.
#' @param stake Stake to place per bet. Default: 1.
#' @param max_odds Maximum odds which a bet is placed it. You might limit odds to lower variance. Default: 5.
#' @param .seed set a seed, good for testing the sampling in the CLV part or to get reproducible CLV stats
#' @return A list of betting statistics.
#' @details DETAILS
#' @examples 
#' 
#' probs <- matrix(runif(9), nrow = 3)
#' odds <- matrix(runif(9, min = 1, max = 25), nrow = 3)
#' outcomes <- factor(sample(c("home", "draw", "away"), size = 9, replace = TRUE), levels = c("home", "draw", "away"))
#' 
#' betting_stats <- simulate_bets(probs, odds, outcomes)
#'
#' @rdname simulate_bets
#' @export 

simulate_bets <- function (probs, odds, outcomes, closing_odds = NA, min_advantage = 0.05, start_bank = 100, stake = 1, 
                           max_odds = NA) {
  
  ## Error handling

  if (!is_matrix_df_tibble(probs)) stop ("'probs' must be a matrix, data.frame or tibble")
  if (!is_matrix_df_tibble(odds)) stop ("'odds' must be a matrix, data.frame or tibble")
    
  if (!is_matrix_df_tibble(closing_odds)) {
    
    if (!is.na(closing_odds[1])) {
      
      stop ("'closing odds' must be a matrix, data.frame or tibble.")
      
    }
    
    closing_odds_supplied <- FALSE
    
    if (!is_same_size(probs, odds)) {
      
      stop("'probs' and 'odds' must have the same number of rows and cols")
      
    }
      
  } else {
    
    if (!is_same_size(probs, odds, closing_odds)) {
      
      stop("'probs', 'odds' and 'closing_odds'  must have the same number of rows and cols")
      
    }
    
    closing_odds_supplied <- TRUE
    
  }  
  
  probs <- as.matrix(probs)
  odds <- as.matrix(odds)
  closing_odds <- as.matrix(closing_odds)
  
  num_matches <- nrow(probs)
  num_outcomes <- ncol(probs)
  
  if (length(outcomes) != num_matches) {
    
    stop(glue("'probs' must have the same number of rows as the length of outcomes: {num_matches} != {length(outcomes)}."))    
    
  } 
  
  if (length(levels(outcomes)) != num_outcomes) {
    
    stop(glue("'probs' must have the same number of cols as the length of the levels of outcomes: {num_outcomes} != {length(levels(outcomes))}."))     
    
  }
  
  probs <- as.matrix(probs, nrow = num_matches, ncol = num_outcomes)
  odds <- as.matrix(odds, nrow = num_matches, ncol = num_outcomes)
  
  # Probabilities for a given match must sum to 1. However, some tolerance in this may be needed depending on how they 
  # were calculated. Default: 3. This means a matches probabilities would not be flagged with they sum to 1+-0.001.
  
  tolerance_digits <- 3
  
  probs_sum_by_match <- probs %>% rowSums() %>% round(tolerance_digits) 
  probs_sum_by_match <- probs_sum_by_match[!is.na(probs_sum_by_match)]         
  probs_sum_by_match <- probs_sum_by_match[probs_sum_by_match != 1]                          
    
  if (length(probs_sum_by_match) > 0) {
    
    warning("all rows of 'probs' do not sum to 1 add tolerance level of {tolerance_digits} d.p.")
    
  }

  if (length(probs[probs < 0 & !is.na(probs)]) > 0)  stop("'probs' elements must be greater than or equal to 0.")
  if (length(probs[probs > 1 & !is.na(probs)]) > 0) stop("'probs' elements must be less than or equal to 1.")
  if (length(odds[odds <= 1 & !is.na(odds)]) > 0) stop("'odds' elements must be greater than 1.")
  if (!is.numeric(min_advantage)) stop("'min_advantage' must be numeric.")
  if (length(min_advantage) != 1) stop(glue("'min_advantage' must have length 1 not {length(min_advantage)}"))
  
  if (min_advantage >= 1 | min_advantage <= - 1){
    
    stop(glue("'min_advantage' must be -1 < min_advantage < 1 not {min_advantage}."))
    
  }
  
  if (min_advantage <= 0)  warning("'min_advantage' is <= 0, expected > 0 but have simulated anyway")
  if (length(max_odds) != 1) stop(glue("'max_odds' must have length 1 not {length(max_odds)}"))
  if (is.na(max_odds)) max_odds <- max(odds, na.rm = TRUE) + 1
  if (!is.numeric(max_odds)) stop("'max_odds' must be numeric.")
  if (max_odds <= 1) stop(glue("'max_odds' must be >= 1 not {max_odds}."))
  if (length(start_bank) != 1) stop(glue("'start_bank' must have length 1 not {length(max_odds)}"))
  if (!is.numeric(start_bank)) stop("'start_bank' must be numeric.")
  if (start_bank <= 0) warning("start_bank' is <= 0, expected > 0 but have simulated anyway")
    
  
  ## Simulate bets
  
  bet_placement_matrix <- build_bet_placement_matrix(probs, odds, min_advantage, max_odds)
  stake_matrix <- bet_placement_matrix * stake # In the case where stake = 1, bet_placement_matrix = stake_matrix
  indicator_matrix <- build_indicator_matrix(outcomes) 
  win_matrix <- bet_placement_matrix * indicator_matrix
  
  ## Derive betting statistics
  
  num_bets <- sum(bet_placement_matrix, na.rm = TRUE)
  bet_percentage <- num_bets / num_matches
  num_bets_by_outcome <- numeric(length = num_outcomes)
  
  for (i in seq_along(1:num_outcomes)) {
    
    num_bets_by_outcome[i] <- sum(bet_placement_matrix[, i], na.rm = TRUE)
    
  }

  bet_percentage_by_outcome <- num_bets_by_outcome / num_matches
  
  # Dont want any zeroes or nas only the odds we actually bet on, by defintion they will be > 1
  
  average_odds_for_bet_all <- as.vector(bet_placement_matrix * odds)
  average_odds_for_bet_all <- average_odds_for_bet_all[!is.na(average_odds_for_bet_all)] 
  average_odds_for_bet_all <- average_odds_for_bet_all[average_odds_for_bet_all != 0] 
  
  average_odds_for_bet <- average_odds_for_bet_all %>% mean(na.rm = TRUE)

  distribution_odds_for_bet <- tibble(odds = average_odds_for_bet_all) %>% 
    group_by(odds) %>% 
    summarise(count = n(), .groups = "drop_last")
  
  # Dont want any zeroes or NAs only the odds we actually won on, by defintion they will be > 1
  
  average_odds_for_win_all <- as.vector(win_matrix * odds)
  average_odds_for_win_all <- average_odds_for_win_all[!is.na(average_odds_for_win_all)] 
  average_odds_for_win_all <- average_odds_for_win_all[average_odds_for_win_all != 0] 
  
  average_odds_for_win <- average_odds_for_win_all %>% mean(na.rm = TRUE) %>% round(2)
  
  distribution_odds_for_win <- tibble(odds = average_odds_for_win_all) %>% 
    group_by(odds) %>% 
    summarise(count = n(), .groups = "drop_last")
  

  profit_loss_by_match <- rowSums(stake * indicator_matrix * bet_placement_matrix * odds - stake * bet_placement_matrix, 
                                  na.rm = TRUE)
  
  num_wins <- sum(win_matrix, na.rm = TRUE)
  
  num_wins_by_outcome <- numeric(length = num_outcomes)
  
  for (i in seq_along(1:num_outcomes)) {
    
    num_wins_by_outcome[i] <- sum(win_matrix[, i], na.rm = TRUE)
    
  }
  
  win_percentage <-  (num_wins / num_bets)
  win_percentage_by_outcome <-  (num_wins_by_outcome / num_bets_by_outcome)
  
  rolling_bank <- c(start_bank, profit_loss_by_match) %>% cumsum()
  
  when_bankrupt <- which(rolling_bank <= 0)
  
  if (length(when_bankrupt) > 0) {
    
    went_bankrupt <- TRUE
    when_bankrupt <- when_bankrupt[1] - 1
    
    
  } else {
    
    went_bankrupt <- FALSE
    when_bankrupt <- NA_real_
    
  }
  
  
  rolling_stats <- tibble(match_num = 0:num_matches, 
                          odds_of_selection = c(NA_real_, rowSums(bet_placement_matrix *  odds, na.rm = TRUE)),
                          odds_of_winner = c(NA_real_, rowSums(indicator_matrix *  odds, na.rm = TRUE)),
                          bank_after_match = rolling_bank, 
                          profit_loss = c(NA_real_, profit_loss_by_match),
                          outcome = c(NA_character_, as.character(outcomes)),
                          bet_result = case_when(
                            profit_loss > 0 ~ "win",
                            profit_loss == 0 ~ "no_bet",
                            profit_loss < 0 ~ "lose",
                            TRUE ~ NA_character_)) %>%
    mutate(odds_of_selection = case_when(is_na_inf_nan(odds_of_selection) ~ NA_real_,
                                         odds_of_selection > 1 ~ odds_of_selection,
                                         odds_of_selection <= 1 ~ NA_real_,
                                         TRUE ~ NA_real_))
  
  
  profit_loss <- rolling_bank[length(rolling_bank)] - start_bank
  
  roi <- profit_loss / sum(stake_matrix, na.rm = TRUE)
  
  ## Calculate closing line value

  if (closing_odds_supplied == TRUE) {
   
    # colnames(closing_odds) <- paste0(levels(outcomes), "_closing_odds")
    
    clv_df <- tibble(actual_odds_bet = rowSums(bet_placement_matrix * odds), 
                     fair_closing_odds = rowSums(bet_placement_matrix * (1 / remove_margin(closing_odds))),
                     match_num = 1:num_matches) %>%
      filter(!is.na(fair_closing_odds), fair_closing_odds != 0) %>%
      mutate(clv_advantage  = actual_odds_bet / fair_closing_odds - 1)
    
    # Our expected advantage if we accept that the closing line odds, adjusted to be fair using the proportional 
    # remove_margin method, are an accurate estimate.
    
    clv_advantage <- mean(clv_df$clv_advantage, na.rm = T)
    
    first_cl_position <- clv_df %>%
      select(match_num) %>%
      unlist() %>%
      min()
    
    rolling_stats <- rolling_stats %>%
      left_join(select(clv_df, fair_closing_odds, match_num, clv_advantage), by = "match_num") %>%
      mutate(bet_placed_closing = if_else(!is.na(fair_closing_odds), 1, 0) * stake,
             expected_bank_after_match_clv = bank_after_match)
    
    for (i in first_cl_position:(num_matches + 1)) {
      
      if (i > 1) {
        
        rolling_stats[i, "expected_bank_after_match_clv"] <- rolling_stats[i - 1, "expected_bank_after_match_clv"] + 
          (rolling_stats[i, "bet_placed_closing"] * clv_advantage)
        
        
      }
      
    }
    
    rolling_stats$expected_bank_after_match_clv <- round(rolling_stats$expected_bank_after_match_clv, 2)
    
    # The following tests the closing line value. This is based on this article
    #
    # https://www.pinnacle.com/en/betting-articles/Betting-Strategy/using-closing-line-to-test-betting-skill/7E6JWJM5YKEJUWKQ
    # A basic summary is that we draw randomly and analyze the odds / closing odds ratio to determine if what we are 
    # seeing is evidence of skill or simply random chance. If the ratio (1 - CLV advantage)
    
    combined_odds <- cbind(odds, closing_odds)
    
    # Filter out rows where there are NAs. We are doing some classic stats testing here so any 1 match isnt important
    
    combined_odds <- combined_odds[complete.cases(combined_odds),] 
    
    odds_cols <- 1:num_outcomes
    closing_cols <- seq(num_outcomes + 1, 2 * num_outcomes) 
    
    combined_odds <- list(combined_odds[, odds_cols], combined_odds[, closing_cols])
    
    combined_odds[[2]] <- 1 / remove_margin(combined_odds[[2]]) # Turn these into fair closing odds
    
    # We want to sample from each outcome the same number of bets we actually placed for a good comparison rather than
    # just randomly from each outcome because often one outcome might be generally favoured like home

    sample_indices <- 1:nrow(combined_odds[[1]]) %>% 
      sample(size = sum(num_bets_by_outcome), replace = TRUE) %>%
      split(rep(1:num_outcomes, num_bets_by_outcome))

    odds_sample <- list()
    
    j <- 0
    
    for (i in seq_along(1:num_outcomes)) {
      
      sample_indices_outcome <- sample_indices[[as.character(i)]]
      
      if(length(sample_indices_outcome) > 0) {
        
        j <- j + 1
        
        odds_sample[[j]] <- tibble(odds = unlist(combined_odds[[1]][sample_indices_outcome, i]),
                                   closing = unlist(combined_odds[[2]][sample_indices_outcome, i]))
        
      }
      
    }
    
    odds_sample <- odds_sample %>% 
      bind_rows() %>%
      mutate(ratio = odds / closing)
    
    ratio_mean_sample <- mean(odds_sample$ratio)
    ratio_sd_sample <- sd(odds_sample$ratio)
    ratio_standard_error <- ratio_sd_sample / (num_bets) ^ 0.5
    
    sd_bounds <- tribble(
      ~p,   ~lower,                                       ~mean,              ~upper,
      68,   ratio_mean_sample - ratio_standard_error,     ratio_mean_sample,  ratio_mean_sample + ratio_standard_error,
      95,   ratio_mean_sample - 2 * ratio_standard_error, ratio_mean_sample,  ratio_mean_sample + 2 * ratio_standard_error,
      99.7, ratio_mean_sample - 3 * ratio_standard_error, ratio_mean_sample,  ratio_mean_sample + 3 * ratio_standard_error
    )
    
    sd_bounds <- sd_bounds %>% 
      mutate(actual_ratio = 1 + clv_advantage,
             info = case_when(
               actual_ratio >= lower & actual_ratio <= upper ~ glue("Ratio between bounds, no evidence of skill"),
               actual_ratio >  upper ~ "Ratio above bounds, evidence of skill",
               actual_ratio <  lower ~ "Ratio below bounds, no evidence of skill",
               TRUE ~ "Error"))
    
    bank_names <- c("bank_after_match", "expected_bank_after_match_clv")
    .title <- "Rolling Bank with Expected Bank from CLV"
    .colours <- c("#FF0000", "#000000")
    
    rolling_stats <- select(rolling_stats, -fair_closing_odds, -bet_placed_closing)
    y_lim <- max(c(rolling_stats$bank_after_match, rolling_stats$expected_bank_after_match_clv), na.rm = TRUE)

  } else {
    
    bank_names <- "bank_after_match"
    .title <- "Rolling Bank"
    .colours <- "#FF0000"
    clv_advantage <- NA
    sd_bounds <- NA
    sample_indices <- NA
    y_lim <- max(c(rolling_stats$bank_after_match), na.rm = TRUE)
    
  }
  
  p_rolling_bank <- rolling_stats %>%
    pivot_longer(cols = all_of(bank_names), names_to = "bank_type", values_to = "bank") %>%
    mutate(bank_type = factor(bank_type, levels = rev(bank_names))) %>%
    ggplot(aes(x = match_num, y = bank, group = bank_type, colour = bank_type)) +
    geom_line() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.grid.major.y = element_line(colour = "#EADDDD"),
          axis.text = element_text(size = 12),
          axis.title.y = element_text(size = 12, margin = margin(0, 18, 0, 0)),
          plot.title = element_text(size = 16, margin = margin(0, 0, 10, 0), face = "bold"),
          plot.subtitle = element_text(size = 12, margin = margin(0, 0, 10, 0)),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none") +
    scale_y_continuous(limits =  c(0, max(y_lim, na.rm = TRUE)), 
                       expand = c(0, max(y_lim, na.rm = TRUE) * 0.05)) +
    labs(title = .title, x = "", y = "") +
    scale_color_manual(values = .colours) 
  
  return(list(profit_loss = profit_loss, 
              roi = roi, 
              went_bankrupt = went_bankrupt,
              when_bankrupt = when_bankrupt,
              rolling = rolling_stats,
              num_bets = num_bets,
              bet_percentage = bet_percentage,
              num_bets_by_outcome = num_bets_by_outcome,
              bet_percentage_by_outcome = bet_percentage_by_outcome,
              average_odds_for_bet = average_odds_for_bet,
              num_wins = num_wins,
              win_percentage = win_percentage,
              num_wins_by_outcome = num_wins_by_outcome,
              win_percentage_by_outcome = win_percentage_by_outcome,
              average_odds_for_win = average_odds_for_win,
              p_rolling_bank = p_rolling_bank,
              clv_advantage = clv_advantage,
              clv_stats_tests = sd_bounds,
              matches_sampled_for_clv_test = sample_indices))
  
}


#' @title Build Bet Placement Matrix
#' @description Build a bet placement matrix
#' @param probs Tibble, data.frame or matrix of estimated match probabilities. Columns are outcomes, rows are matches.
#' @param odds Tibble, data.frame or matrix of bookmakers odds. Columns are outcomes, rows are matches.
#' @param min_advantage Minimum advantage needed before a bet is placed. Default: 0.1.
#' @param max_odds Maximum odds which a bet is placed it. You might limit odds to lower variance. Default: 5.
#' @return A matrix the same size as the probs and odds matrices, composed of 1s and 0s indicating where a bet has been 
#' placed
#' @rdname build_bet_placement_matrix

build_bet_placement_matrix <- function(probs, odds, min_advantage, max_odds) {
  
  ## Error handling
  
  if (!is_matrix_df_tibble(probs)) stop ("'probs' must be a matrix, data.frame or tibble.")
  if (!is_matrix_df_tibble(odds)) stop ("'odds' must be a matrix, data.frame or tibble.")
    
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
  
  if (length(max_odds) != 1) {
    
    stop(glue("'max_odds' must have length 1 not {length(max_odds)}"))
    
  } 
  
  if (is.na(max_odds)) {
    
    max_odds <- max(odds, na.rm = TRUE) + 1
    
  }
  
  if (!is.numeric(max_odds)) {
    
    stop("'max_odds' must be numeric.")
    
  }
  
  
  if (max_odds <= 1) {
    
    stop(glue("'max_odds' must be >= 1 not {max_odds}."))
    
  }
  
  ## Build the matrix
  
  # Advantage is true probs x bookmaker odds - 1 = advantage and we want to bet on the outcome with the maximum 
  # advantage
  
  advantage_matrix <- calc_advantage(probs, odds)  
  max_advantage_by_match <- row_max(as.data.frame(advantage_matrix), append_col = FALSE)
  
  bet_placement_matrix <- matrix(0, nrow = num_matches, ncol = num_outcomes) # Initialize
  
  # We initialized the bet placement with 0s before. This goes through it and adds in a 1 to indicate a bet will be 
  # placed. The bet placement matrix is the same shape as the odds / closing odds / probs matrices. Bets are placed
  # when an advantage is detected. Where multiple outcomes have an advantage, the maximum advantage is picked.

  for (i in seq_along(1:num_matches)) {
    
    odds_i <- unlist(odds[i,])
    probs_i <- unlist(probs[i,])
    
    # If any of the odds or probs for that match are NAs then no bets are placed
    
    if (sum(is_na_inf_nan(odds_i)) == 0 & sum(is_na_inf_nan(probs_i)) == 0) {
      
      for (j in seq_along(1:num_outcomes)) {
        
        advantage <- advantage_matrix[i, j]
        
        if (advantage == max_advantage_by_match[i] & advantage >= min_advantage & odds[i, j] < max_odds) {
          
          bet_placement_matrix[i, j] <- 1
          
        }
        
      }
      
    }
    
  }
  
  return(bet_placement_matrix)
  
}

#' @title Build Indicator Matrix
#' @description Build an indicator matrix of 1s and 0s based on a vector of outcomes
#' @param x vector of outcomes, must be a factor
#' @return An indicator matrix
#' @details Each column is a level from x. A 1 indicates the event occurred, a 0 it didn't.
#' @rdname build_indicator_matrix

build_indicator_matrix <- function(x) {
  
  if (!is.factor(x)) {
    
    stop("'x' must be a factor")
    
  }
  
  values <- levels(x)
  
  indicator <- matrix(0, nrow = length(x), ncol= length(values))
  
  colnames(indicator) <- values
  
  
  for (i in seq_along(1:ncol(indicator))) {
    
    indicator[x == values[i], values[i]] <- 1
    
  }
  
  indicator[is.na(x),] <- NA
  
  return(indicator)
  
}


