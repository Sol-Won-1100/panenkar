
update_elo_odds <- function(elo_home, elo_away, actual_home, actual_draw, 
                            actual_away, advantage_home = 80, x = 10, y = 400, 
                            z = 175){
  
  if(round(sum(actual_home, actual_draw, actual_away),4) != 1){
    stop("sum of actuals must = 1")
  }
  
  expected_home <- 1 / (1 + x ^ ((elo_away - elo_home - advantage_home) / y))
  expected_away <- 1 - expected_home
  
  actual_home <- actual_home + actual_draw * 0.5
  actual_away <- actual_away + actual_draw * 0.5
  
  elo_home_new <- elo_home + z * (actual_home - expected_home)
  elo_away_new <- elo_away + z * (actual_away - expected_away)
  
  return(c(elo_home_new, elo_away_new))
}
