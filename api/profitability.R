## Scoring ----
scoring_categories <- function(score, thresholds = c(0.33, 0.66), categories = NULL, outer_boundaries = c(0, 1)) {
  # Sanitation
  if (thresholds[[1]] != min(outer_boundaries)) thresholds <- c(min(outer_boundaries), thresholds)
  if (thresholds[[length(thresholds)]] != max(outer_boundaries)) thresholds <- c(thresholds, max(outer_boundaries))

  thresholds[thresholds < min(outer_boundaries)] <- min(outer_boundaries)
  thresholds[thresholds > max(outer_boundaries)] <- max(outer_boundaries)
  
  # Scoring
  scoring_category <- cut(score, thresholds, labels = categories, include.lowest = TRUE)
  
  return(scoring_category)
}



## Categories ----
initial_scoring_categories <- function(score, thresholds = c(0.4, 0.6)) {
  initial_categories <- c("Förmodligen <em>inte</em> lönsamt", "Slantsingling", "Förmodligen lönsamt")
  
  scoring_categories(score, thresholds, initial_categories)
}


detailed_scoring_categories <- function(score, thresholds = c(0.15, 0.4, 0.6, 0.85)) {
  detailed_categories <- c(
    "Nästan säkert <em>inte</em> lönsamt",
    "Förmodligen <em>inte</em> lönsamt",
    "Slantsingling",
    "Förmodligen lönsamt",
    "Nästan säkert lönsamt"
  )
  
  scoring_categories(score, thresholds, detailed_categories)
}


cost_comparison_categories <- function(score, thresholds = c(0.8, 0.95, 1.05, 1.2)) {
  detailed_categories <- c(
    "Mycket billigare med elbil",
    "Lite billigare med elbil",
    "Ungefär lika dyrt",
    "Lite dyrare med elbil",
    "Mycket dyrare med elbil"
  )
  
  scoring_categories(score, thresholds, detailed_categories, outer_boundaries = c(0, Inf))
}
