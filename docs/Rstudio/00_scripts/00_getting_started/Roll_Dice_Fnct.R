
roll_die <- function(faces = 1:6, number_of_dice = 1) {

  dice <- sample(x=faces, size = number_of_dice, replace = TRUE,
                 prob = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5))
  
  sum(dice)
  
}

roll_die()

results <- replicate(n = 1000, expr = roll_die(), simplify=TRUE)
hist(results)



