calc_EOQ <- function(D = 1000) {
  K <- 5
  h <- 0.25
  Q <- sqrt(2*D*K/h)
  Q
}
calc_EOQ(D=4000)
