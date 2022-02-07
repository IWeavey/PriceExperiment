#' Finds the optimal price and corresponding reward given a set of prices and
#' true unknown utility distribution (optimal is taken from set of prices).'
#' @export
#'
#' @param TestPrices Set of prices being tested
#' @param ShapeA Shape parameter of distribution
#' @param ShapeB Shape parameter of distribution
#' @return Vector containing the optimal price and corresponding reward
OptimalChoice <- function(TestPrices, ShapeA, ShapeB){
  Rewards = TestPrices*(1 - stats::pbeta(TestPrices, ShapeA, ShapeB))
  return(c(TestPrices[which.max(Rewards)], max(Rewards)))
}
