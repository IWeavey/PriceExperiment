#' Picks an action using a balanced experiment approach.'
#' @export
#'
#' @param t Current iteration of experiment
#' @param TestPrices Set of prices being tested
#' @return An index, k, corresponding to the action chosen
Balanced <- function(t, TestPrices){
  k = ifelse(t %% length(TestPrices) == 0, length(TestPrices), t %% length(TestPrices))
  return (k)
}


#' Picks a price given past experiment data using the UCB1-Tuned-Price Algorithm.'
#' @export
#'
#' @param ArmDemands List of vectors containing complete purchase decisions for each price
#' @param TestPrices Set of prices being tested
#' @return An index, k, corresponding to the action chosen
UCB <- function(ArmDemands, TestPrices){
  n_t              = lengths(ArmDemands)
  t                = sum(n_t)
  V_at1            = sapply(sapply(ArmDemands, function(x) x^2), sum)/n_t
  V_at2            = sapply(ArmDemands, mean)^2
  V_at3            = (2*log(t)/n_t)^(0.5)
  V_at             = (TestPrices^2)*(V_at1 - V_at2) + V_at3
  ExplorationBonus = TestPrices * ((log(t)/n_t) *
                                     pmin(rep(0.25, length(TestPrices)), V_at))^0.5
  ExpectedRewards  = sapply(ArmDemands, mean)*TestPrices
  ActionRewards    = ExpectedRewards + ExplorationBonus
  k                = which.max(ActionRewards)
  return(k)
}

#' Picks an action given past experiment data using Bernoulli Thompson Sampling.'
#' @export
#'
#' @param ArmDemands List of vectors containing complete purchase decisions for each price
#' @param TestPrices Set of prices being tested
#' @return An index, k, corresponding to the action chosen
TS <- function(ArmDemands, TestPrices){
  Successes     = sapply(ArmDemands, sum)
  Failures      = sapply(ArmDemands, length) - Successes
  RandomDraws   = unlist(Map(stats::rbeta, 1, Successes + 1, Failures + 1))
  ActionRewards = TestPrices*RandomDraws
  k             = which.max(ActionRewards)
  return(k)
}
