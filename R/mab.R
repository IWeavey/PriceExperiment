#' Picks an action using a balanced experiment approach.'
#' @export
#'
#' @param Policy Price decision rule (choose Balanced1, UCB1 or TS1)
#' @param TestPrices Set of prices being tested
#' @param NumIter Number of customers in the experiment
#' @param ShapeA Shape parameter of distribution
#' @param ShapeB Shape parameter of distribution
#' @param Seed Seed number for replication
#' @return List containing ArmDemands, Actions, Rewards, Demands
MAB <- function(Policy, TestPrices, NumIter, ShapeA, ShapeB, Seed){

  # Setup
  set.seed(Seed)
  Actions = Demands = rep(NA, NumIter)
  ArmDemands        = vector("list", length(TestPrices))
  Action2IndexHash  = hash::hash(seq(length(TestPrices)), TestPrices)
  Valuations        = replicate(length(TestPrices), stats::rbeta(NumIter, ShapeA, ShapeB))

  # Try each price once
  for (t in 1:length(TestPrices)){
    k               = Balanced(t, TestPrices)
    Action          = Action2IndexHash[[as.character(k)]]
    Demand          = ifelse(Action < Valuations[1,k], 1, 0)
    Actions[t]      = Action
    Demands[t]      = Demand
    ArmDemands[[k]] = c(ArmDemands[[k]], Demand)
  }

  # Run the Bandit
  for (t in (length(TestPrices) + 1): NumIter){
    #Pick action, observe result, and update history with results
    k = switch(Policy, UCB1 = UCB(ArmDemands, TestPrices),
               TS1  = TS(ArmDemands, TestPrices),
               Balanced1 = Balanced(t, TestPrices))

    Action          = Action2IndexHash[[as.character(k)]]
    Demand          = ifelse(Action < Valuations[length(ArmDemands[[k]]) + 1, k],
                             1, 0)
    Actions[t]      = Action
    Demands[t]      = Demand
    ArmDemands[[k]] = c(ArmDemands[[k]], Demand)
  }
  Rewards = Actions*Demands
  Output  = list(ArmDemands, Actions, Rewards, Demands)
  return(Output)
}
