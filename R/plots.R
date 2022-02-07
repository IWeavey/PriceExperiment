#' Plots Histogram of Prices Played'
#' @import ggplot2
#' @export
#'
#' @param Results1 Balanced experiment results
#' @param Results2 UCB experiment results
#' @param Results3 TS experiment results
#' @param NumIter Number of customers in the experiment
#' @param TestPrices Set of prices being tested
#' @param Optimal The optimal price for the experiment
#' @param Title Title for the Graph
#' @return A histogram plot
PlotHistogram <- function(Results1, Results2, Results3, NumIter, TestPrices,
                          Optimal, Title){
  df1 = data.frame(price = TestPrices, percent = lengths(Results1)/NumIter)
  df2 = data.frame(price = TestPrices, percent = lengths(Results2)/NumIter)
  df3 = data.frame(price = TestPrices, percent = lengths(Results3)/NumIter)
  df  = merge(df1, df2, by = "price")
  df  = merge(df, df3, by = "price")
  colnames(df) = c("price", "Balanced", "UCB", "TS")
  DataMelted = reshape2::melt(df, id.var='price')
  ggplot(data=DataMelted, aes(x= price, y = value, fill = variable)) +
    geom_bar(stat="identity", position = "dodge") +
    geom_vline(aes(color = "Optimal Price", xintercept = Optimal), linetype="dotted",
               size = 1) +
    scale_color_manual(values = c("Optimal Price" = "red")) +
    labs(fill = "Algorithm") +
    xlab("Price") +
    ylab("proportion of time chosen") +
    ggtitle(Title) +
    theme_minimal() +
    theme(legend.title=element_blank()) +
    theme(plot.title = element_text(hjust = 0.5))
}

#' Plots Performance of Various policies'
#' @export
#'
#' @param Results1 Balanced experiment results
#' @param Results2 UCB experiment results
#' @param Results3 TS experiment results
#' @param NumIter Number of customers in the experiment
#' @param Optimal The optimal reward for the experiment
#' @param Title Title for the Graph
#' @return A performance plot
PlotPerformance <- function(Rewards1, Rewards2, Rewards3, NumIter,
                            Optimal, Title){
  Iter = 1:NumIter
  CumOptRewards1 = cumsum(Rewards1)/(Iter*Optimal)
  CumOptRewards2 = cumsum(Rewards2)/(Iter*Optimal)
  CumOptRewards3 = cumsum(Rewards3)/(Iter*Optimal)
  Data = data.frame(cbind(Iter, CumOptRewards1, CumOptRewards2, CumOptRewards3))
  colnames(Data) = c("Iteration", "Balanced", "UCB", "TS")
  DataMelted = reshape2::melt(Data, id.var='Iteration')
  colnames(DataMelted) = c("Iteration", "Algorithm", "Value")
  ggplot(DataMelted, aes(x= Iteration, y = Value, group = Algorithm)) +
    geom_line(aes(color = Algorithm), size = 1.1) +
    scale_x_continuous(lim=c(0, NumIter)) +
    scale_y_continuous(lim=c(0, 1.02)) +
    theme_minimal() +
    labs(title = Title, x ="number of iterations", y = "% of optimal profits") +
    theme(plot.title = element_text(hjust = 0.5))
}


#' Finds the likelihood of the true probability of purchase given experimental data
#' (recall purchasing is binary decision)
#' @export
#'
#' @param Successes Number of times that a purchase was made at the price tested
#' @param TimesTested Number of times a price has been tested
#' @return Vector containing likelihoods of purchase probability over a fine grid
PosteriorLikelihoods <- function(Successes, TimesTested){
  Thetas = seq(0, 1, by=.001)
  Liks   = dbinom(Successes, TimesTested, Thetas)
  return (Liks/sum(Liks))
}

#' Creates 95 percent Credible Intervals for the probability of purchase given data.'
#' @export
#'
#' @param TestPrices: Vector of prices being considered
#' @param ArmDemands: List of vectors containing complete purchase decisions for each price
#' @return A list containing 2 vectors: low credible bounds and high credible bounds
CredibleInterval <- function(TestPrices, ArmDemands){
  NumSelections = lengths(ArmDemands)
  Successes     = sapply(ArmDemands, sum)
  ThetaLiks     = mapply(PosteriorLikelihoods, Successes, NumSelections)
  Min025        = integer(0)
  Max975        = integer(0)
  for (i in 1:length(TestPrices)){
    Min025[i] <- which.max((0.025 < cumsum(ThetaLiks[,i]))*1)/1000
    Max975[i] <- which.min((0.975 > cumsum(ThetaLiks[,i]))*1)/1000
  }
  return (list(Min025, Max975))
}


#' Plots Learning of demand or rewards by each policy'
#' @export
#'
#' @param TestPrices Set of prices being tested
#' @param ArmDemands List of vectors containing complete purchase decisions for each price
#' @param Color Color of the dots
#' @param Title Title of the graph
#' @param DemandOrProfit "Demand" or "Rewards" depending on what it is learning
#' @param TrueResults The True demand or rewards function
#' @param ylab y-axis label
#' @return A performance plot
LearningGraph <- function(TestPrices, ArmDemands, Color, Title,
                          DemandOrProfit, TrueResults, ylab){

  Grid     = c(0, seq(1000))/1000
  Bounds   = CredibleInterval(TestPrices, ArmDemands)

  if (DemandOrProfit == "Demand"){
    MinBound = Bounds[[1]]
    MaxBound = Bounds[[2]]
    Results  = sapply(ArmDemands, mean)
    ylim     = 1
  } else{
    MinBound = Bounds[[1]]*TestPrices
    MaxBound = Bounds[[2]]*TestPrices
    Results  = sapply(ArmDemands, mean)*TestPrices
    ylim = max(max(MaxBound), max(Results)) + 0.01
  }

  ggplot() +
    geom_point(data = NULL,aes(x = TestPrices, Results), color = Color, size = 2) +
    geom_errorbar(data = NULL, aes(x = TestPrices, ymin = MinBound, ymax = MaxBound),
                  width = .05, position = position_dodge(.9)) +
    geom_line(data=NULL,aes(x = Grid, y = TrueResults), size = 1,
              linetype="dashed", color = "grey") +
    scale_x_continuous(lim=c(0, 1.05), breaks = TestPrices, labels = TestPrices) +
    scale_y_continuous(lim=c(0, ylim)) +
    theme_minimal() +
    labs(title = Title, x = "price", y = ylab) +
    theme(plot.title = element_text(hjust = 0.5))
}
