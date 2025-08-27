#' panel: plot simulated vs true coinfection for a taxa group
#'
#' @title plot_sim_coinf_panel
#'
#' @param pcr_all
#' @param sims_bats 
#' @param sims_rodents 
#' @param sims_birds
#'
#' @return 
#' @export
plot_sim_coinf_panel <- function(pcr_all, sims_bats, sims_rodents, sims_birds){
  
  p1 <- plot_sim_coinf(pcr_all, "bats", sims_bats) +
    ggtitle("Bats")
  
  p2 <- plot_sim_coinf(pcr_all, "rodents", sims_rodents) +
    ggtitle("Rodents")
  
  p3 <- plot_sim_coinf(pcr_all, "bats", sims_birds) +
    ggtitle("Birds")
  
  sims_panel <- p1 + p2 + p3
  
  return(sims_panel)
  
}