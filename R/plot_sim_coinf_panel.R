#' panel: plot simulated vs true coinfection for two largest host orders
#'
#' @title plot_sim_coinf_panel
#'
#' @param pcr_all
#' @param sims_bats 
#' @param sims_rodents 
#'
#' @return 
#' @export
plot_sim_coinf_panel <- function(pcr_all, sims_bats, sims_rodents){
  
  p1 <- plot_sim_coinf(pcr_all, "Chiroptera", sims_bats) +
    ggtitle("Chiroptera")
  
  p2 <- plot_sim_coinf(pcr_all, "Rodentia", sims_rodents) +
    ggtitle("Rodentia")
  
  sims_panel <- p1 + p2
  
  return(sims_panel)
  
}