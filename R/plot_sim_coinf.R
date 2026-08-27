#' plot simulated vs true coinfection for a host order
#'
#' @title plot_sim_coinf
#'
#' @param pcr_all
#' @param focal_order which host order to examine
#' @param sims_output simulated coinfection data for the order
#'
#' @return 
#' @export
plot_sim_coinf <- function(pcr_all, focal_order, sims_output){

  # calculate the true number of coinfected animals
  true_coinfecteds <- data.frame(table(rowSums(sims_output$sq[, -1]))) %>% 
    dplyr::filter(!Var1 %in% c("0", "1")) %>%
    pull(Freq) %>% 
    sum()

  # plot distribution of simulated number of coinfecteds compared to true
  p <- ggplot(data.frame(sims = sims_output$sims), aes(x = sims)) + 
    geom_density(fill = 'salmon1') + 
    theme_bw() + 
    geom_vline(xintercept = true_coinfecteds, linetype = 2, lwd = 1) + 
    xlim(0, true_coinfecteds + 10) +
    xlab("Number of simulated coinfected animals")
  
  return(p)
  
}
