#' compare observed coinfection to simulated coinfection
#'
#' @title compare_obs_sim_coinf
#'
#' @param sims_output simulated coinfection data for the order
#'
#' @return 
#' @export
compare_obs_sim_coinf <- function(sims_output){
  
  # calculate the true number of coinfected animals
  true_coinfecteds <- data.frame(table(rowSums(sims_output$sq[, -1]))) %>% 
    dplyr::filter(!Var1 %in% c("0", "1")) %>%
    pull(Freq) %>% 
    sum()
  
  # how often does observed coinfection exceed simulated coinfection?
  sum(true_coinfecteds > sims_output$sims)
  
}
