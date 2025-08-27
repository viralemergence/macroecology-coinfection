#' plot simulated vs true coinfection for a taxa group
#'
#' @title plot_sim_coinf
#'
#' @param pcr_all
#' @param focal_taxa which taxa_group to examine
#' @param sims simulated coinfection data for the taxa group
#'
#' @return 
#' @export
plot_sim_coinf <- function(pcr_all, focal_taxa, sims){
  
  pcr_subset <- pcr_all %>% 
    dplyr::filter(taxa_group == focal_taxa)
  
  # make a full grid for all animals and infection status for all viruses
  sq <- pcr_subset %>%
    distinct(predict_sample_id, virus) %>%
    mutate(infection = 1) %>%
    complete(predict_sample_id, virus, fill = list(infection = 0)) %>%
    dplyr::filter(!is.na(virus)) %>%
    pivot_wider(names_from = virus, values_from = infection) %>% 
    data.frame()
  
  # calculate the true number of coinfected animals
  true_coinfecteds <- data.frame(table(rowSums(sq[, -1]))) %>% 
    dplyr::filter(!Var1 %in% c("0", "1")) %>%
    pull(Freq) %>% 
    sum()
  
  rm(pcr_all, pcr_subset)
  
  # plot distribution of simulated number of coinfecteds compared to true
  p <- ggplot(data.frame(sims = sims), aes(x = sims)) + 
    geom_density(fill = 'salmon1') + 
    theme_bw() + 
    geom_vline(xintercept = true_coinfecteds, linetype = 2, lwd = 1) + 
    xlim(0, true_coinfecteds + 10) +
    xlab("Number of simulated coinfected animals")
  
  return(p)
  
}
