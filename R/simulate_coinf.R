#' simulate coinfection based on observed virus prevalence in a host order
#'
#' @title simulate_coinf
#'
#' @param pcr_all
#' @param focal_order which host order to examine
#' @param subgroup boolean: should test results be restricted to a subgroup of animals 
#' @param subgroup_ids if subgroup = T, a vector of predict_sample_id numbers to restrict the analyses to
#' @param restrict_vir boolean: should test results be restricted to core five virus targets
#' @param nsim number of simulations to run
#' @param seed random seed to ensure simulations are reproducible 
#'
#' @return 
#' @export
simulate_coinf <- function(pcr_all, focal_order, subgroup, subgroup_ids = NULL, 
                           restrict_vir, nsim = 1000, seed = 25624){
  
  set.seed(seed)
  
  pcr_subset <- pcr_all %>% 
    dplyr::filter(host_order == focal_order)
  
  if(subgroup){
    pcr_subset <- pcr_subset %>% 
      dplyr::filter(predict_sample_id %in% subgroup_ids)
  }
  
  if(restrict_vir){
    pcr_subset <- pcr_subset %>% 
      dplyr::filter(virus_target_tested %in% c("Coronaviruses", "Filoviruses",
                                               "Flaviviruses", "Influenzas",
                                               "Paramyxoviruses"))
  }
  
  # make a full grid for all animals and infection status for all viruses
  sq <- pcr_subset %>%
    distinct(predict_sample_id, virus) %>%
    mutate(infection = 1) %>%
    complete(predict_sample_id, virus, fill = list(infection = 0)) %>%
    dplyr::filter(!is.na(virus)) %>%
    pivot_wider(names_from = virus, values_from = infection) %>% 
    data.frame()

  sims <- sapply(c(1:nsim), function(i) {
    
    sq.i <- sq
    
    # column by column (ie virus by virus), sample using the observed data
    # this ensures that the overall prevalence of each virus stays the same
    # but now infection is decoupled from any host characteristics
    for (j in 2:ncol(sq.i)){
      sq.i[, j] <- sample(sq.i[, j], replace = FALSE) 
    } 
    
    # if you compare the colSums for sq and sq.i, they are the same
    # colSums(sq) == colSums(sq.i)
    
    # calculate the simulated number of coinfected animals
    sim_coinfecteds <- data.frame(table(rowSums(sq.i[, -1]))) %>% 
      dplyr::filter(!Var1 %in% c("0", "1")) %>%
      pull(Freq) %>% 
      sum()

    })
  
  return(sims_output = list(sq = sq, sims = sims))
}
