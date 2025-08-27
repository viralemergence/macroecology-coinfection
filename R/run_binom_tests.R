#' run binomial tests to compared observed coinfection to expected coinfection
#'
#' @title run_binom_tests
#'
#' @param pcr_pos 
#' @param n_unique_animals number of unique animals
#' @param vir_level either "viral_family" or "virus"
#' @param observed_mat observed coinfection matrix
#'
#' @return 
#' @export

run_binom_tests <- function(pcr_pos, n_unique_animals, vir_level, observed_mat){
  
  viruses <- pcr_pos[[vir_level]] %>% unique() %>% sort()
  
  virus_pairs <- gtools::combinations(n = length(viruses), 
                                      r = 2, 
                                      v = as.vector(viruses),
                                      repeats.allowed = T)
  
  virus_pairs <- t(virus_pairs)
  
  # set up empty matrix to hold expected co-infection prevalence values
  expected_mat = array(NA,
                       dim = c(length(viruses), length(viruses)),
                       dimnames = list(viruses, viruses))
  
  # calculate observed viral prevalence for each virus family
  prev_observed <- pcr_pos %>%
    select(predict_sample_id, {{vir_level}}, infection) %>%
    distinct() %>%
    group_by(across(2)) %>%
    summarise(prev = sum(infection)/n_unique_animals)
  
  # calculate expected prevalence of coinfection of two virus families
  for (vir1 in viruses) {
    for (vir2 in viruses) {
      
      prev1 <- prev_observed %>%
        dplyr::filter(!!as.symbol(vir_level) == vir1) %>% pull(prev)
      
      prev2 <- prev_observed %>%
        dplyr::filter(!!as.symbol(vir_level) == vir2) %>% pull(prev)
      
      expected_prev <- prev1*prev2
      
      expected_mat[vir1, vir2] <- expected_prev
    }}
  
  expected_df <- lapply(1:length(virus_pairs[1, ]),
                        function(x){
                          vir1 <- as.character(virus_pairs[1, x])
                          vir2 <- as.character(virus_pairs[2, x])
                          exp_prev <- expected_mat[vir1, vir2]
                          data.frame(vir1, vir2, exp_prev) %>%
                            mutate(virus_pair = paste(vir1, vir2, sep = "-"))
                          }
                        ) %>%
    bind_rows()
  
  # need to reorder so that the rows and columns match with the expected
  observed_mat_reordered <- observed_mat[order(row.names(x = observed_mat)), 
                                         order(colnames(x = observed_mat))]
  
  observed_df <- lapply(1:length(virus_pairs[1, ]),
                        function(x){
                          vir1 <- as.character(virus_pairs[1, x])
                          vir2 <- as.character(virus_pairs[2, x])
                          obs_freq <- observed_mat_reordered[vir1, vir2]
                          data.frame(vir1, vir2, obs_freq) %>%
                            mutate(virus_pair = paste(vir1, vir2, sep = "-"))
                          }
                        ) %>%
    bind_rows()
  
  binom_df <- expected_df %>%
    left_join(observed_df[, c("virus_pair", "obs_freq")])
  
  binom_tests <- lapply(1:nrow(binom_df), function(x){
    binom.test(
      x = binom_df[x, "obs_freq"],  # x - number of successes
      n = n_unique_animals, # n - number of trials
      p = binom_df[x, "exp_prev"] # p - expected (hypothesized) probability of success
    )})
  
  binom_df$p <- binom_tests %>% purrr::map("p.value") %>% unlist
  binom_df$obs_prev <- binom_tests %>% purrr::map("estimate") %>% unlist
  
  # Bonferroni adjustment
  adj_factor <- nrow(binom_df)
  
  binom_df %<>%
    mutate(p_adj = p*adj_factor,
           # p is a probability 0-1, so cap at 1 if adjusted value is > 1
           p_adj = case_when(
             p_adj > 1 ~ 1, 
             TRUE ~ p_adj),
           signif = case_when(p_adj < 0.05 ~ "*",
                              p_adj >= 0.05 ~ " "),
           greater_than_random = case_when(exp_prev < obs_prev ~ "Yes",
                                           exp_prev > obs_prev ~ "No"))
  
  return(binom_df)
  
}

