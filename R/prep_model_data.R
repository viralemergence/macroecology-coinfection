#' functions to prep data for GLM(M)s to explore correlates of coinfection
#'
prep_model_data_all <- function(coinf_df){
  
  model_data_all <- coinf_df %>% 
    # filtering factor levels with the most data
    dplyr::filter(
      # excludes carnivores, cattle/buffalo, dogs, goats/sheep, other
      taxa_group %in% c("bats", "birds", "rodents", "shrews", "swine"),
      sex %in% c("male", "female"),
      age_class %in% c("adult (reproductive age)",
                       "subadult (immature, independent)", 
                       "juvenile (dependent on dam)")) %>% 
    mutate(age_class = forcats::fct_recode(
      age_class,
      adult = "adult (reproductive age)",
      subadult = "subadult (immature, independent)",
      juvenile = "juvenile (dependent on dam)")) %>% 
    mutate(across(c(taxa_group, sex, age_class, captivity_status), factor)) %>% 
    mutate(age_class = forcats::fct_relevel(age_class, "adult", "subadult"))
    
  return(model_data_all)
  
}

prep_model_data_bats <- function(model_data_all, roosting_data){
  
  model_data_bats <- model_data_all %>% 
    dplyr::filter(taxa_group == "bats") %>% 
    dplyr::left_join(roosting_data, 
                     by = c("host_sci_name" = "scientific_name")) %>% 
    mutate(host_family = case_when(
      host_genus == "Taphozous" ~ "Emballonuridae",
      host_genus == "Hipposideros" ~ "Hipposideridae",
      host_genus == "Megaderma" ~ "Megadermatidae",
      host_genus == "Miniopterus" ~ "Miniopteridae",
      host_genus %in% c("Chaerephon", "Otomops", "Mops") ~ "Molossidae",
      host_genus == "Nycteris" ~ "Nycteridae",
      host_genus %in% c("Acerodon", "Cynopterus", "Eidolon", "Eonycteris", 
                        "Epomophorus", "Epomops", "Lissonycteris", "Megaerops",
                        "Megaloglossus", "Micropteropus", "Myonycteris",
                        "Pteropus", "Rousettus", "Thoopterus") ~ "Pteropodidae",
      host_genus == "Rhinolophus" ~ "Rhinolophidae",
      host_genus == "Rhinopoma" ~ "Rhinopomatidae",
      host_genus %in% c("Glauconycteris", "Kerivoula", "Myotis", "Neoromicia", 
                        "Pipistrellus", "Scotophilus", "Tylonycteris") ~ 
        "Vespertilionidae",
      TRUE ~ host_family)) %>%
    
    # exclude bats with missing family or missing roosting data
    dplyr::filter(!is.na(host_family),
                  !is.na(cave_roosting))
  
  return(model_data_bats)
  
}

prep_model_data_rodents <- function(model_data_all){
  
  model_data_rodents <- model_data_all %>% 
    dplyr::filter(taxa_group == "rodents") %>% 
    mutate(host_family = case_when(
      host_genus %in% c("Arvicanthis", "Bandicota", "Berylmys", "Bunomys", 
                        "Leopoldamys", "Lophuromys", "Mastomys", "Maxomys", 
                        "Mus", "Niviventer", "Paruromys", "Praomys", "Rattus", 
                        "Sundamys") ~ "Muridae",
      host_genus %in% c("Callosciurus", "Dremomys", "Hylopetes", "Menetes", 
                        "Sundasciurus") ~ "Sciuridae",
      
      TRUE ~ host_family)) %>% 
    
    # collapse juveniles and subadults because there are few juveniles
    mutate(age_class = forcats::fct_collapse(
      age_class,
      "adult" = "adult",
      "subadult or juvenile" = c("subadult", "juvenile")))
  
  return(model_data_rodents)
  
}