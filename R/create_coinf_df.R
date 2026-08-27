#' create coinfection dataframe for GLMMs
#' each row is a unique animal
#'
#' @title create_coinf_df
#'
#' @param pcr_pos pcr testing results (positives only)
#' @param test_summaries animal level testing summary quantities
#'
#' @return df of all positive animals along with coinfection info
#' @export
#' 
create_coinf_df <- function(pcr_pos, test_summaries){

  # count unique viruses detected per predict_sample_id (unique animals)
  virus_coinf <- pcr_pos %>% 
    dplyr::select(predict_sample_id, viral_family, virus) %>% 
    distinct() %>% 
    group_by(predict_sample_id) %>% 
    count %>% 
    rename(n_virus = n)
  
  # create df of all positive sample_id, noting virus coinfections
  coinf_df <- pcr_pos %>% 
    left_join(virus_coinf, by = "predict_sample_id") %>% 
    # remove columns at the test level, because each row is now an animal
    dplyr::select(-c(predict_test_id, specimen_id:interpretation, infection,
                     sample_location_latitude, sample_location_longitude)) %>% 
    dplyr::filter(!duplicated(predict_sample_id)) %>% 
    mutate(virus_coinf = case_when(n_virus > 1 ~ 1,
                                   n_virus == 1 ~ 0)) %>% 
    # add test summary info
    left_join(test_summaries) %>% 
    
    # add family for later analyses
    mutate(host_family = case_when(
      
      # Anseriformes
      host_genus %in% c("Anas", "Anser", "Cygnus", "Tadorna") ~ "Anatidae",
  
      # Carnivora
      host_genus == "Nandinia" ~ "Nandiniidae",
      
      # Charadriiformes 
      host_genus == "Larus" ~ "Laridae",
      
      # Chiroptera
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
      
      # Eulipotyphla
      host_genus == "Echinosorex" ~ "Erinaceidae",
      host_genus %in% c("Crocidura", "Suncus") ~ "Soricidae",
      
      # Galliformes
      host_genus == "Coturnix" ~ "Phasianidae",
      
      # Passeriformes
      host_genus == "Corvus" ~ "Corvidae",
                       
      # Rodentia
      host_genus == "Hystrix" ~ "Hystricidae",
      host_genus %in% c("Arvicanthis", "Bandicota", "Berylmys", "Bunomys", 
                        "Leopoldamys", "Lophuromys", "Mastomys", "Maxomys",
                        "Mus", "Niviventer", "Paruromys", "Praomys", "Rattus", 
                        "Sundamys") ~ "Muridae",
      host_genus %in% c("Callosciurus", "Dremomys", "Hylopetes", "Menetes",
                        "Ratufa", "Sundasciurus") ~ "Sciuridae",
      host_genus == "Rhizomys" ~ "Spalacidae",
      
      # Scandentia
      host_genus %in% c("Dendrogale", "Tupaia") ~ "Tupaiidae",
        
      TRUE ~ host_family)) %>%
    
    dplyr::relocate(c(host_family, host_order), .after = host_genus)
  
  return(coinf_df)
  
}
