#' clean pcr data in prep for joining with animal data
#'
#' @title clean_pcr_data
#'
#' @param pcr_raw pcr testing results
#'
#' @return cleaned pcr results for non-humans
#' @export
clean_pcr_data <- function(pcr_raw) {
  
  pcr <- pcr_raw %>%
    
    # exclude humans and a random dog from 2010
    dplyr::filter(!taxa_group == "humans",
                  !predict_sample_id == 65871) %>% 
    
    rename(host_sci_name = scientific_name) %>% 
    
    mutate(across("host_sci_name", 
                  ~ str_trim(.x) %>%
                    str_replace_all(" ", "_"))) %>% 
    
    separate_wider_delim(host_sci_name, "_", names = c("host_genus", NA),
                         cols_remove = F,
                         too_few = "align_start",
                         too_many = "drop") %>% 
    
    # some scientific names provided were families
    # this fixes it so families don't end up in the genus column
    mutate(host_family = case_when(
      host_genus == "Cercopithecidae" ~ "Cercopithecidae",
      host_genus == "Columbidae" ~ "Columbidae",
      host_genus == "Gliridae" ~ "Gliridae",
      host_genus == "Molossidae" ~ "Molossidae",
      host_genus == "Nycteridae" ~ "Nycteridae",
      host_genus == "Rhinopomatidae" ~ "Rhinopomatidae",
      host_genus == "Sciuridae" ~ "Sciuridae",
      host_genus == "Soricidae" ~ "Soricidae",
      host_genus == "Vespertilionidae" ~ "Vespertilionidae",
      host_genus == "Viverridae" ~ "Viverridae")) %>% 
    
    mutate(host_genus = str_replace_all(
      host_genus, 
      c("Cercopithecidae|Columbidae|Gliridae|Molossidae|Nycteridae|Rhinopomatidae|Sciuridae|Soricidae|Vespertilionidae|Viverridae|Chiroptera"),
      NA_character_)) %>% 
    
    # collapse domestic and wild birds
    # separate rodents and shrews
    mutate(
      taxa_group = forcats::fct_collapse(
        taxa_group,
        bats = "bats",
        birds = c("birds", "poultry/other fowl"),
        rodents_shrews = "rodents/shrews"),
      taxa_group = case_when(
        host_genus %in% c("Acomys", "Aethomys", "Apodemus", "Arvicanthis", 
                     "Atherurus", "Bandicota", "Belomys", "Berylmys", "Bunomys", 
                     "Callosciurus", "Cavia", "Chiropodomys", "Cricetomys", 
                     "Dasymys", "Delanymys", "Dephomys", "Dremomys", 
                     "Echiothrix", "Gerbilliscus", "Grammomys", "Graphiurus", 
                     "Heimyscus", "Hybomys", "Hylomyscus", "Hylopetes", 
                     "Hystrix", "Lemniscomys", "Lenomys", "Leopoldamys", 
                     "Lophuromys", "Malacomys", "Mastomys", "Maxomys", 
                     "Menetes", "Mus", "Myomyscus", "Niviventer", "Oenomys", 
                     "Paruromys", "Pelomys", "Petaurillus", "Petaurista", 
                     "Praomys", "Prionomys", "Protoxerus", "Rattus", "Ratufa", 
                     "Rhinosciurus", "Rhizomys", "Rodentia", "Saccostomus", 
                     "Steatomys", "Stochomys", "Sundamys", "Sundasciurus", 
                     "Taeromys", "Tamiops", "Taterillus", "Thryonomys", 
                     "Trichys", "Uranomys", "Vandeleuria") ~ "rodents",
        # shrews here includes true shrews, tree shrews, elephant shrews
        host_genus %in% c("Crocidura", "Dendrogale", "Elephantulus", "Hylomys", 
                     "Paracrocidura", "Suncus", "Sylvisorex", "Tupaia") ~ "shrews", 
        host_sci_name == "Soricidae" ~ "shrews",
        host_sci_name %in% c("Gliridae", "Sciuridae") ~ "rodents",
        TRUE ~ taxa_group)) %>% 

    # clean up host scientific names
    mutate(across("host_sci_name", 
                  ~ str_replace_all(.x, "/", "_") %>%
                    str_remove_all("[.]"))) %>% 
    
    # fix virus families (Bunyaviridae no longer exists as a family)
    mutate(
      viral_family = case_when(
        virus == "Thottapalayam virus" ~ "Hantaviridae",
        TRUE ~ viral_family),
      viral_family = forcats::fct_relevel(
        viral_family, "Hantaviridae", after = 4)
      )
  
  return(pcr)
  
}
