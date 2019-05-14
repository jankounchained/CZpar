sub_nr <- 1

####
# initialize
####

cluster <- create_cluster(4) %>%
  cluster_library("entropy")

calculate_ntr <- function(doc_subset_path, w) {
  
  dataenv = new.env()
  dataenv$mat = read_csv(doc_subset_path)
  
  ntr_output = tibble(doc_id = dataenv$mat$doc_id) %>%
    mutate(
      novelty = novelty2(w, cluster, dataenv),
      transience = transience2(w, cluster, dataenv),
      resonance = novelty - transience) %>%
    filter(complete.cases(.)) %>%
    mutate(z_novelty = z(novelty),
           z_transience = z(transience),
           z_resonance = z(resonance))
  
  res_nov_model = lm(z_resonance ~ z_novelty, data = ntr_output)
  ntr_output$delta_R = ntr_output$z_resonance - predict(res_nov_model)
  
  ntr_output %>%
    write_csv(paste0(getwd(), str_extract(doc_subset_path, "\\d"), "_from.csv"))
  
}

####
# run
####

subset_paths <- list.files(path = ".", pattern = paste0(sub_nr ,"_to"), full.names = T)
calculate_ntr(sub_path, w = 5000)