

library(tidyverse)

clare_requested_mels <- c(187, 215, 219, 225, 310, 371, 373, 408, 436, 441, 454, 475)


clare_desired_subset <- Berkowitz::ngram_item_bank %>%
  tibble::as_tibble() %>%
  dplyr::mutate(stimulus = readr::parse_number(midi_file)) %>%
  dplyr::filter(stimulus %in% clare_requested_mels)


clare_desired_subset %>%
  writexl::write_xlsx('clare_desired_subset.xlsx')
