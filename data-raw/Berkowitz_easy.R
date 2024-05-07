

library(tidyverse)

Berkowitz_easy <- Berkowitz::ngram_item_bank %>%
  tibble::as_tibble() %>%
  filter(rhythmic_difficulty_percentile < 20) %>%
  itembankr::set_item_bank_class()


use_data(Berkowitz_easy)
