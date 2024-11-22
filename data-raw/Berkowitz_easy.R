

library(tidyverse)

# V1
# Berkowitz_easy <- Berkowitz::ngram_item_bank %>%
#   tibble::as_tibble() %>%
#   filter(rhythmic_difficulty_percentile < 10) %>%
#   itembankr::set_item_bank_class()

# V2
Berkowitz_easy <- Berkowitz::Berkowitz_subset %>%
  tibble::as_tibble() %>%
  dplyr::filter(rhythmic_difficulty_percentile <= 10,
                arrhythmic_difficulty_percentile <= 10,
                N < 10) %>%
  itembankr::set_item_bank_class()

use_data(Berkowitz_easy, overwrite = TRUE)

credentials::set_github_pat()

install()
