


#' Get blocks
#'
#' @param num_items
#' @param melody_length
#'
#' @return
#' @export
#'
#' @examples
get_blocks <- function(num_items = 24,
                       melody_length = 3:15,
                       instrument = c("Violin", "Cello") ) {

  instrument <- match.arg(instrument)

  item_bank <- if(instrument == "Violin") suzuki_selected_item_bank_violin else if (instrument == "Cello") suzuki_selected_item_bank_cello else stop("Hm?")

  suzuki_item_bank_ngram_sample <- item_bank %>%
    dplyr::filter(N %in% melody_length) %>%
    dplyr::group_by(melody_group) %>%
    dplyr::slice_sample(n = num_items/4) %>% # 4 == no melody groups
    dplyr::ungroup()



  item_blocks <- suzuki_item_bank_ngram_sample %>%
    split_item_black_into_blocks()


  block_1 <- item_blocks$block_1
  block_2 <- item_blocks$block_2

  # Block 1 arrhythmic vs. rhythmic
  block_1_sub_blocks <- block_1 %>%
    split_item_black_into_blocks()

  block_1_arrhythmic <- block_1_sub_blocks$block_1
  block_1_rhythmic <- block_1_sub_blocks$block_2

  # Block 2 arrhythmic vs. rhythmic
  block_2_sub_blocks <- block_2 %>%
    split_item_black_into_blocks()

  block_2_arrhythmic <- block_2_sub_blocks$block_1
  block_2_rhythmic <- block_2_sub_blocks$block_2

  # Order by rough difficulty
  block_1_arrhythmic <- dplyr::arrange(block_1_arrhythmic, arrhythmic_difficulty)
  block_1_rhythmic <- dplyr::arrange(block_1_rhythmic, rhythmic_difficulty)
  block_2_arrhythmic <- dplyr::arrange(block_2_arrhythmic, arrhythmic_difficulty)
  block_2_rhythmic <- dplyr::arrange(block_2_rhythmic, rhythmic_difficulty)

  list(
    block_1_arrhythmic = block_1_arrhythmic,
    block_1_rhythmic = block_1_rhythmic,
    block_2_arrhythmic = block_2_arrhythmic,
    block_2_rhythmic = block_2_rhythmic
  )
}



split_item_black_into_blocks <- function(df) {
  num_items <- nrow(df)
  block_1_items_idxes <- sample(1:num_items, size = num_items/2)
  block_1_items <- df[block_1_items_idxes, ]

  block_2_items_idxes <- setdiff(1:num_items, block_1_items_idxes)
  block_2_items <- df[block_2_items_idxes, ]

  list(
    block_1 = block_1_items,
    block_2 = block_2_items
  )
}
