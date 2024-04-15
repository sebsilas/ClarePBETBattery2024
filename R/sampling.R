


#' Get items
#'
#' @param num_items
#' @param melody_length
#' @param instrument
#'
#' @return
#' @export
#'
#' @examples
get_items <- function(num_items = 12,
                       melody_length = 3:15,
                       instrument = c("Violin", "Cello") ) {

  instrument <- match.arg(instrument)

  item_bank <- if(instrument == "Violin") suzuki_selected_item_bank_violin else if (instrument == "Cello") suzuki_selected_item_bank_cello else stop("Hm?")

  item_bank_sample <- item_bank %>%
    dplyr::filter(N %in% melody_length) %>%
    dplyr::group_by(melody_group) %>%
    dplyr::slice_sample(n = num_items/4) %>% # 4 == no melody groups
    dplyr::ungroup()


  return(item_bank_sample)
}



split_item_bank_into_blocks <- function(df) {
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


