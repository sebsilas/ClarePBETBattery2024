

#' Run the Suzuki Battery
#'
#' @param title
#' @param app_name
#' @param instrument
#' @param musicassessr_aws
#'
#' @return
#' @export
#'
#' @examples
run_battery <- function(title = "Playing by Ear",
                        app_name = 'pbetsuzuki2024',
                        instrument = c("Violin", "Cello"),
                        musicassessr_aws = FALSE) {

  instrument <- match.arg(instrument)

  tl <- function() {
    suzuki_tl(instrument = instrument, app_name = app_name, musicassessr_aws = musicassessr_aws)
  }

  musicassessr::make_musicassessr_test(

    welcome_page = musicassessr::empty_code_block(),

    title = title,

    admin_password = Sys.getenv("ADMIN_PASSWORD"),

    elts = tl,

    opt = musicassessr::musicassessr_opt(setup_pages = FALSE,
                                         visual_notation = TRUE,
                                         app_name = app_name,
                                         musicassessr_aws = musicassessr_aws),

    final_page = psychTestR::final_page("Thank you for taking part in the test!")

  )


}


suzuki_tl <- function(num_items = 24, instrument = c("Violin", "Cello"), app_name, musicassessr_aws) {

  print('suz')
  print(app_name)
  print(musicassessr_aws)

  instrument <- match.arg(instrument)


  stimuli <- get_blocks(instrument = instrument)

  audio_block <- suzuki_audio_block()

  psychTestR::join(

    # # - demographics (age, gender, nationality, highest educational level obtained.
    #
    # psyquest::DEG(),
    #
    #
    # # - GMSI-musical training subscale
    #
    # psyquest::GMS(subscales = "Musical Training"),
    #
    # # - Concurrent musical activities
    #
    # psyquest::CCM(),
    #
    # custom_questions(),
    #
    #
    # #
    # # - JAJ (8 items)
    #
    # JAJ::JAJ(num_items = 8L),

    musicassessr::setup_pages(skip_setup = 'except_microphone'),

    audio_block,

    #  - SAA (5 rhythmic, 5 arhythmic items)

    SAA::SAA(app_name = app_name,
             num_items = list(long_tones = 6L,
                              arrhythmic = 5L,
                              rhythmic = 5L),
             absolute_url = "https://musicassessr.com/suzuki-pbet-2024/",
             allow_SNR_failure = TRUE,
             musicassessr_aws = musicassessr_aws),


    #   - PBE
    #   -- - 10 items with up to 3 repetitions, items range between 3 and 15 notes and varying difficulty;
    #   - -- 10 items as one-shot, items range between 3 and 15 notes and varying difficulty;
    #   -- just for the piloting we might have half the items in A major and half in their original key
    #


    PBET::PBET(
      app_name = app_name,
      # experiment_id = 3L, # For Clare's exp
      # experiment_id = 1L # For dev/testing
      arrhythmic_item_bank = stimuli$block_1_arrhythmic %>% itembankr::set_item_bank_class(),
      rhythmic_item_bank = stimuli$block_1_rhythmic %>% itembankr::set_item_bank_class(),
      num_items = list(interval_perception = 0L,
                       find_this_note = 0L,
                       arrhythmic = nrow(stimuli$block_1_arrhythmic),
                       rhythmic = nrow(stimuli$block_2_arrhythmic),
                       wjd_audio = list(key_easy = 0L, key_hard = 0L)),
      num_examples = PBET::no_examples(),
      SNR_test = FALSE,
      get_range = FALSE,
      max_goes = 3L,
      melody_length = 3:15,
      presampled_item_bank = TRUE,
      default_range = musicassessr::set_default_range(instrument),
      gold_msi = FALSE,
      demographics = FALSE,
      musicassessr_aws = musicassessr_aws
    ),

    psychTestR::one_button_page("Now you will only have one go at each melody"),

    PBET::PBET(
      app_name = app_name,
      # experiment_id = 3L, # For Clare's exp
      # experiment_id = 1L # For dev/testing
      arrhythmic_item_bank = stimuli$block_1_arrhythmic %>% itembankr::set_item_bank_class(),
      rhythmic_item_bank = stimuli$block_1_rhythmic %>% itembankr::set_item_bank_class(),
      num_items = list(interval_perception = 0L,
                       find_this_note = 0L,
                       arrhythmic = nrow(stimuli$block_1_arrhythmic),
                       rhythmic = nrow(stimuli$block_2_arrhythmic),
                       wjd_audio = list(key_easy = 0L, key_hard = 0L)),
      num_examples = PBET::no_examples(),
      skip_setup = TRUE, # We skip all setup for this
      max_goes = 1L,
      melody_length = 3:15,
      presampled_item_bank = TRUE,
      default_range = musicassessr::set_default_range(instrument),
      show_introduction = FALSE,
      gold_msi = FALSE,
      demographics = FALSE,
      musicassessr_aws = musicassessr_aws
    )

    # Other PBET block..
  ) %>% consentr::consent(need_age_consent = FALSE)

}



suzuki_audio_block <- function() {

  shiny::addResourcePath(prefix = 'audio',
                  directoryPath = system.file('Berk_Cut_melodies_violin_noclick', package = 'ClarePBETBattery2024'))


  purrr::pmap(suzuki_selected_item_bank, iterate_row)
}



# Function to convert row to dataframe
iterate_row <- function(...) {

  # Convert the row to a DF
  tb_row <- tibble::as_tibble(list(...))

  audio_file <- tb_row$`Audio File name`

  audio_file_path <- paste0('audio/', audio_file)

  musicassessr::present_stimuli(
    stimuli = audio_file_path,
    stimuli_type = "audio",
    display_modality = "auditory",
    page_type = "record_audio_page",
    get_answer = musicassessr::get_answer_pyin_melodic_production,
    hideOnPlay = TRUE,
    page_label = audio_file,
    answer_meta_data = tb_row,
    trigger_end_of_stimulus_fun = musicassessr::paradigm(paradigm_type = "call_and_response")$trigger_end_of_stimulus_fun
  )

}

# debug(get('get_opti3', asNamespace('musicassessr')))

