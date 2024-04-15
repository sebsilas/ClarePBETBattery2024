

#' Run the Suzuki Battery
#'
#' @param title
#' @param app_name
#' @param instrument
#'
#' @return
#' @export
#'
#' @examples
run_battery <- function(title = "Playing by Ear",
                        app_name = 'pbetsuzuki2024',
                        instrument = c("Violin", "Cello")) {

  instrument <- match.arg(instrument)

  tl <- function() {
    suzuki_tl(instrument = instrument, app_name = app_name)
  }

  musicassessr::make_musicassessr_test(

    welcome_page = musicassessr::empty_code_block(),

    title = title,

    admin_password = Sys.getenv("ADMIN_PASSWORD"),

    elts = tl,

    opt = musicassessr::musicassessr_opt(setup_pages = FALSE,
                                         visual_notation = TRUE,
                                         app_name = app_name,
                                         musicassessr_aws = TRUE,
                                         use_musicassessr_db = FALSE, # Don't instantiate this here
                                         asynchronous_api_mode = TRUE,
                                         user_id = 60L, # Clare's user test ID
                                         css = "https://musicassessr.com/assets/css/style_songbird.css"),

    final_page = psychTestR::final_page("Thank you for taking part in the test!"),

  )


}


suzuki_tl <- function(num_items = 24, instrument = c("Violin", "Viola", "Cello"), app_name) {

  instrument <- match.arg(instrument)

  inst_id <- if(instrument == "Violin") 14L else if(instrument == "Viola") 15L else if(instrument == "Cello") 16L else stop("Not a valid instrument")

  stimuli <- get_items(instrument = instrument)


  audio_block <- psychTestR::join(
    psychTestR::one_button_page("Now you will play back some melodies as audio"),
    suzuki_audio_block(stimuli, instrument)
  )


  psychTestR::join(

    # - demographics (age, gender, nationality, highest educational level obtained.

    psyquest::DEG(),


    # - GMSI-musical training subscale

    psyquest::GMS(subscales = "Musical Training"),

    # - Concurrent musical activities

    psyquest::CCM(),

    custom_questions(),


    #
    # - JAJ (8 items)

    JAJ::JAJ(num_items = 8L),

    #  - SAA (5 rhythmic, 5 arhythmic items)

    psychTestR::one_button_page(
      shiny::tags$div(
        shiny::tags$link(rel="stylesheet", type="text/css", href="https://musicassessr.com/assets/css/style_songbird.css"),
        shiny::tags$script("var upload_to_s3 = true; console.log('Turning S3 mode on');"),
        shiny::tags$p(paste0("Let's proceed!"))
      )
    ),


    SAA::SAA(app_name = app_name,
             num_items = list(
                              #long_tones = 6L,
                              long_tones = 0L,
                              arrhythmic = 0L,
                              rhythmic = 5L),
             absolute_url = "https://musicassessr.com/suzuki-pbet-2024/",
             skip_setup = 'except_microphone',
             experiment_id = 1L, # dev
             musicassessr_aws = TRUE,
             use_musicassessr_db = TRUE,
             demographics = FALSE,
             gold_msi = FALSE,
             asynchronous_api_mode = TRUE,
             user_id = 60L, # Clare experiment user
             get_answer_melodic = musicassessr::get_answer_add_trial_and_compute_trial_scores_s3
             ),


    #   - PBE
    #   -- - 10 items with up to 3 repetitions, items range between 3 and 15 notes and varying difficulty;
    #   - -- 10 items as one-shot, items range between 3 and 15 notes and varying difficulty;
    #   -- just for the piloting we might have half the items in A major and half in their original key
    #


    # Just use PBET wrapper for instructions and setup, but append audio block at end?

    PBET::PBET(
      app_name = app_name,
      # experiment_id = 3L, # For Clare's exp
      experiment_id = 1L, # For dev/testing
      num_items = list(interval_perception = 0L,
                       find_this_note = 0L,
                       arrhythmic = 0L,
                       rhythmic = 0L, # We're only using for the instructions
                       wjd_audio = list(key_easy = 0L, key_hard = 0L)),
      # arrhythmic_item_bank = stimuli$block_1_arrhythmic %>% itembankr::set_item_bank_class(), # We don't actually use this..
      # rhythmic_item_bank = stimuli$block_1_rhythmic %>% itembankr::set_item_bank_class(),
      num_examples = PBET::no_examples(),
      skip_setup = TRUE, # this is done at the musicassessr_test level
      max_goes = 3L,
      melody_length = 3:15,
      #presampled_item_bank = TRUE,
      default_range = musicassessr::set_default_range(instrument),
      gold_msi = FALSE,
      demographics = FALSE,
      musicassessr_aws = TRUE,
      use_musicassessr_db = TRUE,
      asynchronous_api_mode = TRUE,
      get_answer_function_audio = musicassessr::get_answer_add_trial_and_compute_trial_scores_s3,
      user_id = 60L, # Clare experiment user,
      append_trial_block_after = audio_block,
      instrument_id = inst_id
    )

    # Other PBET block..
  ) %>% consentr::consent(need_age_consent = FALSE)

}



suzuki_audio_block <- function(selected_audio, instrument = c("Violin", "Cello")) {

  instrument <- match.arg(instrument)

  file_path <- if(instrument == "Violin") 'Berk_Cut_melodies_violin_noclick' else if(instrument == "Cello") "Berk-Cut_melodies_cello_noclick" else stop ("Not valid instrument")

  shiny::addResourcePath(prefix = 'audio',
                  directoryPath = system.file(file_path, package = 'ClarePBETBattery2024'))


  purrr::pmap(selected_audio, iterate_row)
}



# Function to convert row to dataframe
iterate_row <- function(..., instrument = c("Violin", "Viola", "Cello")) {

  instrument <- match.arg(instrument)

  # Convert the row to a DF
  tb_row <- tibble::as_tibble(list(...))

  #browser()

  audio_file <- tb_row$`Audio File name`

  audio_file_path <- paste0('audio/', audio_file)

  psychTestR::reactive_page(function(state, ...) {

    db_vars <- if(psychTestR::get_global("musicassessr_db", state)) {

      list(
        midi_vs_audio = "audio",
        stimuli = tb_row$abs_melody,
        stimuli_durations = tb_row$durations,
        trial_time_started = Sys.time(),
        instrument = psychTestR::get_global("inst", state),
        attempt = 1L,
        item_id = tb_row$item_id,
        display_modality = "audio",
        phase = "test",
        rhythmic = TRUE,
        session_id = musicassessr::get_promise_value(psychTestR::get_global("session_id", state)),
        test_id = 2L, # PBET
        review_items_id = NULL,
        new_items_id = NULL
      )
    } else NULL

    musicassessr::present_stimuli(
      stimuli = audio_file_path,
      stimuli_type = "audio",
      display_modality = "auditory",
      page_title = "Play the melody by ear",
      page_text = shiny::tags$div(
                    shiny::tags$p("Play the melody by ear then clip Stop when you are finished."),
                    shiny::tags$p(tb_row$prompt)
                    ),
      page_type = "record_audio_page",
      get_answer = musicassessr::get_answer_add_trial_and_compute_trial_scores_s3,
      hideOnPlay = TRUE,
      page_label = audio_file,
      answer_meta_data = tb_row,
      trigger_end_of_stimulus_fun = musicassessr::paradigm(paradigm_type = "call_and_response")$trigger_end_of_stimulus_fun,
      db_vars = db_vars,
      use_musicassessr_db = TRUE,
      audio_playback_as_single_play_button = TRUE # eventually change
    )

  })



}


