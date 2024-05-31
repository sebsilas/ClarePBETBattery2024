

#' Run the Suzuki Battery
#'
#' @param title
#' @param app_name
#' @param instrument
#' @param show_non_music_tests
#' @param max_goes
#' @param show_music_tests
#' @param user_id
#'
#' @return
#' @export
#'
#' @examples
run_battery <- function(title = "Playing by Ear",
                        app_name = 'pbetsuzuki2024',
                        instrument = c("Violin", "Cello"),
                        show_non_music_tests = TRUE,
                        max_goes = 3L,
                        show_music_tests = TRUE,
                        user_id = 1L) {

  instrument <- match.arg(instrument)

  tl <- function() {
    suzuki_tl(instrument = instrument, app_name = app_name, show_non_music_tests = show_non_music_tests, max_goes = max_goes, show_music_tests = show_music_tests, user_id = user_id)
  }

  musicassessr::make_musicassessr_test(

    welcome_page = musicassessr::empty_code_block(),

    title = title,

    admin_password = Sys.getenv("ADMIN_PASSWORD"),

    elts = tl,

    opt = musicassessr::musicassessr_opt(setup_pages = FALSE,
                                         visual_notation = TRUE,
                                         app_name = app_name,
                                         get_p_id = TRUE,
                                         asynchronous_api_mode = TRUE,
                                         experiment_id = 1L,
                                         user_id = 60L, # Clare's user test ID
                                         css = "https://musicassessr.com/assets/css/style_songbird.css"),

    final_page = psychTestR::final_page("Thank you for taking part in the test!")

  )


}


suzuki_tl <- function(num_items = 24, instrument = c("Violin", "Viola", "Cello"), app_name, show_non_music_tests = TRUE, max_goes = 3L, show_music_tests = TRUE, user_id) {

  instrument <- match.arg(instrument)

  inst_id <- if(instrument == "Violin") 14L else if(instrument == "Viola") 15L else if(instrument == "Cello") 16L else stop("Not a valid instrument")

  stimuli <- get_items(instrument = instrument)


  audio_block <- psychTestR::join(
    psychTestR::one_button_page("Now you will play back some melodies as audio"),
    suzuki_audio_block(stimuli, instrument, max_goes, user_id)
  )

  non_music_tests <- psychTestR::join(

    # - demographics (age, gender, nationality, highest educational level obtained.

    psyquest::DEG(year_range = c(1930, 2024)),


    # - GMSI-musical training subscale

    psyquest::GMS(subscales = "Musical Training"),

    # - Concurrent musical activities

    psyquest::CCM(),

    custom_questions(),

    #
    # - JAJ (8 items)

    JAJ::JAJ(num_items = 8L, feedback = NULL)

  )


  music_tests <- psychTestR::join(
    #  - SAA (5 rhythmic, 5 arhythmic items)

    psychTestR::one_button_page(
      shiny::tags$div(
        shiny::tags$link(rel="stylesheet", type="text/css", href="https://musicassessr.com/assets/css/style_songbird.css"),
        shiny::tags$script("var upload_to_s3 = true; console.log('Turning S3 mode on');"),
        shiny::tags$p(paste0("Let's proceed!"))
      )
    ),


    SAA::SAA(app_name = app_name,
             rhythmic_item_bank = Berkowitz_easy,
             max_goes = 1L,
             num_items = list(
               #long_tones = 6L,
               long_tones = 0L,
               arrhythmic = 0L,
               rhythmic = 5L),
             absolute_url = "https://musicassessr.com/suzuki-pbet-2024/",
             skip_setup = 'except_microphone',
             experiment_id = 3L, # Clare experiment ID
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


    # Just use PBET wrapper for instructions and setup, but append audio block at end

    psychTestR::one_button_page("Now you will need your instrument. Please make sure your instrument is tuned to A=440 before you proceed."),

    PBET::PBET(
      app_name = app_name,
      experiment_id = 3L, # For Clare's exp
      num_items = list(interval_perception = 0L,
                       find_this_note = 0L,
                       arrhythmic = 0L,
                       rhythmic = 0L, # We're only using for the instructions
                       wjd_audio = list(key_easy = 0L, key_hard = 0L)
      ),
      num_examples = PBET::no_examples(),
      skip_setup = TRUE, # this is done at the musicassessr_test level
      max_goes = max_goes,
      melody_length = 3:15,
      default_range = musicassessr::set_default_range(instrument),
      gold_msi = FALSE,
      demographics = FALSE,
      asynchronous_api_mode = TRUE,
      get_answer_function_audio = musicassessr::get_answer_add_trial_and_compute_trial_scores_s3,
      user_id = 60L, # Clare experiment user,
      append_trial_block_after = list(musicassessr::wrap_musicassessr_timeline(audio_block)),
      instrument_id = inst_id)
  )


  psychTestR::join(

    if(show_non_music_tests) non_music_tests,

    if(show_music_tests) music_tests

    # Other PBET block..
  ) %>% consentr::consent(intro_debrief = system.file("extdata/intro_debrief.xlsx", package = "ClarePBETBattery2024"),
                          need_age_consent = FALSE)

}



suzuki_audio_block <- function(selected_audio, instrument = c("Violin", "Cello"), max_goes = 3L, user_id) {

  instrument <- match.arg(instrument)

  file_path <- if(instrument == "Violin") 'Berk_Cut_melodies_violin_noclick' else if(instrument == "Cello") "Berk_Cut_melodies_cello_noclick" else stop ("Not valid instrument")

  shiny::addResourcePath(prefix = 'audio',
                  directoryPath = system.file(file_path, package = 'ClarePBETBattery2024'))


  total_no_melodies <- nrow(selected_audio)

  audio_block <- selected_audio %>%
    dplyr::mutate(melody_no = dplyr::row_number() ) %>%
    purrr::pmap(iterate_row, total_no_melodies = total_no_melodies, max_goes = max_goes, user_id = user_id) %>%
    unlist() %>%
    musicassessr::wrap_musicassessr_timeline(language = "en")

}



# Function to convert row to dataframe
iterate_row <- function(..., instrument = c("Violin", "Viola", "Cello"), total_no_melodies, max_goes = 3L, user_id) {

  instrument <- match.arg(instrument)

  # Convert the row to a DF
  tb_row <- tibble::as_tibble(list(...))

  if(instrument == "Violin") {
    audio_file <- tb_row$`Violin Audio File name`
  }

  audio_file_path <- paste0('audio/', audio_file)

  single_trial_page(tb_row, audio_file_path, audio_file, total_no_melodies, max_goes, attempts_left = 2L, melody_no = tb_row$melody_no, user_id = user_id)

}


single_trial_page <- function(tb_row,
                              audio_file_path,
                              audio_file,
                              total_no_melodies,
                              max_goes = 3L,
                              attempts_left,
                              melody_no,
                              user_id) {

  psychTestR::join(

    psychTestR::code_block(function(state, ...) {

      # Repeat melody logic stuff
      psychTestR::set_global("user_satisfied", "Try Again", state)
      psychTestR::set_global("number_attempts", 1, state)
      psychTestR::set_global("max_goes", max_goes, state)
      psychTestR::set_global("attempts_left", max_goes, state)

    }),

    # Keep in loop until the participant confirms they are happy with their entry
    psychTestR::while_loop(test = function(state, ...) {
      number_attempts <- psychTestR::get_global("number_attempts", state)
      user_answer <- psychTestR::get_global("user_satisfied", state)
      user_wants_to_play_again <- user_answer == "Try Again"
    },
    logic = list(

      psychTestR::reactive_page(function(state, ...) {

        db_vars <- if(psychTestR::get_global("asynchronous_api_mode", state)) {

          list(
            midi_vs_audio = "audio",
            stimuli = tb_row$abs_melody,
            stimuli_durations = tb_row$durations,
            trial_time_started = Sys.time(),
            instrument = psychTestR::get_global("inst", state),
            attempt = psychTestR::get_global('number_attempts', state),
            item_id = tb_row$item_id,
            display_modality = "audio",
            phase = "test",
            rhythmic = TRUE,
            session_id = musicassessr::get_promise_value(psychTestR::get_global("session_id", state)),
            test_id = 2L, # PBET
            review_items_id = NULL,
            new_items_id = NULL,
            user_id = user_id
          )
        } else NULL

          # Grab various variables
          number_attempts <- psychTestR::get_global("number_attempts", state)
          max_goes <- psychTestR::get_global("max_goes", state)
          attempts_left <- psychTestR::get_global("attempts_left", state) - 1L

          musicassessr::present_stimuli(
            stimuli = audio_file_path,
            stimuli_type = "audio",
            display_modality = "auditory",
            page_title = "Play the melody by ear",
            page_text = shiny::tags$div(
              musicassessr::set_melodic_stimuli(itembankr::str_mel_to_vector(tb_row$abs_melody),
                                                itembankr::str_mel_to_vector(tb_row$durations)),
              shiny::tags$p("Play the melody by ear then click Stop when you are finished."),
              shiny::tags$p(tb_row$prompt)
            ),
            page_type = "record_audio_page",
            get_answer = musicassessr::get_answer_add_trial_and_compute_trial_scores_s3,
            hideOnPlay = TRUE,
            page_label = audio_file,
            answer_meta_data = tb_row,
            trigger_end_of_stimulus_fun = musicassessr::paradigm(paradigm_type = "call_and_response")$trigger_end_of_stimulus_fun,
            db_vars = db_vars,
            audio_playback_as_single_play_button = TRUE,
            happy_with_response = TRUE,
            attempts_left = attempts_left,
            max_goes = max_goes,
            total_no_melodies = total_no_melodies,
            show_progress = TRUE,
            melody_no = melody_no
          )

        }),

      musicassessr::update_play_melody_loop_and_save(max_goes)
      )
    )
  )
}


# gamifyr::scp_up("GDPR.pdf")
# gamifyr::scp_up("InformationSheet.pdf")
# gamifyr::scp_up("Debrief.pdf")
