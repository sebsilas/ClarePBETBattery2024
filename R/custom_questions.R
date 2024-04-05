





agree_disagree <-
  c(
    "Strongly Disagree",
    "Disagree",
    "Neither agree nor disagree",
    "Agree",
    "Strongly Agree"
  )
never_rarely <-
  c("Never", "Rarely", "Sometimes", "Very often", "Always")

custom_questions <- function() {
  psychTestR::join(
    psychTestR::NAFC_page(
      label = "over_age",
      prompt = "Are you over 18 years of age?",
      choices = c("Yes", "No")
    ),

    # If over 18...

    psychTestR::conditional(
      test = function(state, ...) {
        psychTestR::answer(state) == "Yes"
      },

      logic = psychTestR::checkbox_page(
        label = "musician_type",
        prompt = "I am a (you may select more than one)",
        choices = c(
          "Full- or part-time professional musician",
          "Postgraduate music student",
          "Undergraduate music student",
          "Instrumental student without higher education training",
          "Amateur adult"
        ),
        force_answer = TRUE
      )

    ),

    # If under 18...

    psychTestR::conditional(
      function(state, ...) {
        psychTestR::answer(state) == "No"
      },
      psychTestR::join(

        psychTestR::checkbox_page(
          label = "musician_type",
          prompt = "I am a (you may select more than one)",
          choices = c(
            'School pupil attending a junior conservatoire on Saturdays (eg. Junior Trinity, Junior Guildhall etc.)',
            'School pupil with a school music scholarship',
            'School pupil receiving private instrumental lessons (not at school)',
            'School pupil receiving instrumental lessons at school',
            'School pupil receiving instrumental lessons at a music school (eg. Colourstrings),'
            ),
            force_answer = TRUE
          ),

        psychTestR::NAFC_page(
          label = "practice_type",
          prompt = "I mostly practice:",
          choices = c("alone", "with the help of somebody else")
        )


        )

    ),

    psychTestR::NAFC_page(
      label = "primary_genre",
      prompt = "The primary genre of music I have learned and perform is:",
      choices = c("Classical", "Jazz", "Folk", "Other")
    ),

    psychTestR::NAFC_page(
      label = "repertoire_listen",
      prompt = "I listen to the repertoire that I am learning for approx.:",
      choices = c(
        "Less than 1 hour per week",
        "1 hour pw",
        "2 hours pw",
        "3 hours pw",
        "4 hours pw",
        "5+ hours pw"
      )
    ),


    psychTestR::NAFC_page(
      label = "memory_comfortable",
      prompt = " I feel comfortable playing from memory:",
      choices = agree_disagree
    ),

    psychTestR::NAFC_page(
      label = "memory_frequency",
      prompt = "I practice music from memory:",
      choices = never_rarely
    ),

    psychTestR::NAFC_page(
      label = "perform_from_memory",
      prompt = "I perform solo repertoire from memory:",
      choices = never_rarely
    ),

    psychTestR::checkbox_page(
      label = "note_reading",
      prompt = "When reading music, I name notes using:",
      subprompt = "(Please consult your teacher if you are unsure)",
      choices = c(
        "The alphabet (A, B, C…)",
        "Fixed do (do = C)",
        "Moveable do (do = keynote ie. In G major do is G)"
      ),
      force_answer = "one"
    ),

    psychTestR::dropdown_page(
      label = "lesson_type",
      prompt = "My first years of lessons were:",
      choices = c("Suzuki",
                  "Colourstrings",
                  "Rolland"),
      alternative_choice = TRUE
    ),

    psychTestR::NAFC_page(
      label = "kodaly_tuition",
      prompt = "I have had regular Kodaly music tuition:",
      choices = agree_disagree
    ),

    psychTestR::dropdown_page(
      label = "highest_grade",
      prompt = "The highest grade I have passed on this instrument is:",
      choices = as.character(1:8)
    ),

    psychTestR::dropdown_page(
      label = "suzuki_book",
      prompt = "If applicable, I am currently on Suzuki Book:",
      choices = c(as.character(1:8), "Not applicable")
    )

  )

}
