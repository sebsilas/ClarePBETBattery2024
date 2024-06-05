





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
    # If over 18...

    psychTestR::conditional(
      test = function(state, ...) {
        psychTestR::get_global("over_18", state) == "Yes"
      },

      logic = psychTestR::checkbox_page(
        label = "musician_type",
        prompt = shiny::tags$div(shiny::tags$h2("Question 1/14"),
                                 shiny::tags$p("I am a (you may select more than one)")),
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
      test = function(state, ...) {
        psychTestR::get_global("over_18", state) == "No"
      },
      logic = psychTestR::join(

        psychTestR::checkbox_page(
          label = "musician_type",
          prompt = shiny::tags$div(shiny::tags$h2("Question 2/14"),
                                   shiny::tags$p("I am a (you may select more than one)")),
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
          prompt = shiny::tags$div(shiny::tags$h2("Question 3/14"),
                                   shiny::tags$p("I mostly practice:")),
          choices = c("alone", "with the help of somebody else")
        )


        )

    ),

    psychTestR::NAFC_page(
      label = "primary_genre",
      prompt = shiny::tags$div(shiny::tags$h2("Question 4/14"),
                               shiny::tags$p("The primary genre of music I have learned and perform is:")),
      choices = c("Classical", "Jazz", "Folk", "Other")
    ),

    psychTestR::NAFC_page(
      label = "repertoire_listen",
      prompt = shiny::tags$div(shiny::tags$h2("Question 5/14"),
                               shiny::tags$p("I listen to the repertoire that I am learning for approx:")),
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
      prompt = shiny::tags$div(shiny::tags$h2("Question 6/14"),
                               shiny::tags$p("I feel comfortable playing from memory:")),
      choices = agree_disagree
    ),

    psychTestR::NAFC_page(
      label = "memory_frequency",
      prompt = shiny::tags$div(shiny::tags$h2("Question 7/14"),
                               shiny::tags$p("I practice music from memory:")),
      choices = never_rarely
    ),

    psychTestR::NAFC_page(
      label = "perform_from_memory",
      prompt = shiny::tags$div(shiny::tags$h2("Question 8/14"),
                               shiny::tags$p("I perform solo repertoire from memory:")),
      choices = never_rarely
    ),

    psychTestR::checkbox_page(
      label = "note_reading",
      prompt = shiny::tags$div(shiny::tags$h2("Question 9/14"),
                               shiny::tags$p("When reading music, I name notes using:")),
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
      prompt = shiny::tags$div(shiny::tags$h2("Question 10/14"),
                               shiny::tags$p("My first years of lessons were:")),
      choices = c("Suzuki",
                  "Colourstrings",
                  "Rolland"),
      alternative_choice = TRUE
    ),

    psychTestR::NAFC_page(
      label = "kodaly_tuition",
      prompt = shiny::tags$div(shiny::tags$h2("Question 11/14"),
                               shiny::tags$p("I have had regular Kodaly music tuition:")),
      choices = c("1-6 months", "6-12 months", "1-2 years", "2-5 years", "5-10 years", "10+ years")
    ),

    psychTestR::dropdown_page(
      label = "highest_grade",
      prompt = shiny::tags$div(shiny::tags$h2("Question 12/14"),
                               shiny::tags$p("The highest grade I have passed on this instrument is:")),
      choices = as.character(c("Not applicable", 1:8))
    ),

    psychTestR::dropdown_page(
      label = "suzuki_book",
      prompt = shiny::tags$div(shiny::tags$h2("Question 13/14"),
                               shiny::tags$p("If applicable, I am currently on Suzuki Book:")),
      choices = c(as.character(1:8), "Not applicable")
    ),

    psychTestR::NAFC_page(
      label = "perfect_pitch",
      prompt = shiny::tags$div(shiny::tags$h2("Question 14/14"),
                               shiny::tags$p("Do you have perfect pitch?")),
      choices = c("Yes", "No", "Not sure")
    )

  )

}
