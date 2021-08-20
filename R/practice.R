get_feedback <- function(page_no, answer){
  if(page_no == 1){
    return("")
  }
  key <- "INCORRECT"
  if(answer$correct){
    key <- "CORRECT"
  }
  feedbacks <- c("SECOND_MORE_EXPRESSIVE", "SECOND_LESS_EXPRESSIVE", "NO_DIFFERENCE")
  shiny::p(psychTestR::i18n(key),psychTestR::i18n(feedbacks[page_no - 1]))

}


# ask_repeat <- function(prompt) {
#   psychTestR::NAFC_page(
#     label = "ask_repeat",
#     prompt = prompt,
#     choices = c("go_back", "continue"),
#     labels = lapply(c("GO_BACK", "CONTINUE"), psychTestR::i18n),
#     save_answer = FALSE,
#     arrange_vertically = FALSE,
#     on_complete = function(state, answer, ...) {
#       psychTestR::set_local("do_intro", identical(answer, "go_back"), state)
#     }
#   )
# }


get_transition_page <- function(answer){
  psychTestR::one_button_page(body = shiny::div(
    get_feedback(4, answer),
    shiny::p(psychTestR::i18n("MAIN_INTRO"))),
    button_text = psychTestR::i18n("CONTINUE"))
}

make_practice_page <- function(page_no, audio_dir) {
  psychTestR::reactive_page(function(answer, ...) {
    get_practice_page(page_no, answer, audio_dir)
  })
}

get_practice_page <- function(page_no, answer, audio_dir){
  training_answers  <- c(3, 1, 2)
  example_audios <- c("example_increasing.mp3", "example_decreasing.mp3", "example_same.mp3")
  prompt <- shiny::div(get_feedback(page_no, answer), get_prompt(page_no, 3, TRUE))
  if(page_no == 4){
    page <- get_transition_page(answer)
  }
  else{
    page <- SMT_item(label = sprintf("training%s", page_no),
                     correct_answer = training_answers[page_no],
                     prompt = prompt,
                     audio_dir = audio_dir,
                     audio_file = example_audios[page_no],
                     save_answer = FALSE,
                     instruction_page = TRUE)
  }
  page
}

practice <- function(audio_dir) {
  lapply(1:4, make_practice_page, audio_dir) %>% unlist()
}
