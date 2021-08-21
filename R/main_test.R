

get_item_sequence <- function(){
  1:nrow(SMT::SMT_item_bank)
}

# swap_by_sep <- function(name, sep = "."){
#   esc_sep <- sep
#   if (sep == "."){
#     esc_sep <- "\\."
#   }
#   tmp <- map_chr(str_split(name, esc_sep), function(x) {
#     if(length(x) != 2){
#       paste(x, sep = sep, collapse = sep)
#     }
#     else{
#       paste(x[[2]], x[[1]], sep = sep, collapse = sep)
#     }
#     })
#   tmp
# }

scoring <- function(){
  psychTestR::code_block(function(state,...){
    results <- psychTestR::get_results(state = state,
                                       complete = FALSE,
                                       add_session_info = FALSE) %>% as.list()
    #browser()
    results <- results$SMT %>% bind_rows()
    results <- results %>%
      group_by(task_group) %>%
      summarise(sum_score = sum(correct), mean_score = mean(correct), n_items = n())
    # %>%
    #   pivot_wider(names_sep = ".",
    #               names_from = task_group,
    #               values_from = c(sum_score, mean_score, n_items)) %>%
    #   set_names(swap_by_sep(names(.)))
    psychTestR::save_result(place = state,
                            label = "scores",
                            value = results)

  })
}

get_prompt <- function(item_number, num_items, task_group = "PITCH", practice_page = FALSE) {
  key <- "PROGRESS_TEXT"
  if(practice_page){
    key <- "SAMPLE_PROGRESS_TEXT"
  }
  shiny::div(
    shiny::h4(
      psychTestR::i18n(
        key,
        sub = list(num_question = item_number,
                   test_length = if (is.null(num_items))
                     "?" else
                       num_items)),
      style  = "text_align:center"
    ),
    shiny::p(
      psychTestR::i18n(sprintf("%s_PROMPT", task_group)),
      style = "margin-left:20%;margin-right:20%;text-align:center")
  )
}

create_test_pages <- function(num_items, task_group, audio_dir = "https://s3.eu-west-1.amazonaws.com/media.dots.org/stimuli/SMT/") {
  task_intro <- psychTestR::one_button_page(body  = shiny::p(
    psychTestR::i18n(sprintf("%s_INTRO", task_group)),
    style = "text-align:justify;margin-left:20%;margin-right:20%"
  ))
  items <- SMT::SMT_item_bank %>% filter(task_group == !!task_group)
  ret <- c()
  for(i in 1:num_items){

    messagef("Added item #%d for task group %s stimulus = %s, correct= %d",
             i,
             task_group,
             items[i,]$audio_file,
             items[i,]$correct)
    item <- SMT_item(label = sprintf("q%s%d", task_group, i),
                     task_group,
                     correct_answer = items[i,]$correct,
                     num_choices = items[i, ]$number_choices,
                     prompt = get_prompt(i,
                                         num_items,
                                         task_group = task_group,
                                         practice_page = FALSE),
                     audio_file = items$audio_file[i],
                     audio_dir = audio_dir,
                     save_answer = TRUE)
    ret <- c(ret, item)

  }
  #browser()

  c(task_intro, ret)
}

main_test <- function(audio_dir = "https://s3.eu-west-1.amazonaws.com/media.dots.org/stimuli/SMT/", max_items_per_task = 50) {
  task_groups <- SMT::SMT_item_bank %>%
    mutate(task_group = factor(task_group) %>% forcats::fct_inorder()) %>%
    count(task_group)
  mt <- c()
  #browser()
  for(i in 1:nrow(task_groups)){
    num_items <- min(task_groups[i, ]$n, max_items_per_task)
    elts <- create_test_pages(num_items, task_groups[i, ]$task_group, audio_dir = audio_dir)
    mt <- c(mt, elts)
  }
  return(mt)
}

SMT_welcome_page <- function(dict = SMT::SMT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
    body = shiny::div(
      shiny::h4(psychTestR::i18n("WELCOME")),
      # shiny::div(psychTestR::i18n("INTRO_TEXT"),
      #          style = "margin-left:0%;display:block")
    ),
    button_text = psychTestR::i18n("CONTINUE")
  ), dict = dict)
}

SMT_finished_page <- function(dict = SMT::SMT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body =  shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        psychTestR::i18n("COMPLETED"),
                         style = "margin-left:0%;display:block"),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

SMT_final_page <- function(dict = SMT::SMT_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        shiny::div(psychTestR::i18n("COMPLETED"),
                   style = "margin-left:0%;display:block"),
        button_text = psychTestR::i18n("CONTINUE")
      )
    ), dict = dict)
}

