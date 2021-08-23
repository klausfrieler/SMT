get_item_definitions <- function(){
  SMT::SMT_item_bank %>%
    mutate(task_group = factor(task_group) %>% forcats::fct_inorder()) %>%
    count(task_group) %>%
    mutate(task_group = as.character(task_group)) %>%
    rename(num_items = n)
}

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
    psychTestR::save_result(place = state,
                            label = "scores",
                            value = results)

  })
}

get_prompt <- function(item_number, num_items, task_group = "PITCH", practice_page = FALSE) {
  force(item_number)
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

get_conditional <- function(task_group, item_id){
  item_id <- force(item_id)
  function(state, ...){
    test_def <- psychTestR::get_local("test_def", state)
    if(task_group %in% test_def$task_group) {
      if(item_id <= test_def[test_def$task_group == task_group,]$num_items){
        return(TRUE)
      }
    }
    return(FALSE)
  }
}

create_item <- function(items, task_group, item_id, audio_dir){
  i <- item_id
  psychTestR::reactive_page(function(state, ...){
    #browser()
    eff_num_items <- psychTestR::get_local("test_def", state) %>%
      filter(task_group == !!task_group) %>%
      pull(num_items)
    SMT_item(label = sprintf("q%s%d", task_group, i),
             task_group,
             correct_answer = items[i,]$correct,
             num_choices = items[i, ]$number_choices,
             prompt = get_prompt(i,
                                 eff_num_items,
                                 task_group = task_group,
                                 practice_page = FALSE),
             audio_file = items$audio_file[i],
             audio_dir = audio_dir,
             save_answer = TRUE)
  })
}

get_task_intro <- function(task_group){
  psychTestR::conditional(
    test = get_conditional(task_group, -1L),
    logic =   psychTestR::one_button_page(body  = shiny::p(
      psychTestR::i18n(sprintf("%s_INTRO", task_group)),
      style = "text-align:justify;margin-left:20%;margin-right:20%"
      ))
  )
}

get_task_feedback <- function(task_group){
  psychTestR::conditional(
    test = get_conditional(task_group, -1L),
    logic = .feedback_with_score(task_group)
  )
}

create_test_pages <- function(num_items,
                              task_group,
                              with_feedback = TRUE,
                              audio_dir = "https://s3.eu-west-1.amazonaws.com/media.dots.org/stimuli/SMT/") {
  task_intro <- get_task_intro(task_group)
  items <- SMT::SMT_item_bank %>% filter(task_group == !!task_group)
  ret <- c()
  if(num_items < 0 ){
    browser()
  }
  for(i in 1:num_items){

     # messagef("Added item #%d for task group %s stimulus = %s, correct= %d",
     #          i,
     #          task_group,
     #          items[i,]$audio_file,
     #          items[i,]$correct)

    ret <- c(ret,
             psychTestR::conditional(
               test = get_conditional(task_group, i),
               logic = create_item(items, task_group, i, audio_dir))
             )

  }
  #browser()

  c(task_intro, ret, if(with_feedback)get_task_feedback(task_group))
}

main_test <- function(audio_dir = "https://s3.eu-west-1.amazonaws.com/media.dots.org/stimuli/SMT/",
                      max_items_per_task = 50,
                      with_feedback = TRUE) {
  task_groups <- get_item_definitions()
  mt <- c()
  #browser()
  max_items_per_task <- max(1, max_items_per_task)
  for(i in 1:nrow(task_groups)){
    num_items <- min(task_groups[i, ]$num_items, max_items_per_task)
    elts <- create_test_pages(num_items, task_groups[i, ]$task_group, audio_dir = audio_dir, with_feedback = with_feedback)
    mt <- c(mt, elts)
  }
  return(mt)
}

SMT_welcome_page <- function(dict = SMT::SMT_dict){
  shiny::addResourcePath("www_smt", system.file("www", package = "SMT"))

  psychTestR::new_timeline(
    psychTestR::one_button_page(
    body = shiny::div(
      shiny::h3(psychTestR::i18n("WELCOME")),
      shiny::div(shiny::img(
        src = "www_smt/images/SMT_splash_screen.jpg",
        width = 300,
        style = "margin-top: 30px;margin-bottom: 30px"
      ))
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

