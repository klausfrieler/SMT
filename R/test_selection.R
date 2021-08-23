SMT_test_selection_page <- function(default_items = NULL) {
    ui <- shiny::div(
      tagify(psychTestR::i18n("TEST_SELECTION")),
      make_ui_test_select(default_items,
        "ui_test_select"
      )
    )
    get_answer <- function(state, input, ...) {
      task_groups <- get_item_definitions() %>% pull(task_group)
      test_def <-
        map_dfr(task_groups, function(tg){
        tibble(task_group = tg, num_items = as.integer(input[[tolower(tg)]]))
      })
      psychTestR::set_local("test_def", test_def %>% filter(num_items > 0), state)
    }
      psychTestR::page(
        ui = ui,
        label = "test_selection",
        get_answer = get_answer,
        save_answer = FALSE,
    )
  }

make_ui_test_select <- function(default_items = NULL, label, id = "response_ui") {
  pitch_choices <- c(1L, 3L, 5L, 10L, 20L, 30L, 40L, 50L)
  test_def <- get_item_definitions() %>% mutate(choices = map(num_items, ~{c(0L, 3L, 5L, seq(10L, .x, 10L))}))

  test_def[test_def$task_group == "PITCH",]$choices[[1]]  <- pitch_choices

  boxes <- shiny::tagList(
    map(unique(test_def$task_group), function(tg){
      selected <- test_def[test_def$task_group == tg,]$choices[[1]]
      if(!is.null(default_items)){
        selected <- selected[selected <= default_items]
        selected <- selected[length(selected)]

      }
      shiny::selectizeInput(tolower(tg),
                            label = psychTestR::i18n(sprintf("%s_LABEL", tg)),
                            choices = test_def[test_def$task_group == tg,]$choices[[1]],
                            selected = selected,
                            multiple = FALSE)

    }))

  selectboxes <-

  shiny::tags$div(id = "tsb", style = "width: 100px",
                  shiny::div(shiny::div(id = id, boxes)),
                  psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
}
