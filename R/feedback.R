#' SMT feedback (with score)
#'
#' Here the participant is given textual feedback at the end of the test.
#' @param task_group (character scalar), specificy single task group feedback (internal use only)
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
#' @examples
#' \dontrun{
#' SMT_demo(feedback = SMT_feedback_with_score())}

SMT_feedback_with_score <- function(task_group = NULL, dict = SMT::SMT_dict) {
    psychTestR::new_timeline(
      .feedback_with_score(task_group),
    dict = dict
  )
}

.feedback_with_score <- function(task_group = NULL){
  psychTestR::reactive_page(function(state, ...) {
    results <- psychTestR::get_results(state = state,
                                       complete = TRUE,
                                       add_session_info = FALSE) %>% as.list()
    if("scores" %in% names(results$SMT)){
      scores <- results$SMT$scores %>% filter(n_items > 0)
    }
    else{
      scores <- results$SMT %>% bind_rows() %>%
        group_by(task_group) %>%
        summarise(sum_score = sum(correct), mean_score = mean(correct), n_items = dplyr::n())
    }
    if(!is.null(task_group)){
      scores <- scores %>% filter(task_group == !!task_group)
      task_label <- psychTestR::i18n(sprintf("%s_LABEL", scores$task_group))
      res <- sprintf("%s/%s (%s %%)",
                     scores$sum_score,
                     scores$n_items,
                     round(scores$mean_score*100))
      psychTestR::page(
        ui = shiny::div(
          shiny::h4(psychTestR::i18n("SCORE_HEADER_SINGLE", sub = list(task_group = task_label, result = res))),
          shiny::p(psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
        )
      )
    }
    else{
      task_labels <- map(sprintf("%s_LABEL", scores$task_group), psychTestR::i18n)
      res <- sprintf("%s: %s/%s (%s %%)", task_labels, scores$sum_score, scores$n_items, round(scores$mean_score*100))
      text_finish <- shiny::tagList(map(res, ~{shiny::p(.x, style ="width:300px;text-align:justify")}))
      psychTestR::page(
        ui = shiny::div(
          shiny::h4(psychTestR::i18n("SCORE_HEADER")),
          shiny::div(text_finish, style = "margin-auto"),
          shiny::p(psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
        )
      )
    }
  }
  )
}

SMT_feedback_bar_chart <- function(data){
  data <- data %>%
    mutate(task_group = map(sprintf("%s_LABEL", data$task_group), psychTestR::i18n) %>% unlist()) %>%
    mutate(sum_label = sprintf("%s/%s", sum_score, n_items))
  q <- data %>% ggplot2::ggplot(ggplot2::aes(x = task_group, y = mean_score, fill = task_group))
  q <- q + ggplot2::geom_col()
  q <- q + ggplot2::geom_text(ggplot2::aes(x = task_group, y = .9, label = sum_label))
  q <- q + ggplot2::theme_minimal()
  q <- q + ggplot2::scale_y_continuous(labels = scales::percent)
  q <- q + ggplot2::theme(legend.position = "none")
  q <- q + ggplot2::labs(x = "", y = "")
  #q <- q + ggplot2::geom_text(ggplot2::aes(x = name, y = .1, label = name), color = "white", size = 5)
  q <- q + ggplot2::scale_fill_brewer(palette = "Set1", direction = -1)
  plotly::ggplotly(q, width = 600, height = 450, )
  #q
}


#' SMT feedback (with graph)
#'
#' Here the participant is given textual and graphical feedback at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
#' @examples
#' \dontrun{
#' SMT_demo(feedback = SMT_feedback_with_score())}
SMT_feedback_with_graph <- function(dict = SMT::SMT_dict) {
  psychTestR::new_timeline(
      psychTestR::reactive_page(function(state, ...) {
        #browser()
        results <- psychTestR::get_results(state = state,
                                           complete = TRUE,
                                           add_session_info = FALSE) %>% as.list()
        text_finish <- psychTestR::i18n("FEEDBACK_SHORT", html = TRUE)
        chart <- SMT_feedback_bar_chart(results$SMT$scores)
        psychTestR::page(
          ui = shiny::div(
            shiny::h4(psychTestR::i18n("SCORE_HEADER")),
            shiny::p(text_finish, style ="width:60%;text-align:center"),
            shiny::p(chart),
            #shiny::plotOutput(chart),
            shiny::p(psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
          )
        )
      }
      ),
    dict = dict
  )

}
