#' SMT feedback (with score)
#'
#' Here the participant is given textual feedback at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
#' @examples
#' \dontrun{
#' SMT_demo(feedback = SMT_feedback_with_score())}

SMT_feedback_with_score <- function(dict = SMT::SMT_dict) {
    psychTestR::new_timeline(
      psychTestR::reactive_page(function(state, ...) {
        #browser()
        results <- psychTestR::get_results(state = state,
                                           complete = TRUE,
                                           add_session_info = FALSE) %>% as.list()
        text_finish <- psychTestR::i18n("FEEDBACK",
                                        html = TRUE,
                                        sub = list(num_questions = results$SMT$num_questions,
                                                   num_correct = round(results$SMT$score * results$SMT$num_questions),
                                                   total_score = results$SMT$total_score,
                                                   max_score = results$SMT$max_score))
        psychTestR::page(
          ui = shiny::div(
            shiny::p(text_finish, style ="width:60%;text-align:justify"),
          )
        )
      }
      ),
    dict = dict
  )
}

SMT_feedback_pie_chart <- function(perc_correct){
  correct_s <- sprintf("%s (%s%%)", psychTestR::i18n("CORRECT_RAW"), round(perc_correct*1000)/10)
  incorrect_s <- sprintf("%s (%s%%)", psychTestR::i18n("INCORRECT_RAW"), round((1-perc_correct)*1000)/10)

  data <- tibble(name = c(correct_s, incorrect_s), value = c(perc_correct, 1 - perc_correct))  %>%
    dplyr::arrange(desc(name)) %>%
    mutate(ypos = cumsum(value)- 0.5 * value )

  q <- data %>% ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = name))
  q <- q + ggplot2::geom_bar(stat = "identity", width = .5, colour = "white")
  #q <- q + ggplot2::coord_polar("y", start = 0)
  #q <- q + ggplot2::coord_flip()
  q <- q + ggplot2::theme_minimal()
  q <- q + ggplot2::scale_y_continuous(labels = scales::percent)
  q <- q + ggplot2::theme(legend.position = "none")
  q <- q + ggplot2::labs(x = "", y = "")
  #q <- q + ggplot2::geom_text(ggplot2::aes(x = name, y = .1, label = name), color = "white", size = 5)
  q <- q + ggplot2::scale_fill_brewer(palette = "Set1", direction = -1)
  plotly::ggplotly(q, width = 600, height = 450, )
  #q
}

SMT_feedback_graph_normal_curve <- function(perc_correct, x_min = 40, x_max = 160, x_mean = 100, x_sd = 15) {
  q <-
    ggplot2::ggplot(data.frame(x = c(x_min, x_max)), ggplot2::aes(x)) +
    ggplot2::stat_function(fun = stats::dnorm, args = list(mean = x_mean, sd = x_sd)) +
    ggplot2::stat_function(fun = stats::dnorm, args=list(mean = x_mean, sd = x_sd),
                           xlim = c(x_min, (x_max - x_min) * perc_correct + x_min),
                           fill = "lightblue4",
                           geom = "area")
  q <- q + ggplot2::theme_bw()
  #q <- q + scale_y_continuous(labels = scales::percent, name="Frequency (%)")
  #q <- q + ggplot2::scale_y_continuous(labels = NULL)
  x_axis_lab <- sprintf(" %s %s", psychTestR::i18n("TESTNAME"), psychTestR::i18n("VALUE"))
  title <- psychTestR::i18n("SCORE_TEMPLATE")
  fake_IQ <- (x_max - x_min) * perc_correct + x_min
  main_title <- sprintf("%s: %.0f", title, round(fake_IQ, digits = 0))

  q <- q + ggplot2::labs(x = x_axis_lab, y = "")
  q <- q + ggplot2::ggtitle(main_title)
  plotly::ggplotly(q, width = 600, height = 450)
}

#' SMT feedback (with graph)
#'
#' Here the participant is given textual and graphical feedback at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
#' @examples
#' \dontrun{
#' SMT_demo(feedback = SMT_feedback_with_score())}
SMT_feedback_with_graph <- function(dict = SMT::SMT_dict, graph = "pie") {
  psychTestR::new_timeline(
      psychTestR::reactive_page(function(state, ...) {
        #browser()
        results <- psychTestR::get_results(state = state,
                                           complete = TRUE,
                                           add_session_info = FALSE) %>% as.list()
        if(graph == "fake_IQ") {#
          x_min <- 40
          x_max <- 160
          total_score <- results$SMT$total_score/results$SMT$max_score
          fake_IQ <- (x_max - x_min) * results$SMT$total_score/results$SMT$max_score + x_min
          text_finish <- psychTestR::i18n("FEEDBACK",
                                          html = TRUE,
                                          sub = list(num_questions = results$SMT$num_questions,
                                                     num_correct = round(results$SMT$score * results$SMT$num_questions),
                                                     total_score = fake_IQ,
                                                     max_score = x_max))
          chart <- SMT_feedback_graph_normal_curve(total_score)
        }
        else{
          text_finish <- psychTestR::i18n("FEEDBACK_SHORT",
                                          html = TRUE,
                                          sub = list(num_questions = results$SMT$num_questions,
                                                     num_correct = round(results$SMT$score * results$SMT$num_questions),
                                                     perc_correct = round(1000 * results$SMT$score)/10))
          chart <- SMT_feedback_pie_chart(results$SMT$score)

        }
        psychTestR::page(
          ui = shiny::div(
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
