options(shiny.error = browser)
debug_locally <- !grepl("shiny-server", getwd())


#' Standalone SMT
#'
#' This function launches a standalone testing session for the SMT
#' This can be used for data collection, either in the laboratory or online.
#' @param title (Scalar character) Title to display during testing.
#' @param max_items_per_task (Scalar integer) Max. number of items per task group.
#' @param with_id (Scalar boolean) Indicates, if ID should be asked for. Defaults to TRUE
#' @param with_feedback (Scalar boolean) Indicates if performance feedback will be given at the end of the test. Defaults to  FALSE
#' @param with_selection (Scalar boolean) Indicates, if test configuration page shall be displayed. Defaults to TRUE
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed.  Defaults to  TRUE
#' @param with_interim_feedback (Scalar boolean) Indicates, if feedback after each task group shall be given
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"en"}) and German (\code{"de"})
#' The first language is selected by default
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto" for default validation
#' which means ID should consist only of  alphanumeric characters.
#' @param ... Further arguments to be passed to \code{\link{SMT}()}.
#' @export

SMT_standalone  <- function(title = NULL,
                            with_id = FALSE,
                            with_feedback = TRUE,
                            with_selection = FALSE,
                            with_welcome = TRUE,
                            with_interim_feedback = FALSE,
                            admin_password = "conifer",
                            researcher_email = "longgoldstudy@gmail.com",
                            languages = c("en", "de"),
                            dict = SMT::SMT_dict,
                            validate_id = "auto",
                            max_items_per_task = 50,
                            ...) {
  feedback <- NULL
  if(with_feedback) {
    #feedback <- SMT::SMT_feedback_with_graph()
    feedback <- SMT::SMT_feedback_with_graph()
  }
  elts <- psychTestR::join(
    if(with_welcome) SMT_welcome_page(dict = dict),
    if(with_id)
      psychTestR::new_timeline(
        psychTestR::get_p_id(prompt = shiny::p(psychTestR::i18n("ENTER_ID"), style = "width:60%;text-align:justify"),
                             button_text = psychTestR::i18n("CONTINUE"),
                             validate = validate_id),
        dict = dict),
      SMT::SMT(max_items_per_task = max_items_per_task,
               with_welcome =  FALSE,
               with_finish = FALSE,
               with_selection = with_selection,
               feedback = feedback,
               dict = dict,
               ...),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    SMT_final_page(dict = dict)
  )
  if(is.null(title)){
    #extract title as named vector from dictionary
    title <-
      SMT::SMT_dict  %>%
      as.data.frame() %>%
      dplyr::filter(key == "TESTNAME") %>%
      dplyr::select(-key) %>%
      as.list() %>%
      unlist()
    names(title) <- tolower(names(title))
  }

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = FALSE,
                                   languages = tolower(languages)))
}
