#' SMT
#'
#' This function defines a SMT  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the SMT in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#'
#' For demoing the SMT, consider using \code{\link{SMT_demo}()}.
#' For a standalone implementation of the SMT,
#' consider using \code{\link{SMT_standalone}()}.
#' @param max_items_per_task (Scalar integer) Max. number of items per task group.
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed. Defaults to TRUE
#' @param with_selection (Scalar boolean) Indicates, if test configuration page shall be displayed. Defaults to TRUE
#' @param with_finish (Scalar boolean) Indicates, if a finish (not final!) page shall be displayed. Defaults to TRUE
#' @param with_interim_feedback (Scalar boolean) Indicates, if feedback after each task group shall be given
#' @param label (Character scalar) Label to give the SMT results in the output file.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export

SMT <- function(max_items_per_task = 50L,
                with_welcome = TRUE,
                with_selection = FALSE,
                with_interim_feedback = FALSE,
                with_finish = TRUE,
                label = "SMT",
                feedback = SMT_feedback_with_score(),
                dict = SMT::SMT_dict
                ) {
  audio_dir <-"https://s3.eu-west-1.amazonaws.com/media.dots.org/stimuli/SMT/"
  stopifnot(purrr::is_scalar_character(label),
            purrr::is_scalar_integer(max_items_per_task) || purrr::is_scalar_double(max_items_per_task),
            purrr::is_scalar_character(audio_dir),
            psychTestR::is.timeline(feedback) ||
              is.list(feedback) ||
              psychTestR::is.test_element(feedback) ||
              is.null(feedback))
  audio_dir <- gsub("/$", "", audio_dir)

  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) SMT_welcome_page(),
    if(with_selection)
      psychTestR::new_timeline(
        SMT_test_selection_page(default_items = max_items_per_task),
        dict = dict),
    psychTestR::new_timeline(
      psychTestR::code_block(function(state,...){
        #browser()
        test_def <- psychTestR::get_local("test_def", state)
        if(is.null(test_def)){
          test_def <- get_item_definitions()
          if(!is.null(max_items_per_task)){
            test_def <- test_def %>% mutate(num_items = max(max_items_per_task, 1))
          }
        }
        psychTestR::set_local("test_def", value = test_def, state)
      }),
      dict = dict),
    psychTestR::new_timeline(
      main_test(audio_dir = audio_dir,
                max_items_per_task,
                with_feedback = with_interim_feedback),
      dict = dict),
    scoring(),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    feedback,
    if(with_finish) SMT_finished_page(),
    psychTestR::end_module())
}

