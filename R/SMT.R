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
#' @param take_training (Logical scalar) Whether to include the training phase. Defaults to FALSE
#' @param with_finish (Scalar boolean) Indicates, if a finish (not final!) page shall be displayed. Defaults to TRUE
#' @param label (Character scalar) Label to give the SMT results in the output file.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export

SMT <- function(max_items_per_task = 50,
                with_welcome = TRUE,
                take_training = FALSE,
                with_finish = TRUE,
                label = "SMT",
                feedback = SMT_feedback_with_score(),
                dict = SMT::SMT_dict
                ) {
  audio_dir <-"https://s3.eu-west-1.amazonaws.com/media.dots.org/stimuli/SMT/"
  stopifnot(purrr::is_scalar_character(label),
            purrr::is_scalar_integer(num_items) || purrr::is_scalar_double(num_items),
            purrr::is_scalar_character(audio_dir),
            psychTestR::is.timeline(feedback) ||
              is.list(feedback) ||
              psychTestR::is.test_element(feedback) ||
              is.null(feedback))
  audio_dir <- gsub("/$", "", audio_dir)

  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) SMT_welcome_page(),
     # if (take_training) psychTestR::new_timeline(instructions(audio_dir),
     #                                             dict = dict),
    psychTestR::new_timeline(
      main_test(audio_dir = audio_dir, max_items_per_task),
      dict = dict),
    scoring(),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    feedback,
    if(with_finish) SMT_finished_page(),
    psychTestR::end_module())
}
