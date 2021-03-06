% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SMT.R
\name{SMT}
\alias{SMT}
\title{SMT}
\usage{
SMT(
  max_items_per_task = 50L,
  with_welcome = TRUE,
  with_selection = FALSE,
  with_interim_feedback = FALSE,
  with_finish = TRUE,
  label = "SMT",
  feedback = SMT_feedback_with_score(),
  dict = SMT::SMT_dict
)
}
\arguments{
\item{max_items_per_task}{(Scalar integer) Max. number of items per task group.}

\item{with_welcome}{(Scalar boolean) Indicates, if a welcome page shall be displayed. Defaults to TRUE}

\item{with_selection}{(Scalar boolean) Indicates, if test configuration page shall be displayed. Defaults to TRUE}

\item{with_interim_feedback}{(Scalar boolean) Indicates, if feedback after each task group shall be given}

\item{with_finish}{(Scalar boolean) Indicates, if a finish (not final!) page shall be displayed. Defaults to TRUE}

\item{label}{(Character scalar) Label to give the SMT results in the output file.}

\item{feedback}{(Function) Defines the feedback to give the participant
at the end of the test.}

\item{dict}{The psychTestR dictionary used for internationalisation.}
}
\description{
This function defines a SMT  module for incorporation into a
psychTestR timeline.
Use this function if you want to include the SMT in a
battery of other tests, or if you want to add custom psychTestR
pages to your test timeline.
}
\details{
For demoing the SMT, consider using \code{\link{SMT_demo}()}.
For a standalone implementation of the SMT,
consider using \code{\link{SMT_standalone}()}.
}
