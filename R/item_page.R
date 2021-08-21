media_js <- list(
  media_not_played = "var media_played = false;",
  media_played = "media_played = true;",
  play_media = "document.getElementById('media').play();",
  show_media   = paste0("if (!media_played) ",
                        "{document.getElementById('media')",
                        ".style.visibility='inherit'};"),
  hide_media   = paste0("if (media_played) ",
                          "{document.getElementById('media')",
                          ".style.visibility='hidden'};"),
  show_media_btn = paste0("if (!media_played) ",
                          "{document.getElementById('btn_play_media')",
                          ".style.visibility='inherit'};"),
  hide_media_btn = paste0("document.getElementById('btn_play_media')",
                          ".style.visibility='hidden';"),
  show_responses = "document.getElementById('response_ui').style.visibility = 'inherit';"
)

#media_mobile_play_button <- shiny::tags$button(
#  shiny::tags$strong(psychTestR::i18n("CLICK_HERE_TO_PLAY")),
#  id = "btn_play_media",
#  style = "visibility: visible;height: 50px",
#  onclick = media_js$play_media
#)

media_mobile_play_button <- shiny::tags$p(
  shiny::tags$button(shiny::tags$span("\u25B6"),
                     type = "button",
                     id = "btn_play_media",
                     style = "visibility: hidden",
                     onclick = media_js$play_media)
)

get_audio_ui <- function(url,
                         type = tools::file_ext(url),
                         autoplay = TRUE,
                         width = 0,
                         wait = TRUE,
                         loop = FALSE) {
  #print(url)
  stopifnot(purrr::is_scalar_character(url),
            purrr::is_scalar_character(type),
            purrr::is_scalar_logical(wait),
            purrr::is_scalar_logical(loop))
  src    <- shiny::tags$source(src = url, type = paste0("audio/", type))
  script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
  audio  <- shiny::tags$audio(
    script,
    src,
    id = "media",
    preload = "auto",
    autoplay = if(autoplay) "autoplay",
    width = width,
    loop = if (loop) "loop",
    oncanplaythrough = media_js$show_media_btn,
    onplay = paste0(media_js$media_played, media_js$hide_media_btn),
    #onended = if (wait) paste0(media_js$show_responses, media_js$hide_media) else "null",
    onended = if (wait) media_js$show_responses else "null"
  )
  shiny::tags$div(audio, media_mobile_play_button)
}

get_audio_element <- function(url,
                              type = tools::file_ext(url),
                              wait = F,
                              autoplay = FALSE,
                              width = 200,
                              height = 50,
                              id = "media") {
  #print(url)
  stopifnot(purrr::is_scalar_character(url),
            purrr::is_scalar_character(type)
            )
  src    <- shiny::tags$source(src = url, type = paste0("audio/", type))
  script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
  audio  <- shiny::tags$audio(
    src,
    script,
    id = id,
    preload = "auto",
    controls = "controls",
    controlslist = "nodownload noremoteplayback",
    autoplay = if(autoplay) "autoplay",
    width = width,
    height = height,
    onplay = paste0(media_js$media_played, media_js$hide_media),
    onended = if (wait) paste0(media_js$show_responses, media_js$hide_media) else "null"
  )
  audio
}

audio_NAFC_page_flex <- function(label,
                                 prompt,
                                 choices,
                                 audio_url,
                                 correct_answer,
                                 save_answer = TRUE,
                                 get_answer = NULL,
                                 on_complete = NULL,
                                 admin_ui = NULL) {
  stopifnot(purrr::is_scalar_character(label))
  audio_ui <- get_audio_ui(audio_url, wait = T, loop = F, width = 200)
  style <- NULL
  ui <- shiny::div(
    tagify(prompt),
    audio_ui,
    psychTestR::make_ui_NAFC(as.character(1:length(choices)),
                             labels = choices,
                             hide = TRUE,
                             arrange_vertically = FALSE,
                             id = "response_ui")
    )

  validate <- function(answer, ...) !is.null(answer)
  psychTestR::page(ui = ui, label = label,
                   get_answer = get_answer, save_answer = save_answer,
                   validate = validate, on_complete = on_complete,
                   final = FALSE,
                   admin_ui = admin_ui)
}



SMT_item <- function(label = "",
                     task_group,
                     audio_file,
                     correct_answer,
                     num_choices,
                     prompt = "",
                     audio_dir = "",
                     save_answer = TRUE,
                     on_complete = NULL,
                     get_answer = NULL,
                     instruction_page = FALSE
){
  #browser()
  #num_choices <- 2
  #if(task_group == "TONAL_MEMORY"){
  #  num_choices <- 5
  #}
  choices <- map(1:num_choices, ~{ psychTestR::i18n(sprintf("%s_CHOICE%s", task_group, .x))})

  audio_url <- file.path(audio_dir, sprintf("%s.mp3", audio_file))
  force(correct_answer)
  get_answer <- function(input, state, ...) {
    #browser()
    answer <- as.numeric(input$last_btn_pressed)
    correct <- correct_answer == answer
    result <- tibble(answer = answer,
                     correct_answer = correct_answer,
                     correct = correct,
                     # stimulus = audio_file,
                     task_group =  task_group)
    #increase item_number
    item_number <- psychTestR::get_local(key = "item_number", state = state)
    psychTestR::set_local(key = "item_number", value = item_number + 1L , state = state)
    #messagef("Increased item_number to %d", item_number + 1L)

    result
  }
  audio_NAFC_page_flex(label = label,
                       prompt = prompt,
                       audio_url = audio_url,
                       choices = choices,
                       correct_answer = correct_answer,
                       save_answer = save_answer,
                       get_answer = get_answer,
                       on_complete = on_complete)
}

