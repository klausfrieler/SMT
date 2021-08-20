SMT_dict_raw <- readxl::read_xlsx("data_raw/SMT_dict.xlsx")
SMT_dict <- psychTestR::i18n_dict$new(SMT_dict_raw)
usethis::use_data(SMT_dict, overwrite = TRUE)
