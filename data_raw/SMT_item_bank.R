library(tidyverse)
SMT_item_bank <- readxl::read_xlsx("data_raw/SMT_item_bank.xlsx") %>%
  select(item_number:audio_file)
usethis::use_data(SMT_item_bank, overwrite = TRUE)
