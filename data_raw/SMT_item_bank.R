library(tidyverse)
SMT_item_bank <- readxl::read_xlsx("data_raw/SMT_item_bank.xlsx")
usethis::use_data(SMT_item_bank, overwrite = TRUE)
