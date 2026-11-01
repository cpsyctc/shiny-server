library(tidyverse)
library(flextable)
list.files(pattern = "*.R$", recursive = TRUE) %>%
  as_tibble() %>%
  rename(file = value) %>%
  rowwise() %>%
  mutate(lines = list(readLines(str_c("./", file), warn = FALSE))) %>%
  unnest_longer(lines) %>%
  ### we don't need the files
  # select(-file) %>%
  distinct() -> tmpTib

tmpTib %>% 
  filter(str_detect(lines, fixed("library("))) %>%
  ### get rid of "suppressMessages(" as these make parsing harder
  mutate(lines = str_remove(lines,  fixed("suppressMessages("))) %>%
  ### ditto "suppressWarnings(" as these make parsing harder
  mutate(lines = str_remove(lines,  fixed("suppressWarnings("))) %>%
  mutate(library = str_replace(lines, "^(.*?)library\\((.*?)\\)(.*)$", "\\2")) %>% filter(library == "esc")
  select(library) %>%
  distinct() -> tmpTibLibrary

tmpTib %>%
  ### don't reanalyse the filter lines
  filter_out(str_detect(lines, fixed("library("))) %>%
  filter(str_detect(lines, fixed("::"))) %>%
  ### get rid of "suppressMessages(" as these make parsing harder
  mutate(lines = str_remove(lines,  fixed("suppressMessages("))) %>%
  ### ditto "suppressWarnings(" as these make parsing harder
  mutate(lines = str_remove(lines,  fixed("suppressWarnings("))) %>%
  ### now parse simple lines without a RtL assigment
  mutate(library = str_replace(lines, "^(.*?)\\:\\:(.*)$", "\\1"),
         library = str_replace(library, "^(.*?)[[:space:]]{1,1}([[:alnum:]]+)", "\\2"),
         ### now do that again to catch the lines with RtL assigment (and so early spaces)
         library = str_replace(library, "^(.*?)[[:space:]]{1,1}([[:alnum:]]+)", "\\2"),
         ### now handle "package::" within allocations to list
         library = str_remove(library, fixed("list("))) %>%
  select(-lines) %>%
  distinct() %>%
  bind_rows(tmpTibLibrary) %>%
  distinct() %>%
  arrange(library) %>%
  flextable() %>%
  autofit()



         