

# The Labor Demand example

wage2 <- data.table::fread("data/data_verbeek/labour2.prn") %>% janitor::clean_names()


# Model
model2 <- wage2 %>%
  mutate_all( ~log(.x)) %>%
  rename_all( ~str_c("log_", .x)) %>%
  lm( log_labour ~ ., data = .)

model2 %>% summary()