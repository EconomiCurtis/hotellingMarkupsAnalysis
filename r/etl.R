library(readr)
library(dplyr)
library(readxl)
library(glue)
library(stringr)

# Stack data-----

sessDat <- tibble()
for (sessFile in list.files("../raw_data/production_raw_data/") %>% str_subset("hotellingmarkup")){
  sessDat = bind_rows(
    sessDat,
    read_csv(file = glue("../raw_data/production_raw_data/", sessFile))
  )
  
}

# clean up
sessDat = sessDat %>%
  filter(
    session.code %in% c('0y5rlsk0','bu14vmv7', 'fsbo59wn', 'l1xwokze', 'umllny6i', 'vj364csp')
    &  !is.na(player.boundary_lo)
  ) %>%
  dplyr::group_by(session.code,group.id_in_subsession,player.period_number) %>%
  distinct(
    session.code,participant.code, participant.id_in_session,player.period_number,player.subperiod_number,
    .keep_all = TRUE)

# Helpful New Columns
sessDat = sessDat %>%
  mutate(
    group_size = participant.id_in_session %>% unique %>% length,
    period_half = case_when(
      (player.subperiod_number >= 1 & player.subperiod_number <= 10) ~ "First Half",
      (player.subperiod_number >= 11 & player.subperiod_number <= 20) ~ "Second Half"
    )
  ) %>%
  mutate(
    group_size_str = case_when(
      group_size == 2 ~ "Two Player",
      group_size == 4 ~ "Four Player",
      TRUE ~ "Other"
    )
  )



# sessDat %>%
#   filter(
#     !is.na(player.boundary_lo)
#   ) %>%
#   group_by(session.code) %>%
#   summarise(
#     price_obs = length(!is.na(player.price)) / (unique(group_size))
#   )

# payment -------
SsPay <- read_csv("../raw_data/production_raw_data/201710 subject payments.csv")
# SsPay %>% 
  # group_by(Session) %>%
  # summarise(
  #   n = n(),
  #   payment_aed = sum(Payment),
  #   payment_usd = payment_aed / 3.67 %>% round(2)
  # )


## Utils ------
se <- function(x){
  mean(x) / length(x)^0.5
}

