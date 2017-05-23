library(readr)
library(dplyr)
library(readxl)

# Stack 2-player data-----

list.files("~/Dropbox/SSEL/hotelling_marketups/hotellingMarkupsAnalysis/raw_data/20170210 all data/")[
    grepl("_2p_",list.files("~/Dropbox/SSEL/hotelling_marketups/hotellingMarkupsAnalysis/raw_data/20170210 all data/"))
    ]

sesDat2p = tibble()
for (sessFile in list.files("~/Dropbox/SSEL/hotelling_marketups/hotellingMarkupsAnalysis/raw_data/20170210 all data/")[
  grepl("_2p_",list.files("~/Dropbox/SSEL/hotelling_marketups/hotellingMarkupsAnalysis/raw_data/20170210 all data/"))
  ]){
  sesDat2p = bind_rows(
    sesDat2p,
    read_excel(paste("~/Dropbox/SSEL/hotelling_marketups/hotellingMarkupsAnalysis/raw_data/20170210 all data/",sessFile,sep=""))
  )
}
sesDat2p = sesDat2p %>%
  filter(!is.na(player.round_payoff))


production_session.code = c("e6zctbfm","qh6mfn23","vtfqykgg")

sesDat2p = sesDat2p %>%
  dplyr::filter(
    session.code %in% c("e6zctbfm","vtfqykgg")
  )

# Stack 4-Player data ------
sesDat4p = tibble()
for (sessFile in list.files("~/Dropbox/SSEL/hotelling_marketups/hotellingMarkupsAnalysis/raw_data/20170210 all data/")[
  grepl("_4p_",list.files("~/Dropbox/SSEL/hotelling_marketups/hotellingMarkupsAnalysis/raw_data/20170210 all data/"))
  ]){
  sesDat4p = bind_rows(
    sesDat4p,
    read_excel(paste("~/Dropbox/SSEL/hotelling_marketups/hotellingMarkupsAnalysis/raw_data/20170210 all data/",sessFile,sep=""))
  )
}

sesDat4p = sesDat4p %>%
  group_by(session.code, player.period_number, participant.code) %>%
  mutate(
    # subperiods_num = length(player.round_payoff),
    player.round_payoff = player.round_payoff/length(player.round_payoff)
  ) %>%
  filter(!is.na(player.round_payoff))


# payment -------
SsPay <- read_excel("~/Dropbox/SSEL/hotelling_marketups/hotellingMarkupsAnalysis/raw_data/20170210 all data/hotellingmarkup_payment_20 (accessed 2017-02-20).xlsx")
SsPay = SsPay %>%
  dplyr::filter(
    participant.code %in% unique(c(sesDat4p$participant.code, sesDat2p$participant.code))
  )


## Utils ------
se <- function(x){
  mean(x) / length(x)^0.5
}

