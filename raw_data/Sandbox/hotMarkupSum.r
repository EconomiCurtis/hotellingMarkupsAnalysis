# libraries
library(readr)
library(dplyr)
library(ggplot2)

# load data
hotMarkup_2p_01 <- read_csv("~/Dropbox/SSEL/hotelling_marketups/hotellingMarkupsAnalysis/raw_data/20170209 2p 01 hotellingmarkup stacked.csv")
hotMarkup_4p_01 <- read_csv("~/Dropbox/SSEL/hotelling_marketups/hotellingMarkupsAnalysis/raw_data/20170209 4p 01 hotellingmarkup stacked.csv")


# 2 player game

names(hotMarkup_2p_01)
unique(hotMarkup_2p_01$session.code)
hotMarkup_2p_01 %>%
  group_by(session.code,player.period_number,player.transport_cost) %>%
  summarise(
    mean_profit = mean(player.round_payoff * 100)
  ) %>%
  View


ggplot(
  hotMarkup_2p_01 %>%
  group_by(session.code,player.transport_cost,player.transport_cost) %>%
  summarise(
    mean_profit = mean(player.round_payoff * 100)
  ) %>%
    filter(
      !is.na(mean_profit)
    )
) +
  geom_point(
    aes(
      x = player.transport_cost,
      y = mean_profit,
      color = paste(session.code,player.transport_cost)
  )
  )





# 4 player game
names(hotMarkup_4p_01)
unique(hotMarkup_4p_01$session.code)
hotMarkup_4p_01 %>%
  group_by(session.code,player.period_number,player.transport_cost) %>%
  summarise(
    mean_profit = mean(player.round_payoff * 100)
  ) %>%
  View

hotMarkup_4p_01 %>%
  group_by(session.code,player.period_number,player.transport_cost) %>%
  filter(subsession.round_number == 20) %>%
  summarise(
    mean_profit = mean(player.round_payoff * 100)
  ) %>%
  View

ggplot(
  hotMarkup_4p_01 %>%
  group_by(session.code,player.transport_cost,player.period_number) %>%
  summarise(
    mean_profit = mean(player.round_payoff * 100)
  ) %>%
    filter(
      !is.na(mean_profit)
    )
) +
  geom_point(
    aes(
      x = player.transport_cost,
      y = mean_profit,
      color = player.period_number)
  )
