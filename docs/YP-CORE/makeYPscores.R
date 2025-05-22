library(tidyverse)

n <- 120
set.seed(12345)
tibble(ID = 1:n,
       latent = rnorm(n, mean = 2),
       score1 = latent + rnorm(n, sd = .2)) %>%
  mutate(change = 0 - latent * .2,
         change = change + rnorm(n, sd = .1),
         score2 = score1 + change) %>%
  filter(score1 <= 4 & score1 > 0.7,
         score2 <= 4 & score2 > 0) -> tmpTib

tmpTib %>%
  pivot_longer(cols = starts_with("score"),
               names_to = "Score") %>%
  mutate(occasion = if_else(Score == "score1",
                            1,
                            2)) -> tmpTibLong

tmpOffset <- .03
tmpTibLong %>%
  group_by(occasion) %>%
  summarise(mean = mean(value)) %>%
  mutate(occasion = if_else(occasion == 1,
                            occasion - tmpOffset,
                            occasion + tmpOffset)) -> tmpTibMeans


ggplot(data = tmpTibMeans,
       aes(x = occasion, y = mean)) +
  geom_point(data = tmpTibMeans,
             aes(x = occasion,
                 y = mean),
             size = 4,
             colour = "blue") +
  geom_line(data = tmpTibMeans,
            aes(x = occasion,
                y = mean),
            linewidth = 2,
            colour = "blue") +
  geom_point(data = tmpTibLong,
             inherit.aes = FALSE,
             aes(x = occasion, y = value, group = ID)) +
  geom_line(data = tmpTibLong,
            inherit.aes = FALSE,
            aes(x = occasion, y = value, group = ID)) +
  xlab("Occasion") +
  ylab("YP-CORE score") +
  ylim(c(0, 4))

tmpTibLong %>%
  select(ID, occasion, value) %>%
  mutate(occasion = str_c("Session ", occasion),
         YPclin = 10 * value) %>%
  rename(RespondentID = ID,
         SessionN = occasion,
         YPmean = value) %>%
  write.csv(file = "YPlong.csv")

tmpTib %>%
  select(ID, score1, score2) %>%
  rename(RespondentID = ID,
         YPmean1 = score1,
         YPmean2 = score2) %>%
  mutate(YPclin1 = 10 * YPmean1,
         YPclin2 = 10 * YPmean2) %>%
  write.csv(file = "YPwide.csv")
