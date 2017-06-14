# Counts table

install.packages("RSQLite")
install.packages("dplyr")
install.packages("RMySQL")
install.packages("gridExtra")
install.packages("reshape2")
install.packages("dbplyr")
library("gridExtra")
library("RMySQL")
library("ggplot2")
library("RSQLite")
library("dplyr")
library("reshape2")
library("dbplyr")

# *ab_id* links 'atbat' and 'pitches' ------------------ #

db <- src_mysql("Baseball", 
                create = FALSE, user = "root", password = "Koufax32")

atbat <- tbl(db, "atbats"); tbl_vars(atbat)
pitches <- tbl(db, "pitches"); tbl_vars(pitches) 

sw <- tbl(db, "pitches") %>% 
  filter(des %in% c("Foul", "Foul (Runner Going)",  "Foul Tip", "In play, no out", "In play, out(s)", "Swinging Strike", "Swinging Strike (Blocked)")) %>% 
  select(px, pz, des, ab_id, pitch_id, id, type); tbl_vars(sw)


atbats <- tbl(db, "atbats") %>% 
  select(ab_id, stand, batter, ball, strike, event) # tbl_vars(atbats)


pitches <- left_join(sw, atbats) %>% 
  collect(n = Inf) # REPRESENTATION becomes DATA; THE KEY STEP

# Verify that HRs are included in "In play, no out" ---------- #
comeonbaby <- select(pitches, des, event) %>% 
  filter(event == "Home Run")
table(comeonbaby)

# Missing-ness --------------------------- #
sum(is.na(pitches$pz))
pitches <- filter(pitches, px != "NA", strike != "NA", ball != "NA", strike != "", ball != "", ball != 4, strike != 3)

# Tabulate ball-strike counts ------------ #
table(pitches$strike, pitches$ball)

# Count totals for RH hitters --------------------------- #
rh_count <- pitches %>% filter(stand == "R") %>%
  mutate(hit = as.numeric(des == "In play, no out")) %>%
  filter(abs(px) < 4) %>% select(ball, strike, hit)

rhbycount <- group_by(rh_count, ball, strike) %>% 
  summarise(p = round(mean(hit), 4))

rhbycount <- spread(rhbycount, key = ball, value = p)

names(rhbycount) <- c("Strikes", "0 Balls", "1 Ball", "2 Balls", "3 Balls")

rhbycount

# Three ball counts ----------------- #
threeball <- pitches %>% 
  filter(ball == 3) %>% 
  mutate(hit = as.numeric(des == "In play, no out"))

# BA affecting three ball counts (ball in play) --------------- #
three_ball_batavg <- filter(
  threeball, des %in% c("In play, no out", "In play, out(s)")) %>% 
  mutate(hit = as.numeric(des == "In play, no out"))

group_by(three_ball_batavg, ball, strike) %>% 
  summarise(p = round(mean(hit), 3), n = n())

# write.csv(rh_count, file = "Data/rh_count.csv")

cba <- group_by(rh_count, ball, strike) %>% 
  summarise(p = round(mean(hit), 3), n = n())

library(tidyr)
# Count success probabilities table ------------------- #
csp <- as.data.frame(spread(select(cba, ball, strike, p), key = ball, value = p))
names(csp) <- c("Strikes", "0 Balls", "1 Ball", "2 Balls", "3 Balls")

ggplot(cba) + geom_line(aes(x = ball, y = p, color = strike, group = strike)) + geom_point(aes(x = ball, y = p, color = strike))
