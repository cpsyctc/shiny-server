library(tidyverse)
library(CECPfuns)
library(janitor)


vecNsessions <- c(1, 1,
                  rep(2, 5),
                  rep(3, 8),
                  rep(4, 10),
                  rep(5, 11),
                  rep(6, 12),
                  rep(7, 7),
                  rep(8, 5),
                  rep(9, 3),
                  rep(10,2),
                  11, 12, 14, 17, 21, 24)

vecNDNA <- c(rep(0, 15),
             rep(1, 5),
             rep(2, 3),
             rep(3, 2))

vecNcancelled <- c(rep(0, 15),
                   rep(1, 5),
                   2)

vecNlate <- c(rep(0, 15),
             rep(1, 5),
             rep(2, 3),
             rep(3, 2), 
             4 : 28)

vecGender <- c(rep("F", 60),
               rep("M", 35),
               rep("O", 3),
               rep("", 2))

vecAges <- c(rep(18, 3),
             rep(19, 7),
             rep(20, 8),
             rep(24, 9),
             rep(25, 9),
             rep(26, 8),
             rep(27, 8),
             rep(28, 9),
             rep(29, 7),
             rep(30, 8),
             rep(31, 7),
             rep(32, 6),
             rep(33, 6),
             rep(34, 6),
             rep(35, 5),
             rep(36, 4),
             rep(37, 5),
             rep(38, 4),
             rep(39, 4),
             rep(40, 4),
             rep(41, 4),
             rep(42, 4),
             rep(43, 3),
             rep(44, 3),
             rep(45, 3),
             rep(46, 2),
             rep(47, 2),
             rep(48, 3),
             rep(49, 4),
             rep(51, 4),
             rep(52, 4),
             rep(53, 3),
             rep(54, 3),
             rep(55, 3),
             rep(56, 2),
             rep(57, 2),
             rep(58, 3),
             rep(59, 4),
             rep(60, 4),
             rep(61, 4),
             rep(62, 3),
             rep(63, 3),
             rep(64, 3),
             rep(65, 5),
             rep(66, 4),
             rep(67, 3),
             rep(68, 3),
             rep(69, 2),
             rep(70, 2), 
             71:95)

vecMeasures <- c(rep("CORE-OM (all items)", 10),
                 rep("CORE-OM (WB)", 2),
                 rep("CORE-OM (Problems)", 3), 
                 rep("CORE-OM (Functioning)", 3),
                 rep("CORE-OM (Risk)", 7),
                 rep("CORE-OM (Non-risk)", 8),
                 rep("CORE-10", 8),
                 rep("CORE-SF/A", 2),
                 rep("CORE-SF/B", 2),
                 rep("GP-CORE", 3),
                 rep("LD-CORE-14", 2),
                 rep("LD-CORE-30", 1))

read_csv(file = "/datadisk/Current_share/Current_Data/CORE/translations/SPA_other/Clara/Our_papers/MarBar_MLM_work_with_Jorge/data_forChris2024.csv") -> tmpQuitoDat
tmpQuitoDat %>%
  colnames()

tmpQuitoDat %>%
  filter(questionnaire %in% c("CORE10", "COREOM")) %>% 
  select(clientid, consultationid, sessionDate, questionnaire, itemid, responseScore) %>%
  arrange(clientid, consultationid, itemid) %>%
  group_by(clientid) %>%
  mutate(nSessions = n_distinct(consultationid)) %>% 
  ungroup() -> tmpQuitoDat2
  

tmpQuitoDat2 %>%
  group_by(clientid, consultationid) %>%
  filter(itemid == min(itemid)) %>%
  summarise(nSessions = first(nSessions)) %>%
  ungroup() %>%
  group_by(clientid) %>%
  mutate(sessionN = row_number()) %>%
  ungroup() -> tibQuitoSessions

tmpQuitoDat2 %>%
  left_join(tibQuitoSessions) %>%
  ### get just first and last scores
  filter(sessionN == 1 | sessionN == nSessions) %>%
  mutate(firstLast = if_else(sessionN == 1,
                             "first",
                             "last")) %>% 
  group_by(clientid, consultationid, questionnaire) %>%
  summarise(sessionN = first(sessionN),
            nSessions = first(nSessions),
            sessionDate = first(sessionDate),
            questionnaire = first(questionnaire),
            firstLast = first(firstLast),
            nItems = n(),
            nMissing = getNNA(responseScore),
            rawMeanScore = mean(responseScore),
            ### sort out prorating
            propMissing = nMissing / nItems,
            meanScore = if_else(propMissing < .1,
                                rawMeanScore,
                                NA_real_)) %>%
  ungroup() -> tmpTibQuito

tmpTibQuito %>%
  group_by(questionnaire, firstLast) %>%
  summarise(mean = mean(meanScore, na.rm = TRUE),
            median = median(meanScore, na.rm = TRUE)) -> tmpTibStats

ggplot(data = tmpTibQuito,
       aes(x = meanScore)) +
  facet_grid(rows = vars(firstLast), cols = vars(questionnaire)) +
  geom_histogram() +
  geom_vline(data = tmpTibStats,
             aes(xintercept = mean),
                 colour = "blue") +
  geom_vline(data = tmpTibStats,
             aes(xintercept = median),
             colour = "green") 
tmpTibQuito %>%
  filter(firstLast == "first") %>%
  filter(!is.na(meanScore)) %>%
  select(meanScore) %>%
  pull() -> vecFirstScores

tmpTibQuito %>%
  filter(firstLast == "last") %>%
  filter(!is.na(meanScore)) %>%
  select(meanScore) %>%
  pull() -> vecLastScores


firstDate <- as.Date("06/01/2020", format = "%d/%m/%Y")
baseDates <- 
  firstDate + days(1:4)

set.seed(12345)
vecIDs <- 14:300
n <- length(vecIDs)
tibble(RespondentID = vecIDs) %>%
  ### sort out scores, done per row
  rowwise() %>%
  mutate(mean1 = sample(vecFirstScores, 1, replace = TRUE),
         mean2 = sample(vecLastScores, 1, replace = TRUE),
         clin1 = round(mean1 * 10, 1),
         clin2 = round(mean2 * 10, 1),
         mean1 = round(mean1, 2),
         mean2 = round(mean2, 2)) %>%
  ungroup() %>%
  ### now sort out the various counts, not rowwise
  mutate(nSessionsAttended = sample(vecNsessions, n, replace = TRUE),
         nSessionsDNAed = sample(vecNDNA, n, replace = TRUE),
         nSessionsCancelled = sample(vecNcancelled, n, replace = TRUE),
         nSessionsLate = sample(vecNlate, n, replace = TRUE),
         Start_date = sample(baseDates, n, replace = TRUE) + weeks(sample(1:90, n, replace = TRUE)),
         nSessionsLate = if_else(nSessionsLate > nSessionsAttended,
                                 0,
                                 nSessionsLate),
         nWeeks = nSessionsAttended + nSessionsDNAed + nSessionsCancelled) %>%
  ### now sort out measures used
  mutate(measure1 = sample(vecMeasures, n, replace = TRUE),
         ### by default set measure2 = measure1
         measure2 = measure1) %>%
  rowwise() %>%
  mutate(measure2 = if_else(measure1 == "CORE-OM (all items)" & sample(0:1, 1, replace = TRUE),
                            "CORE-10",
                            measure2),
         measure2 = if_else(measure1 == "CORE-SF/A" & sample(0:1, 1, replace = TRUE) == 0,
                            "CORE-SF/B",
                            measure2),
         measure2 = if_else(measure1 == "CORE-SF/B" & sample(0:1, 1, replace = TRUE) == 0,
                            "CORE-SF/A",
                            measure2)) %>%
  ungroup() -> tmpTib



tmpTib %>%
  mutate(End_date = Start_date + weeks(nWeeks),
         # Start_date = format(Start_date, format = "%d/%m/%Y"),
         # End_date = format(End_date, format = "%d/%m/%Y"),
         ### now get therapists' IDs and client gender and age
         TherapistID = sample(2:5, n, replace =  TRUE),
         Gender = sample(vecGender, n, replace =  TRUE),
         Age = sample(vecAges, n, replace =  TRUE),
         Comment = "") %>%
  select(RespondentID, TherapistID, Gender, Age, measure1, mean1, clin1, measure2, mean2, clin2, Comment, Start_date, End_date,
         nSessionsAttended, nSessionsDNAed, nSessionsCancelled, nSessionsLate, nWeeks) %>% 
   write_csv(file = "/datadisk/Current_share/Current_Data/MyR/shiny.psyctc.org/docs/CORE_general/CORE_wide2.csv")

