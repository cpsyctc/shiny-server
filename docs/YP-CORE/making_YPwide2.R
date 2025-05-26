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

vecNlate <- c(rep(NA, 15),
             rep(1, 5),
             rep(2, 3),
             rep(3, 2), 
             4 : 10)

vecGender <- c(rep("F", 60),
               rep("M", 35),
               rep("O", 3),
               rep("", 2))

vecAges <- c(rep(11, 7),
             rep(12, 18),
             rep(13, 22),
             rep(14, 27),
             rep(15, 25),
             rep(16, 24),
             rep(17, 20),
             rep(18, 5))

firstDate <- as.Date("06/01/2020", format = "%d/%m/%Y")
baseDates <- 
  firstDate + days(1:4)


read_csv("YPwide.csv") -> tmpTib
n <- nrow(tmpTib)
set.seed(12345)
tmpTib %>%
  mutate(YPmean1 = round(YPmean1, 2),
         YPmean2 = round(YPmean2, 2),
         YPclin1 = round(YPclin1, 1),
         YPclin2 = round(YPclin2, 1),
         nSessionsAttended = sample(vecNsessions, n, replace = TRUE),
         nSessionsDNAed = sample(vecNDNA, n, replace = TRUE),
         nSessionsCancelled = sample(vecNcancelled, n, replace = TRUE),
         nSessionsLate = sample(vecNlate, n, replace = TRUE),
         Start_date = sample(baseDates, n, replace = TRUE) + weeks(sample(1:90, n, replace = TRUE)),
         nSessionsLate = if_else(nSessionsLate > nSessionsAttended,
                                 0,
                                 nSessionsLate),
         nWeeks = nSessionsAttended + nSessionsDNAed + nSessionsCancelled) -> tmpTib

# tmpTib %>%
#   filter(nSessionsDNAed > nSessionsAttended)
# 
# tmpTib %>%
#   filter(nSessionsCancelled > nSessionsAttended)

### now fill in the missing data for rows 32 and upwards 
tmpTib %>% 
  filter(row_number() < 32) -> tmpTib1

tmpTib %>% 
  filter(row_number() >= 32) %>%
  mutate(End_date = Start_date + weeks(nWeeks),
         ### now get therapists' IDs and client gender and age
         TherapistID = sample(2:5, 84, replace =  TRUE),
         Gender = sample(vecGender, 84, replace =  TRUE),
         Age = sample(vecAges, 84, replace =  TRUE)) -> tmpTib2

bind_rows(tmpTib1,
          tmpTib2) %>%
  mutate(RespondentID = row_number()) %>%
  write_csv(file = "YPwide2.csv")

