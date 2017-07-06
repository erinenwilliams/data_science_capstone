# standardize locations
all_loc <- All_Students %>%
  mutate(location = tolower(location)) %>% 
  mutate(location = case_when(
    .$location %in% c('denver', 'denver, colorado', 'colorado', 'co', 'denver colorado', 'denver area', 'denver tech center') ~ 'denver, co',
    .$location %in% c('nyc', 'new york city') ~ 'new york, ny',
    TRUE ~ .$location))

#merge data sets
dropouts <- left_join(dropouts_May_2017, Drop_out_analysis2, by = "id")
all_drop <- left_join(all_loc, dropouts, by = "id")
all_demo <- left_join(all_drop, demographics, by = "id")


#alphabetize column names
alpha_col <- all_demo %>% select(noquote(order(colnames(all_demo))))

#install tidyr

#eliminate duplicates
transmute(coalesce(alpha_col$enrollments.x, alpha_col$enrollments.y))
alpha1 <- Coalesce(alpha_col$Age.x, alpha_col$Age.y)

#use excel to eliminate extra columns
#upload new document
library(readr)
alphacol2 <- read_csv("~/Documents/alphacol2.csv")

#Replace NAs with 0 in Mod dropped out column
alphacol2$`Mod dropped out`[is.na(alphacol2$`Mod dropped out`)] <- 0

#Calculate mean of Age
27

#Replace NAs with mean in Age column
alphacol2$Age[is.na(alphacol2$Age)] <- 27

#Replace NAs in Experience with "some organized learning"
alphacol2$Experience[is.na(alphacol2$Experience)] <- "Some organized learning"

#Replace NAs in Job with Unknown
alphacol2$Job[is.na(alphacol2$Job)] <- "Unknown"

#Replace NAs in Vet status with No
alphacol2$`Vet status`[is.na(alphacol2$`Vet status`)] <- "No"

#Isolate app id in application column, then delete extra columns
alphacol3 <- separate(alphacol2, 'application', into = c("url1", "url2", "url3", "url4", "url5", "url6", "app_id"))
within(alphacol3, rm("url1", "url2", "url3", "url4", "url5", "url6"))

#merge new dataset with gender added
alpha6 <- left_join(alphacol5, alphacol4, by = "id")
alpha7 <- alpha6 %>% select(noquote(order(colnames(alpha6))))

#import new datasets
logic_interviewer <- read_csv("~/Downloads/logic_interviewer.csv")
quiz_scores <- read_csv("~/Downloads/quiz_scores.csv")

#merge new datasets with original
logic <- left_join(quiz_scores, logic_interviewer, by = "app_id")
logic1 <- drop_na(logic)
alldata <- left_join(alpha8, logic1, by = "app_id")

#download new doc
write_csv(alpha7, "alpha7.csv")

#remove duplicate columns
within(alldata, rm("student.x", "student.y", "Logic score.x", "Interviewer.x"))
