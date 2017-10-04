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

#eliminate duplicates (this did not work)
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

#Replace NAs in location with unknown
alldata_clean$location[is.na(alldata_clean$location)] <- "unknown"

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

#replace NAs for Education, Behavior in program, Reason for withdrawal, referred by, and Salary
alldata$Education[is.na(alldata$Education)] <- "Unknown"
alldata$Salary[is.na(alldata$Salary)] <- "Unknown"
alldata$`Behavior in program`[is.na(alldata$`Behavior in program`)] <- "acceptable"
alldata$`Reason for withdrawal`[is.na(alldata$`Reason for withdrawal`)] <- "Did not withdraw"
alldata$referred_by[is.na(alldata$referred_by)] <- "No response"

#fill in race
alldata$`Race/Ethnicity`[is.na(alldata$`Race/Ethnicity`)] <- "White"

#fill in payment plan, quiz, logic score, and interviewer with 'unknown'
alldata$int_logic_score[is.na(alldata$int_logic_score)] <- "Unknown"
alldata$quiz[is.na(alldata$quiz)] <- "Unknown"
alldata$interviewer[is.na(alldata$interviewer)] <- "Unknown"
alldata$payment_plan[is.na(alldata$payment_plan)] <- "Unknown"

#change zeros and 1-3s in quiz to TC for Try Coding
alldata$quiz = gsub("0", "TC", alldata$quiz)
alldata_clean$quiz = gsub("1:3", "TC", alldata_clean$quiz)

#remove addresses in location to anonymize - didn't work, using excel
within(alldata_clean$location, rm(0:9))

#collapse payment plans into larger groups
alldata_clean$payment_plan = gsub("Custom Payment Plan - .*", "Custom Payment Plan", alldata_clean$payment_plan)

#change quantifiable columns to factor or integer
alldata_clean <- read_csv("~/alldata_clean.csv", 
                          +     col_types = cols(Gender = col_factor(levels = c("m","f")), `Race/Ethnicity` = col_factor(levels = c("White", "Af-Am", "Asian-Am", "Indian", "Latinx", "MENA")), 
                                                 int_logic_score = col_integer(), 
                                                 program = col_factor(levels = c("Front End Engineering", "Back End Engineering")), 
                                                 quiz = col_integer(), 
                                                 status = col_factor(levels = c("expelled", "dropped_out", "graduated", "failed", "left_for_job", "enrolled"))))

#make new column for male/female binary
alldata_safe$GenderBinary <- 0
alldata_safe$GenderBinary[alldata_safe$Gender == "m"] <- 1

#make new numeric columns for positive/negative outcomes
alldata_safe$graduated <- 0
alldata_safe$graduated[alldata_safe$status == "graduated"] <- 1

#begin logistic regression
grad.rate <- glm(graduated~age+GenderBinary, data=alldata_safe, family = "binomial")
coef(summary(grad.rate))
Estimate Std. Error    z value     Pr(>|z|)
(Intercept)   2.02509935  0.5867000  3.4516777 0.0005571127
age          -0.06459817  0.0189974 -3.4003696 0.0006729482
GenderBinary  0.10238508  0.1810412  0.5655347 0.5717101456

#new results from updated data
grad.rate <- glm(graduated~age+GenderBinary+quiz+program, data=alldata_safe1, family = "binomial")
> coef(summary(grad.rate))
Estimate Std. Error    z value    Pr(>|z|)
(Intercept)                 -1.17574319 1.09974519 -1.0691051 0.285022318
age                         -0.02980253 0.02286065 -1.3036605 0.192349354
GenderBinary                 0.07802921 0.23613223  0.3304471 0.741062152
quiz                         0.33136250 0.10715650  3.0923227 0.001985968
programBack End Engineering  0.60009560 0.26856045  2.2344899 0.025450858

#transform coefficients to make them easier to interpret
grad.rate.tab <- coef(summary(grad.rate))
grad.rate.tab[, "Estimate"] <- exp(coef(grad.rate))
grad.rate.tab

Estimate Std. Error    z value    Pr(>|z|)
(Intercept)                 0.3085896 1.09974519 -1.0691051 0.285022318
age                         0.9706372 0.02286065 -1.3036605 0.192349354
GenderBinary                1.0811542 0.23613223  0.3304471 0.741062152
quiz                        1.3928646 0.10715650  3.0923227 0.001985968
programBack End Engineering 1.8222930 0.26856045  2.2344899 0.025450858

#How much more likely is a person with a quiz score of 8 to graduate than a person with 7,6, or 5? maybe this is l
grad1 <- glm(graduated~quiz+age,
               data=alldata_safe1, family="binomial")
coef(summary(grad1))

predData1 <- with(alldata_safe1,
                 expand.grid(age = mean(age, na.rm = TRUE),
                              GenderBinary = c(0,1),
                              quiz = c(4, 5, 6, 7, 8), 
                              program = c("Back End Engineering", "Front End Engineering")))

# predict likelihood of graduating at those levels
cbind(predData1, predict(grad1, type = "response",
                        se.fit = TRUE, interval="confidence",
                        newdata = predData1))
age GenderBinary quiz               program       fit     se.fit residual.scale
1  29.07912            0    4  Back End Engineering 0.4334528 0.08416925              1
2  29.07912            1    4  Back End Engineering 0.4334528 0.08416925              1
3  29.07912            0    5  Back End Engineering 0.5233101 0.06113067              1
4  29.07912            1    5  Back End Engineering 0.5233101 0.06113067              1
5  29.07912            0    6  Back End Engineering 0.6116834 0.03733316              1
6  29.07912            1    6  Back End Engineering 0.6116834 0.03733316              1
7  29.07912            0    7  Back End Engineering 0.6932759 0.02273575              1
8  29.07912            1    7  Back End Engineering 0.6932759 0.02273575              1
9  29.07912            0    8  Back End Engineering 0.7643296 0.02562388              1
10 29.07912            1    8  Back End Engineering 0.7643296 0.02562388              1
11 29.07912            0    4 Front End Engineering 0.4334528 0.08416925              1
12 29.07912            1    4 Front End Engineering 0.4334528 0.08416925              1
13 29.07912            0    5 Front End Engineering 0.5233101 0.06113067              1
14 29.07912            1    5 Front End Engineering 0.5233101 0.06113067              1
15 29.07912            0    6 Front End Engineering 0.6116834 0.03733316              1
16 29.07912            1    6 Front End Engineering 0.6116834 0.03733316              1
17 29.07912            0    7 Front End Engineering 0.6932759 0.02273575              1
18 29.07912            1    7 Front End Engineering 0.6932759 0.02273575              1
19 29.07912            0    8 Front End Engineering 0.7643296 0.02562388              1
20 29.07912            1    8 Front End Engineering 0.7643296 0.02562388              1
#How much more likely is one to graduate based on gender?
grad.gen <- glm(graduated~age+GenderBinary,
             data=alldata_safe1, family="binomial")
# Create a dataset with predictors set at desired levels
predData2 <- with(alldata_safe1,
                expand.grid(age = mean(age, na.rm = TRUE),
                            GenderBinary = c(0,1)))

# predict success at those levels
cbind(predData2, predict(grad.gen, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predData2))

age GenderBinary       fit     se.fit residual.scale
1 29.07912            0 0.5908547 0.03652352              1
2 29.07912            1 0.6198881 0.02439629              1

#logistic regression, take 2
> logreg <- glm(graduated~age+quiz+GenderBinary, data=alldata_safe1, family="binomial")
> logreg

Call:  glm(formula = graduated ~ age + quiz + GenderBinary, family = "binomial", 
           data = alldata_safe1)

Coefficients:
  (Intercept)           age          quiz  GenderBinary  
-0.56431      -0.03785       0.34521       0.08725  

Degrees of Freedom: 436 Total (i.e. Null);  433 Residual
(157 observations deleted due to missingness)
Null Deviance:	    530.3 
Residual Deviance: 515.3 	AIC: 523.3
> summary(logreg)

Call:
  glm(formula = graduated ~ age + quiz + GenderBinary, family = "binomial", 
      data = alldata_safe1)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.8736  -1.3438   0.7298   0.8199   1.5866  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)   
(Intercept)  -0.56431    1.05869  -0.533  0.59402   
age          -0.03785    0.02255  -1.678  0.09326 . 
quiz          0.34521    0.10629   3.248  0.00116 **
  GenderBinary  0.08725    0.23477   0.372  0.71017   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 530.29  on 436  degrees of freedom
Residual deviance: 515.25  on 433  degrees of freedom
(157 observations deleted due to missingness)
AIC: 523.25

Number of Fisher Scoring iterations: 4

#test out some examples
> newdata <- data.frame(age=27, quiz=7, GenderBinary=0)
> predict(logreg, newdata, type = "response")
1 
0.6963988 
> newdata.male <- data.frame(age=27, quiz=7, GenderBinary=1)
> predict(logreg, newdata.male, type = "response")
1 
0.7145228 
> newdata.8F <- data.frame(age=27, quiz=8, GenderBinary=0)
> predict(logreg, newdata.8F, type = "response")
1 
0.7641234 
> newdata.8M <- data.frame(age=27, quiz=8, GenderBinary=1)
> predict(logreg, newdata.8M, type = "response")
1 
0.7794847 

#reduce to just age and quiz
agequiz.reg <-  glm(formula = graduated ~ age + quiz, family = "binomial", 
                    +                     data = alldata_safe1)
> agequiz.reg

Call:  glm(formula = graduated ~ age + quiz, family = "binomial", data = alldata_safe1)

Coefficients:
  (Intercept)          age         quiz  
-0.47241     -0.03924      0.34651  

Degrees of Freedom: 436 Total (i.e. Null);  434 Residual
(157 observations deleted due to missingness)
Null Deviance:	    530.3 
Residual Deviance: 515.4 	AIC: 521.4
> summary(agequiz.reg)

Call:
  glm(formula = graduated ~ age + quiz, family = "binomial", data = alldata_safe1)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.8686  -1.3296   0.7129   0.8283   1.5717  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)   
(Intercept) -0.47241    1.02913  -0.459   0.6462   
age         -0.03924    0.02225  -1.764   0.0777 . 
quiz         0.34651    0.10614   3.265   0.0011 **
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 530.29  on 436  degrees of freedom
Residual deviance: 515.39  on 434  degrees of freedom
(157 observations deleted due to missingness)
AIC: 521.39

Number of Fisher Scoring iterations: 4

#test a sample
> newdata.8old <- data.frame(age=50, quiz=8)
> predict(agequiz.reg, newdata.8old, type = "response")
1 
0.5835862 

#make new columns for each racial category
alldata_safe1$AfAm <- 0
alldata_safe1$AfAm[alldata_safe1$`Race/Ethnicity` == "Af-Am"] <- 1

alldata_safe1$Latinx <- 0
alldata_safe1$Latinx[alldata_safe1$`Race/Ethnicity` == "Latinx"] <- 1

alldata_safe1$MENA <- 0
alldata_safe1$MENA[alldata_safe1$`Race/Ethnicity` == "MENA"] <- 1

alldata_safe1$White <- 0
alldata_safe1$White[alldata_safe1$`Race/Ethnicity` == "White"] <- 1

alldata_safe1$Asian <- 0
alldata_safe1$Asian[alldata_safe1$`Race/Ethnicity` == "Asian-Am"] <- 1

alldata_safe1$Indian <- 0
alldata_safe1$Indian[alldata_safe1$`Race/Ethnicity` == "Indian"] <- 1

#make new columns for each program
alldata_safe1$BE <- 0
alldata_safe1$BE[alldata_safe1$program == "Back End Engineering"] <- 1

alldata_safe1$FE <- 0
alldata_safe1$FE[alldata_safe1$program == "Front End Engineering"] <- 1

#make binary veterans column

alldata_safe1$veterans <- 0
alldata_safe1$veterans[alldata_safe1$`vet status` == "Yes"] <- 1

#try glm with more variables

log.reg.all <- glm(graduated~age+quiz+GenderBinary+AfAm+Asian+Indian+Latinx+White+MENA+veterans+BE+FE+enrollments+int_logic_score, data=alldata_safe1, family="binomial")
Call:  glm(formula = graduated ~ age + quiz + GenderBinary + AfAm + 
             Asian + Indian + Latinx + White + MENA + veterans + BE + 
             FE + enrollments + int_logic_score, family = "binomial", 
           data = alldata_safe1)

Coefficients:
  (Intercept)              age             quiz     GenderBinary             AfAm            Asian           Indian           Latinx            White             MENA         veterans               BE               FE  
-8.59629         -0.02051          0.40771          0.20075          0.12910          0.30368          0.53739          1.31899          0.97888               NA         -0.21321         -0.43124               NA  
enrollments  int_logic_score  
1.32030          0.16522  

Degrees of Freedom: 436 Total (i.e. Null);  424 Residual
(157 observations deleted due to missingness)
Null Deviance:	    530.3 
Residual Deviance: 341.1 	AIC: 367.1

summary(log.reg.all)
Call:
  glm(formula = graduated ~ age + quiz + GenderBinary + AfAm + 
        Asian + Indian + Latinx + White + MENA + veterans + BE + 
        FE + enrollments + int_logic_score, family = "binomial", 
      data = alldata_safe1)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-3.1620  -0.3646   0.4394   0.5750   1.9511  

Coefficients: (2 not defined because of singularities)
Estimate Std. Error z value Pr(>|z|)    
(Intercept)     -8.59629    2.24708  -3.826  0.00013 ***
  age             -0.02051    0.02952  -0.695  0.48727    
quiz             0.40771    0.14342   2.843  0.00447 ** 
  GenderBinary     0.20075    0.31010   0.647  0.51738    
AfAm             0.12910    1.46380   0.088  0.92972    
Asian            0.30368    1.38576   0.219  0.82654    
Indian           0.53739    1.70989   0.314  0.75331    
Latinx           1.31899    1.48271   0.890  0.37369    
White            0.97888    1.31305   0.746  0.45597    
MENA                  NA         NA      NA       NA    
veterans        -0.21321    1.03200  -0.207  0.83633    
BE              -0.43124    0.38350  -1.124  0.26081    
FE                    NA         NA      NA       NA    
enrollments      1.32030    0.14001   9.430  < 2e-16 ***
  int_logic_score  0.16522    0.10418   1.586  0.11276    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 530.29  on 436  degrees of freedom
Residual deviance: 341.12  on 424  degrees of freedom
(157 observations deleted due to missingness)
AIC: 367.12

Number of Fisher Scoring iterations: 5

#test predictions
new.AfAm <- data.frame(age=27, AfAm=1, GenderBinary=1, quiz=7, veterans=0, BE=1, int_logic_score=13, enrollments=c(1:7))
> predict(log.reg.afam, new.AfAm, type = "response")
1          2          3          4          5          6          7 
0.05385855 0.17204959 0.43135862 0.73469010 0.90998162 0.97361630 0.99263140 
> new <- data.frame(age=27, AfAm=1, GenderBinary=1, quiz=7, veterans=0, BE=1, int_logic_score=13, enrollments=c(1:7))

alldata_safe1$repeatbin <- 1
alldata_safe1$repeatbin[alldata_safe1$repeats == "0"] <- 0
