#create logistic regression for large set of variables

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

#make new glm calls just to view binary racial categories
log.reg.afam <- glm(graduated~age+quiz+GenderBinary+AfAm+veterans+BE+int_logic_score+enrollments, data=alldata_safe1, family="binomial")
log.reg.afam

Call:  glm(formula = graduated ~ age + quiz + GenderBinary + AfAm + 
             veterans + BE + int_logic_score + enrollments, family = "binomial", 
           data = alldata_safe1)

Coefficients:
  (Intercept)              age             quiz     GenderBinary             AfAm         veterans               BE  int_logic_score      enrollments  
-7.50886         -0.02156          0.41161          0.20997         -0.78872         -0.37397         -0.48328          0.16237          1.29486  

Degrees of Freedom: 436 Total (i.e. Null);  428 Residual
(157 observations deleted due to missingness)
Null Deviance:	    530.3 
Residual Deviance: 343.6 	AIC: 361.6
> summary(log.reg.afam)

Call:
  glm(formula = graduated ~ age + quiz + GenderBinary + AfAm + 
        veterans + BE + int_logic_score + enrollments, family = "binomial", 
      data = alldata_safe1)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-3.1339  -0.3693   0.4543   0.5735   1.9799  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)     -7.50886    1.81535  -4.136 3.53e-05 ***
  age             -0.02156    0.02923  -0.738  0.46080    
quiz             0.41161    0.14204   2.898  0.00376 ** 
  GenderBinary     0.20997    0.30825   0.681  0.49576    
AfAm            -0.78872    0.70932  -1.112  0.26616    
veterans        -0.37397    1.02053  -0.366  0.71403    
BE              -0.48328    0.38071  -1.269  0.20430    
int_logic_score  0.16237    0.10298   1.577  0.11489    
enrollments      1.29486    0.13715   9.441  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 530.29  on 436  degrees of freedom
Residual deviance: 343.64  on 428  degrees of freedom
(157 observations deleted due to missingness)
AIC: 361.64

Number of Fisher Scoring iterations: 5

> log.reg.latinx <- glm(graduated~age+quiz+GenderBinary+Latinx+veterans+BE+int_logic_score+enrollments, data=alldata_safe1, family="binomial")
> log.reg.latinx

Call:  glm(formula = graduated ~ age + quiz + GenderBinary + Latinx + 
             veterans + BE + int_logic_score + enrollments, family = "binomial", 
           data = alldata_safe1)

Coefficients:
  (Intercept)              age             quiz     GenderBinary           Latinx         veterans               BE  int_logic_score      enrollments  
-7.85588         -0.01893          0.42863          0.24604          0.44897         -0.45901         -0.47912          0.16490          1.30724  

Degrees of Freedom: 436 Total (i.e. Null);  428 Residual
(157 observations deleted due to missingness)
Null Deviance:	    530.3 
Residual Deviance: 344.4 	AIC: 362.4
> summary(log.reg.latinx)

Call:
  glm(formula = graduated ~ age + quiz + GenderBinary + Latinx + 
        veterans + BE + int_logic_score + enrollments, family = "binomial", 
      data = alldata_safe1)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-3.3759  -0.3590   0.4581   0.5774   1.9224  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)     -7.85588    1.83463  -4.282 1.85e-05 ***
  age             -0.01893    0.02916  -0.649  0.51623    
quiz             0.42863    0.14213   3.016  0.00256 ** 
  GenderBinary     0.24604    0.30652   0.803  0.42215    
Latinx           0.44897    0.71280   0.630  0.52878    
veterans        -0.45901    0.98133  -0.468  0.63997    
BE              -0.47912    0.37729  -1.270  0.20411    
int_logic_score  0.16490    0.10307   1.600  0.10961    
enrollments      1.30724    0.13831   9.452  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 530.29  on 436  degrees of freedom
Residual deviance: 344.45  on 428  degrees of freedom
(157 observations deleted due to missingness)
AIC: 362.45

Number of Fisher Scoring iterations: 5

> log.reg.white <- glm(graduated~age+quiz+GenderBinary+White+veterans+BE+int_logic_score+enrollments, data=alldata_safe1, family="binomial")
> log.reg.white

Call:  glm(formula = graduated ~ age + quiz + GenderBinary + White + 
             veterans + BE + int_logic_score + enrollments, family = "binomial", 
           data = alldata_safe1)

Coefficients:
  (Intercept)              age             quiz     GenderBinary            White         veterans               BE  int_logic_score      enrollments  
-7.9440          -0.0219           0.4028           0.2002           0.4713          -0.3806          -0.4447           0.1636           1.3098  

Degrees of Freedom: 436 Total (i.e. Null);  428 Residual
(157 observations deleted due to missingness)
Null Deviance:	    530.3 
Residual Deviance: 343.2 	AIC: 361.2
> summary(log.reg.white)

Call:
  glm(formula = graduated ~ age + quiz + GenderBinary + White + 
        veterans + BE + int_logic_score + enrollments, family = "binomial", 
      data = alldata_safe1)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-3.2680  -0.3566   0.4492   0.5757   1.8671  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)     -7.94399    1.83012  -4.341 1.42e-05 ***
  age             -0.02190    0.02931  -0.747  0.45509    
quiz             0.40282    0.14299   2.817  0.00485 ** 
  GenderBinary     0.20024    0.30797   0.650  0.51558    
White            0.47126    0.35764   1.318  0.18761    
veterans        -0.38063    0.99086  -0.384  0.70088    
BE              -0.44465    0.38311  -1.161  0.24578    
int_logic_score  0.16355    0.10257   1.595  0.11080    
enrollments      1.30982    0.13857   9.453  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 530.29  on 436  degrees of freedom
Residual deviance: 343.17  on 428  degrees of freedom
(157 observations deleted due to missingness)
AIC: 361.17

Number of Fisher Scoring iterations: 5

new.MoC.quiz <- data.frame(age=27, White=0, GenderBinary=1, quiz=c(4:8), veterans=0, BE=1, int_logic_score=13, enrollments=4)
new.WoC.quiz <- data.frame(age=27, White=0, GenderBinary=0, quiz=c(4:8), veterans=0, BE=1, int_logic_score=13, enrollments=4)

#testing different combinations of gender and race with quiz scores
new.MoC.quiz <- data.frame(age=27, White=0, GenderBinary=1, quiz=c(4:8), veterans=0, BE=1, int_logic_score=13, enrollments=4)
> predict(log.reg.white, new.MoC.quiz, type = "response")
1         2         3         4         5 
0.5491322 0.6456524 0.7316088 0.8030739 0.8591727 
> new.WoC.quiz <- data.frame(age=27, White=0, GenderBinary=0, quiz=c(4:8), veterans=0, BE=1, int_logic_score=13, enrollments=4)
> predict(log.reg.white, new.WoC.quiz, type = "response")
1         2         3         4         5 
0.4992324 0.5986270 0.6905229 0.7694810 0.8331614 
> new.ytm.quiz <- data.frame(age=27, White=1, GenderBinary=1, quiz=c(4:8), veterans=0, BE=1, int_logic_score=13, enrollments=4)
> predict(log.reg.white, new.ytm.quiz, type = "response")
1         2         3         4         5 
0.6611506 0.7448334 0.8136739 0.8672522 0.9071815 
> new.ytw.quiz <- data.frame(age=27, White=1, GenderBinary=0, quiz=c(4:8), veterans=0, BE=1, int_logic_score=13, enrollments=4)
> predict(log.reg.white, new.ytw.quiz, type = "response")
1         2         3         4         5 
0.6149554 0.7049554 0.7813970 0.8424596 0.8888908 
> 
  log.reg.race <- glm(graduated~quiz+GenderBinary+White+veterans+BE, data=alldata_safe1, family="binomial")

#removed extra variables but increased AIC

> WoC.quiz.sim <- data.frame(White=0, GenderBinary=0, quiz=c(4:8), veterans=0, BE=1)
          > predict(log.reg.race, WoC.quiz.sim, type = "response")
          1         2         3         4         5 
          0.4353778 0.5169486 0.5976261 0.6733436 0.7409880 
          
          > ytw.quiz.sim <- data.frame(White=1, GenderBinary=0, quiz=c(4:8), veterans=0, BE=1)
          > predict(log.reg.race, ytw.quiz.sim, type = "response")
          1         2         3         4         5 
          0.4839973 0.5655531 0.6437078 0.7148907 0.7767836 
          
          > MoC.quiz.sim <- data.frame(White=0, GenderBinary=1, quiz=c(4:8), veterans=0, BE=1)
          > predict(log.reg.race, MoC.quiz.sim, type = "response")
          1         2         3         4         5 
          0.4665656 0.5483053 0.6275193 0.7004313 0.7644288 
          
          > ytm.quiz.sim <- data.frame(White=1, GenderBinary=1, quiz=c(4:8), veterans=0, BE=1)
          > predict(log.reg.race, ytm.quiz.sim, type = "response")
          1         2         3         4         5 
          0.5154882 0.5962191 0.6720561 0.7398641 0.7978685 
          
          
#add column to conduct log reg on repeats
          
          alldata_safe1$repeatbin <- 1
          alldata_safe1$repeatbin[alldata_safe1$repeats == "0"] <- 0
          
#logreg on repeats
          > logreg.repeats <- glm(formula = repeatbin ~ age + quiz + GenderBinary + White + 
                                    +                           veterans + BE + int_logic_score, family = "binomial", 
                                  +                       data = alldata_safe1)
          > logreg.repeats
          
          Call:  glm(formula = repeatbin ~ age + quiz + GenderBinary + White + 
                       veterans + BE + int_logic_score, family = "binomial", data = alldata_safe1)
          
          Coefficients:
            (Intercept)              age             quiz     GenderBinary            White         veterans               BE  int_logic_score  
          -1.08981          0.05289         -0.27543          0.24711         -0.75984         -0.69285          1.01557         -0.04254  
          
          Degrees of Freedom: 436 Total (i.e. Null);  429 Residual
          (157 observations deleted due to missingness)
          Null Deviance:	    400.7 
          Residual Deviance: 379.9 	AIC: 395.9
          > summary(logreg.repeats)
          
          Call:
            glm(formula = repeatbin ~ age + quiz + GenderBinary + White + 
                  veterans + BE + int_logic_score, family = "binomial", data = alldata_safe1)
          
          Deviance Residuals: 
            Min       1Q   Median       3Q      Max  
          -1.1489  -0.6219  -0.5347  -0.3830   2.3462  
          
          Coefficients:
            Estimate Std. Error z value Pr(>|z|)  
          (Intercept)     -1.08981    1.54859  -0.704   0.4816  
          age              0.05289    0.02740   1.931   0.0535 .
          quiz            -0.27543    0.13077  -2.106   0.0352 *
            GenderBinary     0.24711    0.30073   0.822   0.4113  
          White           -0.75984    0.30071  -2.527   0.0115 *
            veterans        -0.69285    1.13143  -0.612   0.5403  
          BE               1.01557    0.44205   2.297   0.0216 *
            int_logic_score -0.04254    0.08563  -0.497   0.6193  
          ---
            Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
          
          (Dispersion parameter for binomial family taken to be 1)
          
          Null deviance: 400.69  on 436  degrees of freedom
          Residual deviance: 379.94  on 429  degrees of freedom
          (157 observations deleted due to missingness)
          AIC: 395.94
          
          Number of Fisher Scoring iterations: 5
          
   #build training and test sets
          > table(alldata_safe1$graduated)
          
          0   1 
          232 362 
         
          > 362+232
          [1] 594
          > 362/594
          [1] 0.6094276
          > library("caTools", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
          > set.seed(51)
          > split <- sample.split(alldata_safe1$graduated, SplitRatio = 0.61)
          > split
          [1] FALSE  TRUE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE
          [37]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
          [73] FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE
          [109] FALSE FALSE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE
          [145] FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
          [181] FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE
          [217]  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE
          [253] FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE
          [289] FALSE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
          [325]  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
          [361]  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
          [397] FALSE  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
          [433] FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE
          [469]  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE
          [505]  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE
          [541]  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE FALSE
          [577] FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE
          > dataTrain = subset(alldata_safe1, split == TRUE)
          > dataTest = subset(alldata_safe1, split == FALSE)
          > nrow(dataTrain)
          [1] 363
          > nrow(dataTest)
          [1] 231
          > #run logistic regression on training set
                                       > TrainLog <- glm(formula = graduated ~ age + quiz + GenderBinary + White + veterans + BE + int_logic_score + enrollments, family = "binomial", data = dataTrain) 
                                       > summary(TrainLog)
                                       
                                       Call:
                                         glm(formula = graduated ~ age + quiz + GenderBinary + White + 
                                               veterans + BE + int_logic_score + enrollments, family = "binomial", 
                                             data = dataTrain)
                                       
                                       Deviance Residuals: 
                                         Min       1Q   Median       3Q      Max  
                                       -3.2717  -0.3686   0.4581   0.5708   1.9461  
                                       
                                       Coefficients:
                                         Estimate Std. Error z value Pr(>|z|)    
                                       (Intercept)     -5.803227   2.158090  -2.689  0.00717 ** 
                                         age             -0.056587   0.036457  -1.552  0.12062    
                                       quiz             0.466038   0.190429   2.447  0.01439 *  
                                         GenderBinary     0.188163   0.384596   0.489  0.62466    
                                       White            0.362003   0.427645   0.847  0.39727    
                                       veterans         1.023666   1.206281   0.849  0.39610    
                                       BE               0.015956   0.470741   0.034  0.97296    
                                       int_logic_score -0.003953   0.117845  -0.034  0.97324    
                                       enrollments      1.323335   0.181674   7.284 3.24e-13 ***
                                         ---
                                         Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                       
                                       (Dispersion parameter for binomial family taken to be 1)
                                       
                                       Null deviance: 330.58  on 270  degrees of freedom
                                       Residual deviance: 216.62  on 262  degrees of freedom
                                       (92 observations deleted due to missingness)
                                       AIC: 234.62
                                       
                                       Number of Fisher Scoring iterations: 5
                                       
                                       > predictTrain <- predict(TrainLog, type = "response")
                                       > summary(predictTrain)
                                       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
                                       0.01233 0.68720 0.85010 0.70110 0.89360 0.99790 
                                      #create predicted set using TrainLog 
                                       predicted <- predict(TrainLog, dataTest, type = "response")
                                      #check for multicollinearity
                                       vif(TrainLog)
                                       age            quiz    GenderBinary           White        veterans              BE int_logic_score     enrollments 
                                       1.076311        1.099675        1.073506        1.016048        1.087558        1.086935        1.143637        1.169370 
                                       > 
                                      #calculate misclassification errors
                                      misClassError(dataTest$graduated, predicted, threshold = 0.5)
                                       [1] 0.1212  
                                      #ROC
                                      > plotROC(dataTest$graduated, predicted) # 0.4493
                                      > Concordance(dataTest$graduated, predicted)
                                      $Concordance
                                      [1] 0.8379237
                                      
                                      $Discordance
                                      [1] 0.1620763
                                      
                                      $Tied
                                      [1] -5.551115e-17
                                      
                                      $Pairs
                                      [1] 5664
                                      #plot confusion matrix - best threshold for lowest errors
                                      > confusionMatrix(dataTest$graduated, predicted, threshold = 0.5)
                                      0   1
                                      0 32  12
                                      1 16 106
                                      > 
