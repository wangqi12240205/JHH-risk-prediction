# build matrix for data
# 7 March 2020

library(reshape2)
library(TTR)
library(dplyr)
library(DataCombine)



create_data_frame <- function() {
  For_Eval <- data.frame()
  
  Corr <- 0.5
  Max_sample <- 3
  R_sample <- integer(48)
  Hocount_sample <- integer(48)
  LengthOfStay  <- integer(48)
  AvgCleaning <-  integer(48)
  Units <- paste(letters[1:12])
  
  writeLines('\nUnits:')
  cat(Units)
  writeLines('\n')
  
  for (i in c(1:12)) {
    for (j in c(1:48)) {
      R_sample_j = sample(Max_sample,1) - 1
      if (j==1) {
        R_sample[j] = R_sample_j
      } else {
        R_sample[j] = round(R_sample[j-1] * Corr + R_sample_j * (1-Corr))
      }
      Hocount_sample[j] = sample(R_sample[j] + 1, 1) - 1
      LengthOfStay[j] = sample(10,1) - 1
      AvgCleaning[j] = sample(5,1) - 1
    }
    Hocount = R_sample
    For_Eval_i <- data.frame(
      month_cum = c(1:48),
      Infection_Count = R_sample,
      HoCount = Hocount_sample,
      NotHoCount = R_sample - Hocount_sample,
      UnitName =  Units[i],
      HHCom = runif(48,min = 0.8,max = 0.95),
      LengthOfStay = LengthOfStay,
      AvgCleaning = AvgCleaning,
      other =  "other",
      stringsAsFactors = FALSE
    )
    For_Eval <- rbind(For_Eval,For_Eval_i)
  }
  print(head(For_Eval))
  # Last line is returned
  For_Eval
}

For_Evaluation <- create_data_frame()
moving_average <- function(array,k){
  temp <- runMean(array,n = k)
  for (i in (1:k-1)){
    temp[i] = mean(array[1:i]) 
  }
  return (temp)
}

by_month <- For_Evaluation %>% group_by(month_cum)
by_month_mean <- by_month %>% summarise(
  Infection_Count = mean(Infection_Count),
  HHCom = mean(HHCom)
)
by_month_mean <- as.data.frame(by_month_mean)


ma_infect = moving_average(by_month_mean$Infection_Count,3)
plot(by_month_mean$month_cum, ma_infect,type="l",xlab = 'Month',  ylab = 'infection count',
     main = 'moving average of infection count')


For_Evaluation$Y = For_Evaluation$HoCount > 0
For_Evaluation <- slide(For_Evaluation, Var = "Y", NewVar = "Y_Lag1", GroupVar = 'UnitName', slideBy = -1)  # create lag1 variable
For_Evaluation <- slide(For_Evaluation, Var = "NotHoCount", NewVar = "NotHoCount_Lag1", GroupVar = 'UnitName', slideBy = -1)  # create lag1 variable
For_Evaluation <- slide(For_Evaluation, Var = "LengthOfStay", NewVar = "LengthOfStay_Lag1", GroupVar = 'UnitName', slideBy = -1)  # create lag1 variable
For_Evaluation <- slide(For_Evaluation, Var = "HHCom", NewVar = "HHCom_Lag1", GroupVar = 'UnitName', slideBy = -1)  # create lag1 variable
For_Evaluation <- slide(For_Evaluation, Var = "AvgCleaning", NewVar = "AvgCleaning_Lag1", GroupVar = 'UnitName', slideBy = -1)  # create lag1 variable

fit <- glm(Y ~ Y_Lag1 + NotHoCount_Lag1 + LengthOfStay_Lag1 + HHCom_Lag1 + AvgCleaning_Lag1, 
           data=For_Evaluation,
           , family=binomial(link="logit"))

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)