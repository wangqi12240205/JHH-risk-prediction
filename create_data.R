# build matrix for data
# 7 March 2020

library(reshape2)
library(TTR)


create_data_frame <- function() {
  For_Eval <- data.frame()
  
  Corr <- 0.5
  Max_sample <- 3
  R_sample <- integer(48)
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
    }
    For_Eval_i <- data.frame(
      month_cum = c(1:48),
      Infection_Count = R_sample,
      UnitName =  Units[i],
      HHCom = runif(48,min = 0.8,max = 0.95),
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
ma_infect = moving_average(For_Evaluation$Infection_Count,3)
plot(ma_infect,type="l",xlab = 'Index',  ylab = 'infection count',
     main = 'moving average of infection count')

