# Analyze MRSA data
library(survival)
Tbl = read.csv(file = 'by_month.csv');

# 
# get_censored <- function(Y_k) {
#   # Censoring
#   #----------------------------------------------------------------------------------------------
#   # Censoring TEST
#   # Y_k = c(1,0,1,0,0,1,0,0,0,0);
#   # Y_k = c(0,0,1,0,0,1,0,0,0,1);
#   
#   #----------------------------------------------------------------------------------------------
#   censored = c();
#   for (i in 1:length(Y_k)) {
#     if (Y_k[i] == 0) {
#       if (Y_k[min(i+1,length(Y_k))]==0) {
#         censored = c(censored, 1)
#       } else {
#         censored = c(censored, 0)
#       }
#     }
#   }
#   return (censored)
# }


process_data <- function(X_k, Y_k) {
  censored = c();
  T_start = c();
  T_end   = c();
  t       = 0;
  if (Y_k[1] == 0) {
    T_start = c(T_start, 1)
  }
  
  for (i in 2:length(Y_k)) {
    if (Y_k[i] == 0) {
      if (Y_k[i-1] > 0) {
        T_start = c(T_start, i)
        T_end = c(T_end, i - 1)
        censored = c(censored, 1)
      }
    }
  }
  if (length(T_end) == length(T_start)) {
    T_end = T_end[-1]
  } else {
    if (Y_k[length(Y_k)] == 0) {
      censored = c(censored, 0)
    } else {
      censored = c(censored, 1)
    }
  }
  T_end = c(T_end, length(Y_k) + 1)
  
  T_diff = T_end - T_start
  Xs = c()
  for (j in 1:length(T_start)) {
    Xs_temp = X_k[T_start[j]:min(T_end[j],length(Y_k)),]
    if (is.matrix(Xs_temp)) {
      Xs = rbind(Xs, colMeans(Xs_temp))
    } else {
      Xs = rbind(Xs, Xs_temp)
    }
  }
  
  output = data.frame(T_start, T_end, T_diff, censored, Xs)
  rownames(output) <- c()
  
  return(output)
}

# Initialize and use unit numbers
unit_name = unique(Tbl$UnitName);
X_Tbl = cbind(Tbl$LengthOfStay,Tbl$AvgCleaning,Tbl$HHCom);
X_all = c(); T_all = c(); unit_all = c(); Y_all = c(); C_all = c();
n_units = length(unit_name);

# Run for each unit
res.cox_each <- vector(mode = "list", length = length(unit_name))
dp <- vector(mode = "list", length = length(unit_name))
dp_all = data.frame()
betas = c()
for (k in 1:length(unit_name)) {
  index = (Tbl$UnitName == unit_name[k]);
  Y_k = Tbl$HoCount[index];
  X_k = X_Tbl[index,];
  dp[[k]] = process_data(X_k, Y_k);
  dp_all = rbind(dp_all, dp[[k]]);
  cat(sprintf("\n***** UNIT %s *****\n",unit_name[k]))
  res.cox_each[[k]] <- coxph(Surv(T_diff, censored) ~ X1 + X2 + X3, data =dp[[k]])
  betas = rbind(betas, res.cox_each[[k]]$coefficients) 
}
res.cox_all <- coxph(Surv(T_diff, censored) ~ X1 + X2 + X3, data = dp_all)
par(mfrow=c(1,1))
plot(survfit(res.cox_all), ylim=c(0, 1), xlab="Days",
     ylab="Survival Probability", main = "All Units",
     conf.int = FALSE)

par(mfrow=c(3,4))    # set the plotting area into a 3*4 array
for (k in 1:length(unit_name)) {
  plot(survfit(res.cox_each[[k]]), ylim=c(0, 1), xlim = c(0,20),
       main = sprintf("Unit %s",unit_name[k]), conf.int = FALSE)
}

