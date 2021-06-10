library(pROC)

file1 <-"Desktop/Ingoodwetrust/libros/ADHD_dataset_2/var_st_entropy_AP_RP_Control_ADHD_V2_1.csv"
df1 <- read.csv(file1)

file0 <-"Desktop/Ingoodwetrust/libros/ADHD_dataset_2/var_st_entropy_AP_RP_Control_ADHD_V2_0.csv"
df0 <- read.csv(file0)

#df1 <- sample_frac(df1, 1L)
#df0 <- sample_frac(df0, 1L)

#====================function of maximum likehood===============================

maxlikehood <- function(train,test)
{
  
  x <- train
  n <- length(train)
  
  #mean
  m <- (1/n)*(sum(x))
  #sd
  s = sqrt(sum((x - m)^2)/(n))
  
  
  #probability of occurrence
  f <- function(x){1/sqrt(2*pi*s^2)*exp(-((x-m)^2/(2*s^2)))}
  
  fx <- f(test)
  return <- fx
  
}

#======================Normalize the data=========================

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

standarize <- function(x){
  meanX <- mean(x)
  sdX <- sd(x)
  stz.df <- (x-meanX)/sdX
  return(stz.df)
}



#df1$Staentropy <- standarize(df1$entropy)
#df1$Norentropy <- normalize(df1$entropy)

#x11()
#plot(density(df1$Staentropy))
#x11()
#plot(density(df1$Norentropy))
#x11()
#plot(density(df1$entropy))

#df0$Staentropy <- standarize(df0$entropy)
#df0$Norentropy <- normalize(df0$entropy)

#x11()
#plot(density(df0$Staentropy))
#x11()
#plot(density(df0$Norentropy))
#x11()
#plot(density(df0$entropy))

#x11()
#plot(density(df0$entropy))
#lines(density(df1$entropy))


#x11()
#plot(density(df0$Norentropy))
#lines(density(df1$Norentropy))


#df1$Stasd <- standarize(df1$sd)
#df1$Norsd <- normalize(df1$sd)
#df0$Stasd <- standarize(df0$sd)
#df0$Norsd <- normalize(df0$sd)

#x11()
#plot(density(df0$sd))
#lines(density(df1$sd))

#df1$Norvar <- normalize(df1$var)
#df0$Norvar <- normalize(df0$var)

#x11()
#plot(density(df0$Norvar))
#lines(density(df1$Norvar))

#df1$NorAbsolutePower <- normalize(df1$AbsolutePower)
#df0$NorAbsolutePower <- normalize(df0$AbsolutePower)

#x11()
#plot(density(df0$NorAbsolutePower))
#lines(density(df1$NorAbsolutePower))

#df1$NorRealitivePower <- normalize(df1$RealtivePower)
#df0$NorRealitivePower <- normalize(df0$RealtivePower)

#x11()
#plot(density(df0$RealtivePower))
#lines(density(df1$RealtivePower))

#=========================Normal code===================
df1$var<-normalize(df1$var)
df0$var<-normalize(df0$var)
df1$sd<-normalize(df1$sd)
df0$sd<-normalize(df0$sd)
df1$entropy<-normalize(df1$entropy)
df0$entropy<-normalize(df0$entropy)
df1$AbsolutePower<-normalize(df1$AbsolutePower)
df0$AbsolutePower<-normalize(df0$AbsolutePower)
df1$RealtivePower<-normalize(df1$RealtivePower)
df0$RealtivePower<-normalize(df0$RealtivePower)

#Ab.pow1 <- data.frame(df1$AbsolutePower)
#Ab.pow0 <- data.frame(df0$AbsolutePower)
#=================Spread the data to train and test=================

sample1 <- sample(c(TRUE, FALSE), nrow(df1), replace=TRUE, prob=c(0.7,0.3))
train1 <- df1[sample1, ]
test1 <- df1[!sample1, ]

sample0 <- sample(c(TRUE, FALSE), nrow(df0), replace=TRUE, prob=c(0.7,0.3))
train0 <- df0[sample0, ]
test0 <- df0[!sample0, ]

#test <- c(test1,test0)
test <-  rbind(test1,test0)
#========================train and test the model==================

prob_oc1<-maxlikehood(train1$AbsolutePower,test$AbsolutePower) 

prob_oc0<-maxlikehood(train0$AbsolutePower,test$AbsolutePower)

#===================== predict value=================
#test$prob.oc1 <- data_frame(prob.oc1)
test$pred_class_AP <- ifelse(prob_oc1 >= prob_oc0, "1", "0")


#========================for entropy===========
prob_oc1<-maxlikehood(train1$entropy,test$entropy) 
prob_oc0<-maxlikehood(train0$entropy,test$entropy)
test$pred_class_En <- ifelse(prob_oc1 >= prob_oc0, "1", "0")


#========================for Realtive Power===========
prob_oc1<-maxlikehood(train1$RealtivePower,test$RealtivePower) 
prob_oc0<-maxlikehood(train0$RealtivePower,test$RealtivePower)
test$pred_class_ReaPow <- ifelse(prob_oc1 >= prob_oc0, "1", "0")


#========================for sd===========
prob_oc1<-maxlikehood(train1$sd,test$sd) 
prob_oc0<-maxlikehood(train0$sd,test$sd)
test$pred_class_Sd <- ifelse(prob_oc1 >= prob_oc0, "1", "0")

#========================for var===========
prob_oc1<-maxlikehood(train1$var,test$var) 
prob_oc0<-maxlikehood(train0$var,test$var)
test$pred_class_Var <- ifelse(prob_oc1 >= prob_oc0, "1", "0")

#=================mode

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
a <- print(length(test$label))
# Create the vector with numbers.
for( i in 1:a){
  v <- c(test$pred_class_AP[i],test$pred_class_En[i],test$pred_class_ReaPow[i],test$pred_class_Sd[i],test$pred_class_Var[i])
  print(v)
  test$pred_class[i] <- getmode(v)
  print(i)
}

#v <- c(test$pred_class_AP,test$pred_class_En,test$pred_class_ReaPow,test$pred_class_Sd,test$pred_class_Var)

#test$pred_class <- getmode(v)

#test$label
#test$pred_class
ctab_test <- table(test$label, test$pred_class)
ctab_test

accuracy_test <- sum(diag(ctab_test))/sum(ctab_test)*100
accuracy_test

Recall <- (ctab_test[2, 2]/sum(ctab_test[2, ]))*100
Recall

Precision <- (ctab_test[2, 2]/sum(ctab_test[, 2]))*100
Precision

F_Score <- (2 * Precision * Recall / (Precision + Recall))/100
F_Score



#roc <- roc(train$label, model1$fitted.values)
#auc(roc)

#plot(density(df1$var))
#lines(density(df0$var))

#plot(density(df1$sd))
#lines(density(df0$sd))

#plot(density(df0$entropy))
#lines(density(df1$entropy))

#plot(density(df1$AbsolutePower))
#lines(density(df0$AbsolutePower))


#plot(density(df1$RealtivePower))
#lines(density(df0$RealtivePower))


