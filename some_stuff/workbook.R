library(ggcorrplot)
library(caret)
library(logistf)
library(dplyr)
caret::findC
install.packages("ResourceSelection")
setwd("C:/Users/user/Desktop/studia/Data Science/Semestr 2/Advanced_Econometrics/projekt/Advanced_Econometric_Default_Modelling_in-R")
data1 <- read.csv2("imputedData.csv")
missing_d <- data1[rowSums(is.na(data1))>0,]
sum(missing_d$class)
data1 <- data1[rowSums(is.na(data1))==0,]
coln <- as.character(read.csv2("column_names_1.csv", header = FALSE)$V2)
coln <- c(coln, "BANKRUPTCY")
colnames(data1) <- coln
data1$INONE <- ifelse(NITA>0, 1, 0)
data1$OENEG <- ifelse(TLTA>=1, 1, 0)
y_3 <- data1$BANKRUPTCY
data1$BANKRUPTCY <- NULL
X_3 <- data1


## To see if we can limit number of features in our model first we decided to run Logit regression with one covariate at a time. 
## If the pvalue of the variable is > 0.25 we will drop it. 

selected_names <- c()
selected_names <- lapply(names(X_3),function(name){
one_cov <- glm(y_3 ~ X_3[,name], family=binomial(link= "probit"))

if (as.numeric(summary(one_cov)$coefficients[,4][2]) < 0.25){

  selected_names <- c(selected_names, name)
}

})

selected_names <- unlist(selected_names)

selected_names

data1 <- data1 %>% dplyr::select(selected_names)

## In this way we extracted 48 variables which pvalues for the regression y ~ variable[i] were less than 0,25
## It helped us to reduce the dataset by 17 variables

## The another approach to limit number of variables is to drop those which are highly correlated. 
## After the analysis of correlation data we removed next 15 variables
corr <- round(cor(X_3), 1)
to_drop <- findCorrelation(corr, cutoff = 0.9, verbose = FALSE, names = F,
                exact = ncol(corr) < 100)

data1 <- data1[,-to_drop]
data1$BANKRUPTCY <- y_3



## Now we have 33 dependent variables. It is still a lot. Our next step will be to run stepwise regression in order to limit number of 
## features
library(MASS)
# Fit the full model 
full.model <- glm(BANKRUPTCY ~.,data = data1, family=binomial(link= "probit"), control = list(maxit = 50))
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)




# After running the stepwise regression we ended up with the model which uses 27 dependent variables from which 21 are significant
model_summary <- summary(step.model)
model_summary


lrtest(step.model, myprobit_final)

## Checking number of significant variables
i <- 0
for (x in model_summary$coefficients[,4]){
  if (x <= 0.05){
    i <- i + 1
  }
}


myprobit2 <- glm(BANKRUPTCY~SIZE+TLTA+WCTA+OENEG+NITA+FUTL+SIZE2+CHSALES+CHSALES:INONE, 
                 data=ohl_data, family=binomial(link="probit"))
lrtest(myprobit2, myprobit_final)


xd <- anova(myprobit_final, step.model, test= "LRT")

as.data.frame(xd)
# What should be stated here is that the standard logistic regression signalised us that there might be a problem of perfect or quasi perfect separation. 
# It brigns the risk that obtained results might be inflated and do not properly reflect the reality. Surely there is a question
# whether this is just some problem with the data we use or maybe it is the real thing for the whole population. If the second option
# were true no special actions would be necessary because the perfect separation would be just the proper result of a data. 
# However it is not easy to give such an answer. It is possible that actually companies which are supposed to go bancrupt have all the financial
# indicators worse than those which are not going to default. However it should be verified how performs the model which removes the issue
# of perfect separation. The way to deal with it is to use logistic regression model which uses Firth's bias reduction. In this case we 
# can't repeat the process of stepwise regression because the Logistic Regression with Firth's bias reduction is much more computionaly demanding
# than the standard one. We will just run the model on full data and than remove insignificant variables. Obviously it is not a perfect solution 
# but there are no better options available. 

full.model_firths <- logistf(formula = class ~.,data = data1)
step.model <- stepAIC(full.model_firths, direction = "both", 
                      trace = FALSE)


full_model_firths_summary <- summary(full.model_firths)

i <- 0

insignificants_firths <- c()
for (x in names(full_model_firths_summary$prob)){
  print(x)
  if (full_model_firths_summary$prob[x] >= 0.05){
    insignificants_firths <- c(insignificants_firths,x)
    i <- i + 1
  }
}


final.model_firths <- logistf(formula = class ~.,data = data1 %>% select(-insignificants_firths))

final.model_firths$prob
saveRDS(step.model, "step_model.RDS")
xx <- readRDS("step_model.RDS")

step.model$formula
sum$coefficients
mm <- broom::tidy(sum$coefficients)


kableExtra::kable(mm)
# Unfortunately after the analysis we had to drop the Logistic Regression with Firth's bias reduction. The stepwise regression in this case
# is to computionaly demanding. Also, when we dropped insignificant variables we ended up with a model with 10 significant and 8 insignificant
# variables we had to drop this way of solving our problem and just stay with the model we obtained before and it is the one we will use
# to compare with Ohlson type. 
