require(mice)

# Q1
data1 = nhanes
sum(is.na(data1,byrow = TRUE))/nrow(data)

imps1 <- mice(data1, printFlag = FALSE,m = 30,donors = 3, maxit = 30, seed = 1)
fits1 <- with(imps1, lm(bmi ~ age + hyp + chl))
ests1 <- pool(fits1)
summary(ests1, conf.int = TRUE)

for(i in c(2,3,4,5,6)){
  imps1 <- mice(data1, printFlag = FALSE,m = 30,donors = 3, maxit = 30, seed = i)
  fits1 <- with(imps1, lm(bmi ~ age + hyp + chl))
  ests1 <- pool(fits1)
  print(summary(ests1, conf.int = TRUE))
}

for(i in c(5,100)){
  imps1 <- mice(data1, printFlag = FALSE,m = i,donors = 3, maxit = 30, seed = 1)
  fits1 <- with(imps1, lm(bmi ~ age + hyp + chl))
  ests1 <- pool(fits1)
  print(summary(ests1, conf.int = TRUE))
}

# Q2
dataex2 = load(file = "dataex2.Rdata")
count = 0
for(i in c(1:100)){
  data2 = dataex2[,,i]
  imps2 = mice(data2, printFlag = FALSE, method = 'norm.predict', m = 20,donors = 3, seed = 1)
  fits2 <- with(imps2, lm(Y ~ X))
  ests2 <- pool(fits2)
  low = summary(ests2, conf.int = TRUE)['2.5 %'][2,1]
  up = summary(ests2, conf.int = TRUE)['97.5 %'][2,1]
  if(up>=3 && low <= 3) count = count +1
}
print(count/100)

count1 = 0
for(i in c(1:100)){
  data2 = dataex2[,,i]
  imps2 = mice(data2, printFlag = FALSE, method = "norm.boot",m = 20,donors = 3, seed = 1)
  fits2 <- with(imps2, lm(Y ~ X))
  ests2 <- pool(fits2)
  low = summary(ests2, conf.int = TRUE)['2.5 %'][2,1]
  up = summary(ests2, conf.int = TRUE)['97.5 %'][2,1]
  if(up >= 3 && low <= 3) count1 = count1 +1
}
print(count1/100)



# Q4
# a
load(dataex4.Rdata)
imps4 <- mice(dataex4,method = "norm.boot", m = 20, donors = 3,
             printFlag = FALSE, seed = 1)
imps4$predictorMatrix
fits4 <- with(imps4, lm(y ~ x1 + x2 + x1:x2))
ests4 <- pool(fits4)
summary(ests4, conf.int = TRUE)

# b
interaction = dataex4['x1']*dataex4['x2']
dataex4_new = cbind(dataex4,interaction)
colnames(dataex4_new)=c('y','x1','x2','interaction')

imp0 <- mice(dataex4_new, maxit = 0)
meth <- imp0$method
meth["interaction"] <- "~I(x1*x2)"
meth
pred <- imp0$predictorMatrix
pred[c("x1", "x2"), "interaction"] <- 0
pred["x1", "x2"] <- 1
pred["x2", "x1"] <- 1
pred
imp4 <- mice(dataex4_new, method = meth, predictorMatrix = pred,
            maxit = 20, m = 50, seed = 1, printFlag = FALSE)
fit4 <-  with(imp4, lm(y ~ x1 + x2 + interaction))
ests4 <- pool(fit4)
summary(ests4, conf.int = TRUE)

# c
imps4 <- mice(dataex4_new, printFlag = FALSE,m = 50,maxit = 20, seed = 1)
fit4 <- with(imps4, lm(y ~ x1 + x2 + interaction))
ests4 <- pool(fit4)
summary(ests4, conf.int = TRUE)

# Q5
# build the imp model
NHANES2 = load(file = "NHANES2.Rdata")
data5 = data.frame(NHANES2['wgt'],NHANES2['gender'],NHANES2['age'],NHANES2['hgt']
                   ,NHANES2['WC'])
imps5 = mice(data5,printFlag = FALSE,m = 30,donors = 3, maxit = 30, seed = 1)
imps5$loggedEvents
plot(imps5)
densityplot(imps5)
densityplot(imps5, ~hgt|gender)
fit5 <- with(imps5, lm(wgt ~ gender + age + hgt + WC))
plot(fit5$analyses[[1]]$fitted.values, residuals(fit5$analyses[[1]]),
     xlab = "Fitted values", ylab = "Residuals")
qqnorm(rstandard(fit5$analyses[[1]]), xlim = c(-4, 4), ylim = c(-6, 6))
qqline(rstandard(fit5$analyses[[1]]), col = 2)
ests <- pool(fit5)
summary(ests, conf.int = TRUE)
pool.r.squared(ests, adjusted = TRUE)

imps5_new = mice(data5,printFlag = FALSE,m = 50,donors = 3, maxit = 30, seed = 1)
plot(imps5_new)
fit5_new <- with(imps5_new, lm(wgt ~ gender + age + hgt + WC))
ests_new <- pool(fit5_new)
summary(ests_new, conf.int = TRUE)
pool.r.squared(ests, adjusted = TRUE)











