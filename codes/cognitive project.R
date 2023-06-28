library(ggplot2)
library(hrbrthemes)
library(ggExtra)
library(dplyr)
library(tidyr)
library(viridis)
library(plotly)
library(lattice)
library(recipes)
library(caret)
library(gains)
library(pROC)

#prepreprocess data
data = read.csv("E:/Users/ASUS/Documents/term8/ravesh tahghigh/clean data.csv",header = TRUE)
attach(data)
subj = as.factor(subj)
trial = as.numeric(trial)
s = as.factor(S)
r = as.factor(R)
rt = as.numeric(RT)
correct = ifelse(correct=="TRUE",1,0)
correct = as.factor(correct)
instruction = as.factor(instruction)
new.data = data.frame(subj,trial,s,r,rt,correct,instruction)

#EDA PART
#histogram of response time when instruction = accuracy
accuracy.rt = subset(new.data, instruction == "accuracy")$rt
hist(accuracy.rt, col = "pink",
     main = "histogram of response time when instruction = accuracy",
     xlab = "response time", ylab = "frequency")

#correct vs subject
correct.subj = table(correct,subj)
barplot(correct.subj,
        legend.text = TRUE,
        beside = TRUE,
        main = "number of right and wrong choices for each person",
        xlab = "",
        ylab = "",col=c("darkorchid4","darkorchid1"),
        horiz=T, las=1)

#instruction vs correct
correct.instruction = table(correct,instruction)
correct.instruction.bar = barplot(correct.instruction,
        legend.text = TRUE,
        beside = TRUE,
        main = "number of right and wrong choices for each instruction",
        xlab = "type of instruction",
        ylab = "",col=c("aliceblue","cadetblue1"),
        horiz=F, las=1)

#response time vs correct
true.rt = subset(new.data, correct == "1")$rt
false.rt = subset(new.data, correct == "0")$rt
hist(true.rt, breaks=30,  col=rgb(1,0,0,0.5), xlab="response time",
     ylab="frequency", main="distribution of response time for each choice" )
hist(false.rt, breaks=30, col=rgb(0,0,1,0.5), add=T)
legend("topright", legend=c("TRUE","FALSE"), col=c(rgb(1,0,0,0.5), 
                                rgb(0,0,1,0.5)), pt.cex=2, pch=15)

#response time vs instruction
instruction.rt = table(rt,instruction)
boxplot(rt ~ instruction , col=terrain.colors(4),
        main = "distribution of response time for each instruction type",
        xlab = "instruction",
        ylab = "response time")

#response time vs instruction
ggplot(data=new.data, aes(x=rt, group=instruction, fill=instruction)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum() +
  ylab("") +
  xlab("response time") +
  labs(title = "distribution of response time for each type of instruction")

#response time vs instruction vs correct
mean.rt <- aggregate(rt ~ instruction + correct, data = new.data, mean)
mean.rt=data.frame(mean.rt)
mean.rt <- mean.rt %>%
  mutate(text = paste0("response time mean: ",rt))
p <- ggplot(mean.rt, aes(instruction, correct, fill= rt, text=text)) + 
  geom_tile() +
  theme_ipsum()
ggplotly(p, tooltip="text")

#test 
chisq.test(subj,correct)
chisq.test(subj,instruction)
chisq.test(instruction,correct)

#definition train and validation and test data
set.seed(42)
train_rows=sample(nrow(new.data),9500)
train_data=new.data[train_rows,]
table(train_data$correct)
validation_rows=sample(setdiff(row.names(new.data),train_rows),4000)
validation_data=new.data[validation_rows,]
table(validation_data$correct)
test_rows=setdiff(row.names(new.data),c(train_rows,validation_rows))
test_data=new.data[test_rows,]
table(test_data$correct)

#model
model=glm(correct~subj+rt+instruction,data=train_data,family="binomial")
predict.model.validation=predict(model,validation_data,type="response")
confusionMatrix(
  data=as.factor(ifelse(predict.model.validation>0.5,1,0)),
  reference=validation_data$correct)
summary(model)

#over fitting check
predict.model.train=predict(model,train_data,type="response")
confusionMatrix(
  data=as.factor(ifelse(predict.model.train>0.5,1,0)),
  reference=train_data$correct)

#step methods
#backward
model.back=model %>% stats::step(direction = "backward")
model.back.pred=predict(model.back,validation_data,type="response")
confusionMatrix(
  data=as.factor(ifelse(model.back.pred>0.5,1,0)),
  reference=validation_data$correct)

#forward
model.for=model %>% stats::step(direction = "forward")
model.for.pred=predict(model.for,validation_data,type="response")
confusionMatrix(
  data=as.factor(ifelse(model.for.pred>0.5,1,0)),
  reference=validation_data$correct)

#both
model.both=model %>% stats::step(direction = "both")
model.both.pred=predict(model.both,validation_data,type="response")
confusionMatrix(
  data=as.factor(ifelse(model.both.pred>0.5,1,0)),
  reference=validation_data$correct)

#lift chart
logit.reg.lift=lift(relevel(as.factor(correct),ref="1")~
                      predict.model.validation,data=validation_data)
xyplot(logit.reg.lift,plot="gain")

#decile lift chart
gain=gains(as.numeric(validation_data$correct),predict.model.validation)
heights=gain$mean.resp/mean(as.numeric(validation_data$correct))
decile.chart=barplot(heights,names.arg=gain$depth,
                     xlab="percentile",ylab="mean response")

#roc plot
r=roc(validation_data$correct,predict.model.validation)
plot.roc(r)
auc(r)

#gamma regression models for "response time" variable
gamma.model = glm(rt~correct+instruction,data=train_data,
                  family = Gamma(link = "identity"))
predict.gamma.model.validation=predict(gamma.model,
                                       validation_data,type="response")
confusionMatrix(
  data=as.factor(ifelse(predict.gamma.model.validation>0.5,1,0)),
  reference=as.factor(ifelse(validation_data$rt>0.5,1,0)))
summary(gamma.model)

#over fitting check
predict.gamma.model.train=predict(gamma.model,train_data,type="response")
confusionMatrix(
  data=as.factor(ifelse(predict.gamma.model.train>0.5,1,0)),
  reference=as.factor(ifelse(train_data$rt>0.5,1,0)))

#visualization of gamma model
# shape: 1 divided by dispersion parameter
m0_shape <- 1/0.08938668
# scale: mean/shape
m0_scale <- sum(coef(gamma.model))/m0_shape
hist(rt, breaks = 40, freq = FALSE)
curve(dgamma(x, shape = m0_shape, scale = m0_scale), 
      from = 0.2, to = 1.4, col = "red", add = TRUE)

#compare simulation of this gamma model with real data
sims1 <- simulate(gamma.model, nsim = 1000)
plot(density(rt), 
     main = "Simulated data for gamma model")
for(i in 1:50)lines(density(sims1[[i]]), col="green")
