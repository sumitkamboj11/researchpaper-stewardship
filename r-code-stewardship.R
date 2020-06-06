library(dplyr)
library(caret)
library(caTools)
library(readxl)
data <- read_xlsx(file.choose())
str(data)
regressor = lm(formula = SOB ~`Value_Congruence`*`Effective_communication`+Autonomy*`Effective_communication`,
               data = data)

Value Congruence*Effective communication + Autonomy*Effective communication
summary(regressor)
library(rms)
plot(regressor)
mcDiagnose(regressor)
getPartialCor(regressor)
mcGraph3(data$VC,data$EC,data$DS)
library(sjPlot)
library(ggplot2)
plot_model(regressor, type = "pred",terms = "EC")
plot<-plotSlopes()

m1ps<-plotSlopes(model = regressor, plotx = "Autonomy", modx ="Effective_communication",modxVals = "std.dev")
m1psts <- testSlopes(m1ps)
plot(m1psts)


ggplot(aes(data$EC,regressor))
terms = "VC","EC"
predict()
library(corrplot)
correlations = cor(data[,1:6])
corrplot(correlations, method="circle")
install.packages("rockchalk")
library(rockchalk)
rockchalk

regressor1 = lm(formula = DS ~ AT,
               data = data)
summary(regressor1)
plot(regressor1)
plotSlopes(regressor,plotx = "SOB",modx = "Autonomy",modxVals = "std.dev")
plotCurves(regressor,plotx = "Autonomy",modx = "Effective_communication",modxVals = "std.dev")
