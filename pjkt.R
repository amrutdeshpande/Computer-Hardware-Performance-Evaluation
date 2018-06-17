hardproject <- read.csv(file.choose(),header=T)
lm(y~Frequency+Angle.of.attack+Chord.length+Free.stream.velocity+Suction.side.displacement.thick,airn)
modelhardproject <- lm(y~.,hardproject)
attach(hardproject)
xsummary(modelhardproject)

anova(modelfmet)
plot(modelhardware)

print(vif(modelhardproject))

par(mfrow=c(2,2))
abline(model1)
plot(Scaled.sound.pressure.level,Frequency)
xnew <- x7/x3
attach(airn)
lm(newy~Frequency+Angle.of.attack+Chord.length+Free.stream.velocity+Suction.side.displacement.thick,airn)
model2 <- lm(newy~Frequency+Angle.of.attack+Chord.length+Free.stream.velocity+Suction.side.displacement.thick,airn)
plot(model2)
newresponse <- log(Scaled.sound.pressure.level,base=exp(1))
lm(newresponse~Frequency+Angle.of.attack+Chord.length+Free.stream.velocity+Suction.side.displacement.thick,airn)
model3 <- lm(newresponse~Frequency+Angle.of.attack+Chord.length+Free.stream.velocity+Suction.side.displacement.thick,airn)
plot(model3)
newr <- sqrt(Scaled.sound.pressure.level)+1
model4 <- lm(newr~Frequency+Angle.of.attack+Chord.length+Free.stream.velocity+Suction.side.displacement.thick,airn)
plot(model4)
boxcox(y~ ., data=hardproject, lambda = seq(-2, 2, 1/10), plotit = TRUE, eps = 1/50, xlab = expression(lambda),ylab = "log-Likelihood")
newy2 <- (estimated.relative.perf)^0.5

newytube = hardproject[,8]
newytube = newy2^0.5

datytube = ytube[,-14]
datytube['y'] <- newy2
View(datytube)

mod1 = lm(y ~., data = dat1)
plot(mod1)


write.csv(dat, file = 'newdata')
