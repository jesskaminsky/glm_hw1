#Jess, Blain, David
#############PROBLEM 1###################
wine <- read.csv('Wine.csv')
attach(wine)
summary(wine)

cor(WINE, MORTALITY)
plot(WINE, MORTALITY)
lm(MORTALITY ~ WINE, wine)
qqplot(WINE, MORTALITY)

cor(log(WINE), log(MORTALITY))
plot(log(WINE), log(MORTALITY))
lm(log(MORTALITY) ~ log(WINE), wine)
qqplot(log(WINE), log(MORTALITY))

### add a column for wine consumption level - a categorical variable
wine$level[1:6] <- "low"
wine$level[7:12] <- "medium"
wine$level[13:18] <- "high"
#look for a significant difference in mean death rate within each level group
#then

#############PROBLEM 2###################
####part a####
flowers <- read.csv('flowers.csv')
flowers[,4] <- rep(c(1,2))
names(flowers)[4] <- "REPLICATE"
flowers$REPLICATE <- as.factor(REPLICATE)

####part b####
min(flowers$INTENS)
max(flowers$INTENS)
flowers[,5] <- NA
names(flowers)[5] <- "INTENS_CAT"
for (i in 1:nrow(flowers)) {
  if(flowers$INTENS[i] < 275) {
    flowers$INTENS_CAT[i] <- 1
  }
  else if (flowers$INTENS[i] < 400) {
    flowers$INTENS_CAT[i] <- 2
  }
  else if (flowers$INTENS[i] < 525) {
    flowers$INTENS_CAT[i] <- 3
  }
  else if (flowers$INTENS[i] < 650) {
    flowers$INTENS_CAT[i] <- 4
  }
  else if (flowers$INTENS[i] < 775) {
    flowers$INTENS_CAT[i] <- 5
  }
  else { 
    flowers$INTENS_CAT[i] <- 6
  }
}
flowers$INTENS_CAT <- as.factor(INTENS_CAT)

t.test(flowers$FLOWERS ~ flowers$REPLICATE)

####part c####
#The research questions are: What are effects of intensity and timing? 
#Is there an interaction between the two factors?

####part d####
#First create an analysis of variance using timing and the categorical 
#form of the light intensity variable. 
#Determine if there is an effect of each factor.
summary(aov(FLOWERS ~ TIME + INTENS_CAT, flowers))
m1 <- lm(FLOWERS ~ TIME + INTENS_CAT, flowers)

####part e####
#Then create an interaction between light intensity and timing by 
#multiplying the two variables and test for the presence of an interaction.
summary(aov(FLOWERS ~ TIME + INTENS_CAT + TIME*INTENS_CAT, flowers))
m2 <- lm(FLOWERS ~ TIME + INTENS_CAT + TIME*INTENS_CAT, flowers)

####part f####
#Now repeat the process but using light intensity as a continuous variable.
summary(aov(FLOWERS ~ TIME + INTENS, flowers))
m3 <- lm(FLOWERS ~ TIME + INTENS, flowers)
summary(aov(FLOWERS ~ TIME + INTENS + TIME*INTENS, flowers))
m4 <- lm(FLOWERS ~ TIME + INTENS + TIME*INTENS, flowers)

####part g####
#Then perform F-tests to compare the four model you have created 
#(light as continuous and categorical with and without the interaction)
anova(m1, m2, m3, m4)

####part h####
#Predict the number of flowers grown at each combination 
#of light and timing for each of the four models.
m1_predict <- predict(m1)
m2_predict <- predict(m2)
m3_predict <- predict(m3)
m4_predict <- predict(m4)

predicted_values <- cbind(time = flowers$TIME, intensity = flowers$INTENS, 
                          intensity_cat = flowers$INTENS_CAT,
                          m1 = m1_predict, m2 = m2_predict, m3 = m3_predict,
                          m4 = m4_predict)
####part i####
#Compare each prediction to the observed number of flowers and calculate the 
#difference (observed – predicted). This is the residual. Calculate the residual mean
#squared error for each model by adding the squared residuals together and dividing by 
#the number of residual degrees of freedom. This should equal the mean squared error in each 
#ANOVA table.

##calculate residual values
residual_values <- cbind(m1 = flowers$FLOWERS - m1_predict, m2 = flowers$FLOWERS - m2_predict,
                         m3 = flowers$FLOWERS - m3_predict, m4 = flowers$FLOWERS - m4_predict)

##calculate MSE manually and show that it is equal to the MSE from the anova table
MSE_m1 <- sum(residual_values[,1] * residual_values[,1]) / m1$df.residual
ANOVA_MSE_1 <- summary(aov(m1))[[1]] $ `Mean Sq`[3]

MSE_m2 <- sum(residual_values[,2] * residual_values[,2]) / m2$df.residual
ANOVA_MSE_2 <- summary(aov(m2))[[1]] $ `Mean Sq`[4]

MSE_m3 <- sum(residual_values[,3] * residual_values[,3]) / m3$df.residual
ANOVA_MSE_3 <- summary(aov(m3))[[1]] $ `Mean Sq`[3]

MSE_m4 <- sum(residual_values[,4] * residual_values[,4]) / m4$df.residual
ANOVA_MSE_4 <- summary(aov(m4))[[1]] $ `Mean Sq`[4]


####part j####
#Now plot the residuals vs. the predicted for each model and see if there are any patterns. 
#If you see any, what might you do to remove them?
#model 1 plot
plot(residual_values[,1], predicted_values[,4])
plot(residual_values[,2], predicted_values[,5])
plot(residual_values[,3], predicted_values[,6])
plot(residual_values[,4], predicted_values[,7])


####part k####
#Finally, take the model you think describes the data the best and write a short report for 
#your grandmother who would like to grow these flowers carefully explaining to her how she 
#should best grow them and why. Note that your grandmother is curious about how much changes 
#in light and timing might affect her flowers and how sensitive her results will be to the 
#settings she makes.

