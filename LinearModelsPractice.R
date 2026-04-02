#Linear Models Practice
#Carson Archie 

#Load in packages
library(tidyverse)
library(lme4)
library(emmeans)
library(multcomp)

#load in data
data("mtcars")

#Plot regression of mpg to weight
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_smooth(method = lm, se = FALSE, color = "grey") +
  geom_point(aes(color = wt)) +
  xlab("Weight") + 
  ylab("Miles per gallon") +
  scale_colour_gradient(low = "forestgreen", high = "black") +
  theme_classic()

#Basic lm 
lm(mpg~wt, data = mtcars)

#get summary statistics of a lm 
summary(lm(mpg~wt, data = mtcars))

#Get anova
anova(lm(mpg~wt, data = mtcars))


#Get a correlation test
cor.test(mtcars$wt, mtcars$mpg)

#Get residual plot with lm model
model <- lm(mpg~wt, data = mtcars)
ggplot(model, aes(y = .resid, x = .fitted)) +
  geom_point() +
  geom_hline(yintercept = 0)


#Bull Richness data set
bull.rich <- read.csv("Bull_richness.csv")

#Filtering for 1 treatment
bull.rich %>%
  filter(GrowthStage == "V8" & Treatment == "Conv.") %>%
  ggplot(aes(x = Fungicide, y = richness)) + 
  geom_boxplot()

#Run a t test on the filtered data
bull.rich.sub <- bull.rich %>%
  filter(GrowthStage == "V8" & Treatment == "Conv.")

t.test(richness~Fungicide, data = bull.rich.sub)


t.test(richness~Fungicide, data = bull.rich.sub, var.equal = TRUE)

#Summary of lm data
summary(lm(richness~Fungicide, data = bull.rich.sub))

#anova of lm data
anova(lm(richness~Fungicide, data = bull.rich.sub))


#Filter for 3 growth stages
bull.rich.sub2 <- bull.rich %>%
  filter(Fungicide == "C" & Treatment == "Conv." & Crop == "Corn")

#Visualise the data
bull.rich.sub2$GrowthStage <- factor(bull.rich.sub2$GrowthStage, levels = c("V6", "V8", "V15"))

ggplot(bull.rich.sub2, aes(x = GrowthStage, y = richness)) +
  geom_boxplot()

#New summary of data
lm.growth <- lm(richness ~ GrowthStage, data = bull.rich.sub2)
summary(lm.growth)

#Run anova
anova(lm.growth)

#Summary of data
summary(aov(richness ~ GrowthStage, data = bull.rich.sub2))


lsmeans <- emmeans(lm.growth, ~GrowthStage) # estimate lsmeans of variety within siteXyear
Results_lsmeans <- cld(lsmeans, alpha = 0.05, reversed = TRUE, details = TRUE) # contrast with Tukey ajustment by default. 
Results_lsmeans


bull.rich.sub3 <- bull.rich %>%
  filter(Treatment == "Conv." & Crop == "Corn")

bull.rich.sub3$GrowthStage <- factor(bull.rich.sub3$GrowthStage, levels = c("V6", "V8", "V15"))

# write it like this
lm.inter <- lm(richness ~ GrowthStage + Fungicide + GrowthStage:Fungicide, data = bull.rich.sub3)

# or like this
lm(richness ~ GrowthStage*Fungicide, data = bull.rich.sub3)

summary(lm.inter) # significant terms

anova(lm.inter) # The interaction term is signifant. 

bull.rich.sub3 %>%
  ggplot(aes(x = GrowthStage, y = richness, fill = Fungicide)) +
  geom_boxplot()

lsmeans <- emmeans(lm.inter, ~Fungicide|GrowthStage) # estimate lsmeans of variety within siteXyear
Results_lsmeans <- cld(lsmeans, alpha = 0.05, reversed = TRUE, details = TRUE) # contrast with Tukey ajustment
Results_lsmeans

#Mixed effect model
lme0 <- lm(richness ~ GrowthStage*Fungicide, data = bull.rich.sub3)

lme1 <- lmer(richness ~ GrowthStage*Fungicide + (1|Rep), data = bull.rich.sub3)
summary(lme1)

#Summary of different models
summary(lme0)
summary(lme1)
