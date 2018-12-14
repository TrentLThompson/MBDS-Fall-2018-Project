setwd("~/Desktop/MBDS/BDS 522 Statistical Reasoning for Behavioral Science/Final Project")
library(readxl)
MBDSproject <- read_excel("~/Desktop/MBDS/BDS 501 Behavioral Economics & Psychology/MBDSproject.xlsx", 
                            +     sheet = "data")
View(MBDSproject)
attach(MBDSproject)

boxplot(MBDSproject$actual[depleted == "1"], 
        MBDSproject$actual[depleted == "0"], 
        names = c("Depleted", "Non-depleted"),
        xlab = "Treatment Conditions",
        ylab = "Number of matrices actually solved",
        main = "Performance on Matrices Solving Task")
t.test(MBDSproject$actual[depleted == "1"], MBDSproject$actual[depleted == "0"], alternative = "less")
### Due to design issues, we were not able to include a proper manipulation check to see if participants 
### in the depleted_loss and depleted_gain treatment groups truly were depleted of self-control. However, 
### we did hypothesize, in line with the literature, that subjects in these treatment groups would perform
### worse, on average, than subjects not depleted of self control. However, when we run a two-sample test of
### the difference in average performance for the matrices solving task between depleted and non-depleted
### participants, we fail to find any meaningful difference; t(129.41) = -0.6574, p = 0.2561. This perhaps 
### indicates that our depletion manipulation was not challenging enough or lasting enough to have an effect
### on subsequent cognitive tasks. This would also help to explain why we failed to find any real meaningful
### results, or to substantiate any of our predictions. 

boxplot(MBDSproject$amt_cheat[depleted == "1"], 
        MBDSproject$amt_cheat[depleted == "0"], 
        names = c("Depleted", "Non-depleted"),
        xlab = "Treatment Conditions",
        ylab = "Number of matrices over-reported")

boxplot(MBDSproject$amt_cheat[gain == "0"], 
        MBDSproject$amt_cheat[gain == "1"], 
        names = c("Loss framing", "Gain framing"),
        xlab = "Treatment Conditions",
        ylab = "Number of matrices over-reported")

# Hypothesis 1a Test: Between the average rate of cheating in the two depleted vs. two non-depleted treatment groups
t.test(MBDSproject$cheated[depleted == "1"], MBDSproject$cheated[depleted == "0"], alternative = "greater")
Htest_1a <- t.test(MBDSproject$cheated[MBDSproject$depleted == "1"], MBDSproject$cheated[MBDSproject$depleted == "0"], alternative = "greater")
### Thus, at the 95% confidence level, we fail to reject the null hypothesis which says that there is no statistically 
### significant difference between the average rate of cheating accross the two depleted treatment conditions 
### (M = 0.1286, SD = 0.3371) and that of the two non-depleted treatment conditions 
### (M = 0.1290, SD = 0.3380); t(128) = -0.0078, p = 0.5031.
summary(glm(cheated ~ depleted, family = binomial, data = MBDSproject))

# Hypothesis 1b: Between the average amount of cheating in the two depleted vs. two non-depleted treatment groups
t.test(MBDSproject$amt_cheat[depleted == "1"], MBDSproject$amt_cheat[depleted == "0"], alternative = "greater")
Htest_2a <- t.test(MBDSproject$amt_cheat[depleted == "1"], MBDSproject$amt_cheat[depleted == "0"], alternative = "greater")
### Thus, at the 95% confidence level, we fail to reject the null hypothesis which says that there is no statistically 
### significant difference between the amount of cheating demonstrated by participants accross the two depleted treatment 
### conditions (M = 1.75, SD = 1.39) and that of the two non-depleted treatment conditions 
### (M = 4.25, SD = 3.33); t(9.37) = -1.9612, p = 0.9599.
t.test(MBDSproject$amt_cheat[depleted == "0"], MBDSproject$amt_cheat[depleted == "1"], alternative = "greater")
### In fact, at the 95% confidence level, we find evidence in support of the opposing alternative hypothesis, which essentially
### says that the amount of cheating demonstrated by participants accross the two non-depleted treatment conditions
### (M = 4.25, SD = 3.33) was statistically significantly greater than that of the two depleted treatment conditions
### (M = 1.75, SD = 1.39); t(9.37) = 1.9612, p = 0.0401, thus constituting a direct contradiction of what we expected 
### to find as well as the prior literature.
summary(lm(amt_cheat ~ depleted, data = MBDSproject))


# Hypothesis 2a: Between the average rate of cheating in the two loss framing vs. two gain framing treatment groups
t.test(MBDSproject$cheated[gain == "0"], MBDSproject$cheated[gain == "1"], alternative = "greater")
### Thus, at the 95% confidence level, we fail to reject the null hypothesis which says that there is no statistically 
### significant difference between the average rate of cheating demonstrated by participants accross the two loss framing
### treatment groups (M = 0.1525, SD = 0.36) and that of the two gain framing treatment groups (M = 0.1096, SD = 0.31); 
### t(115.56) = 0.7175, p = 0.2373.
summary(glm(cheated ~ gain, family = binomial, data = MBDSproject))

# Hypothesis 2b: Between the average amount of cheating in the two loss framing vs. two gain framing treatment groups
t.test(MBDSproject$amt_cheat[gain == "0"], MBDSproject$amt_cheat[gain == "1"], alternative = "greater")
### However, at the 95% confidence level, we do indeed reject the null hypothesis which says that there is no statistically 
### significant difference between the amount of cheating demonstrated by participants accross the two loss framing treatment 
### conditions (M = 3.38, SD = 3.20) and that of the two gain framing treatment conditions (M = 1.63, SD = 1.41); 
### t(9.61) = 2.2223, p = 0.0258. With this, we find evidence in support of the alternative hypothesis, which says that the 
### amount of cheating demonstrated by participants accross the two loss framing treatment groups is statistically significantly 
### greater than that which was demonstrated by participants accross the two gain framing treatment groups.
summary(lm(amt_cheat ~ gain, data = MBDSproject))



# Hypothesis 3a: 
summary(glm(cheated ~ condition, data = MBDSproject, family = binomial))
summary(glm(cheated ~ depleted*gain, data = MBDSproject, family = binomial))

summary(aov(cheated ~ condition, data = MBDSproject))

### Thus, at the 95% confidence level, we fail to reject the null hypothesis which says that there is no statistically
### significant difference between the average rate of cheating accross all the treatment groups [ie. depletion_loss 
### (M = 0.16, SD = 0.37); depletion_gain (M = 0.10, SD = 0.31); non-depletion_loss (M = 0.14, SD = 0.36); non-depletion_gain
### (M = 0.12, SD = 0.33)]; F(3, 128) = 0.201, p = 0.8960.


# Hypothesis 3b: 
summary(lm(amt_cheat ~ condition), data = MBDSproject)
summary(aov(MBDSproject$amt_cheat ~ MBDSproject$depleted*MBDSproject$gain))
my_aov <- aov(amt_cheat ~ condition, data = MBDSproject)
summary(my_aov)
TukeyHSD(my_aov)
plot(TukeyHSD(my_aov), las = 1)
### However, at the 95% confidence level, we do indeed reject the null hypothesis which says that there is no statistically 
### significant difference between the average amount (i.e. magnitude) of cheating accross all the treatment groups [ie. depletion_loss 
### (M = 2.25, SD = 1.89); depletion_gain (M = 1.25, SD = 0.50); non-depletion_loss (M = 6.5, SD = 2.89); non-depletion_gain
### (M = 2.00, SD = 2.00)]; F(3, 128) = 5.567, p = 0.0125. With this, we find evidence in support of the alternative hypothesis, which says that the 
### average amount of cheating demonstrated by participants differs accross the four treatment conditions. Moreover, through pair-wise 
### comparisons, we find that the average amount of cheating by cheaters in the non-depletion_loss condition was statistically significantly
### greater than that of cheaters in all other categories (i.e. NL > DL = NL = NG).

days <- table(MBDSproject$day)
barplot(days[order(days, decreasing = T)], 
        names.arg = c("Thu", "Wed", "Fri", "Sun", "Tue", "Mon"),
        ylab = "Number of Obs.", main = "# of Obs. Collected by Day of Week", ylim = c(0,40))


times <- table(MBDSproject$time_period)
barplot(times[order(times, decreasing = T)], 
        names.arg = c("Afternoon (12pm-5pm)", "Evening (5pm-10pm)", "Morning (8am-12pm)"),
        ylab = "Number of Obs.", 
        main = "Fig. 5a Distribution of Data Collection Across Times of Day (n = 132)", 
        ylim = c(0, 75),
        las = 1,
        col = c("salmon", "paleturquoise3", "darkseagreen2"))

genders <- table(MBDSproject$gender)
barplot(genders[order(genders, decreasing = T)], 
        names.arg = c("Female", "Male"),
        ylab = "Number of Obs.", main = "# of Obs. by Gender", ylim = c(0, 85),
        las = 1)

gend <- table(MBDSproject$gender, MBDSproject$condition)
barplot(gend, col=c("red","blue"), 
        legend = rownames(gend), 
        beside=TRUE, 
        ylab = "# of Observations",
        las = 1,
        ylim = c(0, 25),
        main = "Gender Distribution by Treatment Condition")

genderbytreatment <- table(MBDSproject$gender[condition == "DL"], MBDSproject$gender[condition == "DG"])

ages <- table(MBDSproject$age)
boxplot(age, horizontal = T, xlab = "Age")



years <- table(MBDSproject$uni_year)
barplot(years[order(years, decreasing = T)], 
        names.arg = c("Freshman", "Other", "Junior", "Sophomore", "Senior"),
        ylab = "Number of Obs.", ylim = c(0, 65),
        las = 1)

yearsss <- table(MBDSproject$uni_year, MBDSproject$condition)
barplot(yearsss, legend = rownames(yearsss), beside=TRUE)

majors <- table(MBDSproject$major)
barplot(majors[order(majors, decreasing = T)], 
        names.arg = c("Humanities", "Science", "Engineering", "Business", "Undecided", "Not listed", "Nursing", "Math"),
        ylab = "Number of Obs.", ylim = c(0, 40),
        las = 2)
par(mar = c(7, 4.1, 4.1, 2.1))

conditions <- table(MBDSproject$condition)
barplot(conditions[order(conditions, decreasing = T)], 
        names.arg = c("Depleted_Gain", "Non-depleted_Gain", "Depleted_Loss", "Non-depleted_Loss"),
        ylab = "Number of Obs.",
        xlab = "Treatment Groups",
        ylim = c(0, 40),
        las = 1)

boxplot(MBDSproject$actual[sex == "1"], 
        MBDSproject$actual[sex == "0"], 
        names = c("Males", "Females"),
        xlab = "Participants",
        ylab = "Number of matrices actually solved",
        main = "Performance on Matrices Solving Task by Gender")
t.test(MBDSproject$actual[sex == "1"], MBDSproject$actual[sex == "0"])
### t(89.07) = 1.0687, p = 0.2881
### Men successfully solved 4.06 matrices on average, while women solved 3.63; not stat sig.

boxplot(MBDSproject$actual[uni_year == "Freshman"],
        MBDSproject$actual[uni_year == "Sophomore"],
        MBDSproject$actual[uni_year == "Junior"],
        MBDSproject$actual[uni_year == "Senior"],
        MBDSproject$actual[uni_year == "Other"],
        names = c("Freshmen", "Sophomores", "Juniors", "Seniors", "Others (e.g. Graduate)"),
        xlab = "Participants",
        ylab = "Number of matrices actually solved",
        main = "Performance on Matrices Solving Task by Year in College")
summary(aov(actual ~ uni_year, data = MBDSproject))
F(4, 127) = 0.576, p = 0.6810
### No stat. sig. diff in matrices solving abilities between grade levels.

boxplot(MBDSproject$actual[major == "Business"],
        MBDSproject$actual[major == "Engineering"],
        MBDSproject$actual[major == "Humanities"],
        MBDSproject$actual[major == "Math"],
        MBDSproject$actual[major == "Science"],
        MBDSproject$actual[major == "Nursing"],
        MBDSproject$actual[major == "Not listed"],
        MBDSproject$actual[major == "Undecided"],
        names = c("Business", "Engineering", "Humanities", "Math", "Science", "Nursing", "Not listed", "Undecided"),
        ylab = "Number of matrices actually solved",
        main = "Performance on Matrices Solving Task by Academic Major Area")

lm(actual ~ age) # Age not a significant predictor of matrices performance.

summary(lm(cheated ~ day))
summary(lm(cheated ~ time_period))
summary(lm(cheated ~ day*time_period))
### No stat. sig. day-of-week, time-of-day, or time/day interaction effects.

summary(aov(actual ~ major, data = MBDSproject))
lm(actual ~ major, data = MBDSproject)

library(xtable)
data(MBDSproject)
age_aov <- aov(MBDSproject$age ~ MBDSproject$condition)
age_aov_table <- xtable(age_aov)
print(age_aov_table, type = "html")


par(mar = c(5,7,4,2) + 0.1)
barplot(majors[order(majors, decreasing = F)], 
        names.arg = c("Math", "Nursing", "Not listed", "Undecided", "Business", "Engineering", "Science", "Humanities"),
        xlab = "Number of Obs.",
        col = c("wheat", "honeydew3", "lightpink", "plum2", "peachpuff1", "palegreen3", "rosybrown1", "cadetblue"), 
        main = "Participants' Academic Major Area (n = 132)", 
        horiz = T, las = 1, xlim = c(0,40))


##**For Fun**
###**Of the ten matrices, which was the hardest for participants to actually solve?**
####**Answer: <font color="blue";>MATRIX _**</font>
<font size = 3><p>
  
  
  ###**Did men or women do better actually solving matrices?**
  ####**Answer: <font color="blue";>NO REAL DIFFERENCE**</font>
  ```{r echo = FALSE}
boxplot(MBDSproject$actual[sex == "1"], 
        MBDSproject$actual[sex == "0"], 
        names = c("Males", "Females"),
        ylab = "Number of Matrices Actually Solved",
        main = "Performance on Matrices Solving Task by Gender",
        las = 1,
        ylim = c(0,10))
t.test(MBDSproject$actual[sex == "1"], MBDSproject$actual[sex == "0"])
```
<font size = 3><p>While it is true that on average men successfully solved more matrices than women, (M = 4.06 versus M = 3.63), the t-test output above demonstrates that there is no statistically significant difference in these averages; t(89.07) = 1.0687, p = 0.2881</p></font>
  
barplot(mean(BDSproject$cheated[MBDSproject$depleted == "1"]),
        mean(MBDSproject$cheated[MBDSproject$depleted == "0"]))
##############################################################################################################################
##############################################################################################################################
###BAR PLOT OF FREQUENCY OF CHEATING ACROSS ALL FOUR TREATMENT CONDITIONS 
###https://stats.stackexchange.com/questions/190223/how-to-visualize-independent-two-sample-t-test
mean(MBDSproject$cheated[MBDSproject$condition == "DL"])*100
mean(MBDSproject$cheated[MBDSproject$condition == "DG"])
mean(MBDSproject$cheated[MBDSproject$condition == "NL"])
mean(MBDSproject$cheated[MBDSproject$condition == "NG"])

means <- c(mean(MBDSproject$cheated[MBDSproject$condition == "DL"])*100,
           mean(MBDSproject$cheated[MBDSproject$condition == "DG"])*100,
           mean(MBDSproject$cheated[MBDSproject$condition == "NL"])*100,
           mean(MBDSproject$cheated[MBDSproject$condition == "NG"])*100)

names(means) <- c("DL", "DG", "NL", "NG")
se <- c(sd(MBDSproject$cheated[MBDSproject$condition == "DL"])*100/sqrt(length(MBDSproject$cheated[MBDSproject$condition == "DL"])),
        sd(MBDSproject$cheated[MBDSproject$condition == "DG"])*100/sqrt(length(MBDSproject$cheated[MBDSproject$condition == "DG"])),
        sd(MBDSproject$cheated[MBDSproject$condition == "NL"])*100/sqrt(length(MBDSproject$cheated[MBDSproject$condition == "NL"])),
        sd(MBDSproject$cheated[MBDSproject$condition == "NG"])*100/sqrt(length(MBDSproject$cheated[MBDSproject$condition == "NG"])))


bp <- barplot(means, ylim = c(0,25), las = 1, 
              main = "Frequency of Cheating Across All Treatment Groups", 
              ylab = "% of Participants Who Cheated",
              names.arg = c("Depleted_Loss", "Depleted_Gain", "Non-depleted_Loss", "Non-depleted_Gain"), 
              col = c("royalblue4", "slategray1", "deepskyblue4", "deepskyblue"),)
arrows(x0=bp, y0=means-se, y1=means+se, code=3, angle=90)
text(x = bp, y = means, label = c("16%", "10%", "14%", "12%"), pos = 3, cex = 1)
par(mai=c(1, 1, 1, 1))
##############################################################################################################################
##############################################################################################################################
###BAR PLOT OF MAGNITUDE OF CHEATING ACROSS ALL FOUR TREATMENT CONDITIONS          
mean(MBDSproject$amt_cheat[MBDSproject$condition == "DL"], na.rm = TRUE)
mean(MBDSproject$amt_cheat[MBDSproject$condition == "DG"], na.rm = TRUE)
mean(MBDSproject$amt_cheat[MBDSproject$condition == "NL"], na.rm = TRUE)
mean(MBDSproject$amt_cheat[MBDSproject$condition == "NG"], na.rm = TRUE)

meanmags <- c(2, 1.25, 6.5, 2)

sd(MBDSproject$amt_cheat[MBDSproject$condition == "DL"], na.rm = TRUE) # 1.732051
sd(MBDSproject$amt_cheat[MBDSproject$condition == "DG"], na.rm = TRUE) # 0.50
sd(MBDSproject$amt_cheat[MBDSproject$condition == "NL"], na.rm = TRUE) # 2.886751
sd(MBDSproject$amt_cheat[MBDSproject$condition == "NG"], na.rm = TRUE) # 2.0

sqrt(length(MBDSproject$amt_cheat[MBDSproject$condition == "DL"])) # = sqrt(5)
sqrt(length(MBDSproject$amt_cheat[MBDSproject$condition == "DG"])) # = sqrt(4)
sqrt(length(MBDSproject$amt_cheat[MBDSproject$condition == "NL"])) # = sqrt(4)
sqrt(length(MBDSproject$amt_cheat[MBDSproject$condition == "NG"])) # = sqrt(4)

semags <- c(1.732051/sqrt(5), 0.5/sqrt(4), 2.886751/sqrt(4), 2/sqrt(4))

bpmags <- barplot(meanmags, ylim = c(0,10), las = 1, 
                  main = "Magnitude of Cheating Across Treatment Groups", 
                  ylab = "Number of Matrices Over-Reported",
                  names.arg = c("Depleted_Loss", "Depleted_Gain", "Non-depleted_Loss", "Non-depleted_Gain"),
                  col = c("royalblue4", "slategray1", "deepskyblue4", "deepskyblue"))
arrows(x0=bpmags, y0=meanmags-semags, y1=meanmags+semags, code=3, angle=90)
text(x = bp, y = meanmags, label = c("2.00", "1.25", "6.50", "2.00"), pos = 3, cex = 1)
##############################################################################################################################
##############################################################################################################################
###BAR PLOT OF FREQUENCY OF CHEATING ACROSS DEPLETION TREATMENT CONDITIONS
mean(MBDSproject$cheated[MBDSproject$depleted == "1"]) # 0.1285714
mean(MBDSproject$cheated[MBDSproject$depleted == "0"]) # 0.1290323

mean_depleted_cheat <- c(mean(MBDSproject$cheated[MBDSproject$depleted == "1"]), mean(MBDSproject$cheated[MBDSproject$depleted == "0"]))
se_depleted_cheat <- c(sd(MBDSproject$cheated[MBDSproject$depleted == "1"])/sqrt(length(MBDSproject$cheated[MBDSproject$depleted == "1"])),
                       sd(MBDSproject$cheated[MBDSproject$depleted == "0"])/sqrt(length(MBDSproject$cheated[MBDSproject$depleted == "0"])))

bp_mean_depleted_cheat <- barplot(mean_depleted_cheat, ylim = c(0, 0.20), las = 1, 
                                  main = "Frequency of Cheating Across Depletion Condition", 
                                  ylab = "Frequecy of Cheating",
                                  names.arg = c("Depleted", "Non-depleted"), col = c("mistyrose2", "ivory"))
arrows(x0=bp_mean_depleted_cheat, y0=mean_depleted_cheat-se_depleted_cheat, y1=mean_depleted_cheat+se_depleted_cheat, code=3, angle=90)
text(x = bp_mean_depleted_cheat, y = mean_depleted_cheat, label = c("0.1286", "0.1290"), pos = 3, cex = 1)
##############################################################################################################################
##############################################################################################################################
###BAR PLOT OF MAGNITUDE OF CHEATING ACROSS DEPLETION TREATMENT CONDITIONS
mean(MBDSproject$amt_cheat[MBDSproject$depleted == "1"], na.rm = TRUE) # 1.666667
mean(MBDSproject$amt_cheat[MBDSproject$depleted == "0"], na.rm = TRUE) # 4.25

mean_mags_depletion <- c(mean(MBDSproject$amt_cheat[MBDSproject$depleted == "1"], na.rm = TRUE), mean(MBDSproject$amt_cheat[MBDSproject$depleted == "0"], na.rm = TRUE))

sd(MBDSproject$amt_cheat[MBDSproject$depleted == "1"], na.rm = TRUE) # 1.322876
sd(MBDSproject$amt_cheat[MBDSproject$depleted == "0"], na.rm = TRUE) # 3.327376

na.omit(MBDSproject$amt_cheat[MBDSproject$depleted == "1"])
# length(MBDSproject$amt_cheat[MBDSproject$depleted == "1"]) = 9
na.omit(MBDSproject$amt_cheat[MBDSproject$depleted == "0"])
# length(MBDSproject$amt_cheat[MBDSproject$depleted == "0"]) = 8

# sqrt(length(MBDSproject$amt_cheat[MBDSproject$depleted == "1"])) = sqrt(9)
# sqrt(length(MBDSproject$amt_cheat[MBDSproject$depleted == "1"])) = sqrt(8)

se_mags_depletion <- c(1.322876/sqrt(9), 3.327376/sqrt(8))

bp_mags_depletion <- barplot(mean_mags_depletion, 
                             ylim = c(0,10), 
                             las = 1, col = c("mistyrose2", "ivory"),
                             main = "Magnitude of Cheating Across Depletion Treatment Groups (n = 132)", 
                             ylab = "Number of Matrices Over-Reported",
                             names.arg = c("Depletion", "Non-depletion"))
arrows(x0=bp_mags_depletion, y0=mean_mags_depletion-se_mags_depletion, y1=mean_mags_depletion+se_mags_depletion, code=3, angle=90)
text(x = bp_mags_depletion, y = mean_mags_depletion, label = c("1.67", "4.25"), pos = 3, cex = 1)
##############################################################################################################################
##############################################################################################################################
###BAR PLOT OF FREQUENCY OF CHEATING ACROSS FRAMING TREATMENT CONDITIONS
mean(MBDSproject$cheated[MBDSproject$gain == "0"]) # 0.1525424
mean(MBDSproject$cheated[MBDSproject$gain == "1"]) # 0.109589

mean_framing_cheat <- c(mean(MBDSproject$cheated[MBDSproject$gain == "0"]), mean(MBDSproject$cheated[MBDSproject$gain == "1"]))
se_framing_cheat <- c(sd(MBDSproject$cheated[MBDSproject$gain == "0"])/sqrt(length(MBDSproject$cheated[MBDSproject$gain == "0"])),
                       sd(MBDSproject$cheated[MBDSproject$gain == "1"])/sqrt(length(MBDSproject$cheated[MBDSproject$gain == "1"])))

bp_mean_framing_cheat <- barplot(mean_framing_cheat, ylim = c(0, 0.20), las = 1, col = c("salmon", "khaki"), 
                                  main = "Frequency of Cheating Across Framing Condition", 
                                  ylab = "Frequecy of Cheating",
                                  names.arg = c("Loss Framing", "Gain Framing"))
arrows(x0=bp_mean_framing_cheat, y0=mean_framing_cheat-se_framing_cheat, y1=mean_framing_cheat+se_framing_cheat, code=3, angle=90)
text(x = bp_mean_framing_cheat, y = mean_framing_cheat, label = c("0.15", "0.11"), pos = 3, cex = 1)
##############################################################################################################################
##############################################################################################################################
###BAR PLOT OF MAGNITUDE OF CHEATING ACROSS FRAMING TREATMENT CONDITIONS
mean(MBDSproject$amt_cheat[MBDSproject$gain == "0"], na.rm = TRUE) # 4.0
mean(MBDSproject$amt_cheat[MBDSproject$gain == "1"], na.rm = TRUE) # 1.625

mean_mags_framing <- c(mean(MBDSproject$amt_cheat[MBDSproject$gain == "0"], na.rm = TRUE), mean(MBDSproject$amt_cheat[MBDSproject$gain == "1"], na.rm = TRUE))

na.omit(MBDSproject$amt_cheat[MBDSproject$gain == "0"])
# length(MBDSproject$amt_cheat[MBDSproject$gain == "0"]) = 9
na.omit(MBDSproject$amt_cheat[MBDSproject$gain == "1"])
# length(MBDSproject$amt_cheat[MBDSproject$gain == "1"]) = 8

# sqrt(length(MBDSproject$amt_cheat[MBDSproject$gain == "0"])) = sqrt(9)
# sqrt(length(MBDSproject$amt_cheat[MBDSproject$gain == "1"])) = sqrt(8)

se_mags_framing <- c(4.0/sqrt(9), 1.625/sqrt(8))

bp_mags_framing <- barplot(mean_mags_framing, 
                             ylim = c(0,8), 
                             las = 1, col = c("salmon", "khaki"),
                             main = "Magnitude of Cheating Across Framing Conditions", 
                             ylab = "Number of Matrices Over-Reported",
                             names.arg = c("Loss Framing", "Gain Framing"))
arrows(x0=bp_mags_framing, y0=mean_mags_framing-se_mags_framing, y1=mean_mags_framing+se_mags_framing, code=3, angle=90)
text(x = bp_mags_framing, y = mean_mags_framing, label = c("4.00", "1.63"), pos = 3, cex = 1)


wilcox.test(MBDSproject$cheated[MBDSproject$depleted == "1"], 
            MBDSproject$cheated[MBDSproject$depleted == "0"], 
            alternative = "greater", 
            conf.int = T)
wilcox.test(MBDSproject$amt_cheat[MBDSproject$depleted == "1"], 
            MBDSproject$amt_cheat[MBDSproject$depleted == "0"], 
            alternative = "greater", 
            conf.int = T)

wilcox.test(MBDSproject$cheated[MBDSproject$gain == "0"], 
            MBDSproject$cheated[MBDSproject$gain == "1"], 
            alternative = "greater", 
            conf.int = T)
wilcox.test(MBDSproject$amt_cheat[MBDSproject$gain == "0"], 
            MBDSproject$amt_cheat[MBDSproject$gain == "1"], 
            alternative = "greater", 
            conf.int = T)

kruskal.test(cheated ~ condition)
kruskal.test(amt_cheat ~ condition)
kruskalmc(amt_cheat ~ condition)



###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
<div>&nbsp;</div>
  <h3 align = justify>Irrespective of framing, was the effect of self-control depletion on whether or not someone cheated significant, and did self-control depletion have a positive effect on cheating (i.e. negative effect on honesty)?</h3>
  ####**Answer: <font color="red";>NO**</font>
  <font size = 3><p align = justify>At the 95% confidence level, we find that not only was the effect of self-control depletion on cheating behavior negative, but that this effect was also highly statistically insignificant ($\beta$ = -0.0041, p = 0.9940).</p> 
  $logit(cheat) =  -1.91 - 0.0041(depleted)$
  ```{r echo= FALSE, message= FALSE, warning = FALSE}
summary(glm(cheated ~ depleted, family = binomial, data = MBDSproject))
```
<div>&nbsp;</div>
  
  <h3 align = justify>Irrespective of framing, was the effect of self-control depletion on the amount someone cheated significant, and if so did it have a positive effect on how much someone cheated (i.e. positive effect on how dishonest someone was)?</h3>
  ####**Answer: <font color="red";>NO**</font>
  <font size = 3><p align = justify>While the effect of self-control depletion is statistically significant at the 95% confidence level ($\beta$ = -2.58, p = 0.0480), we find that such an effect was in the opposite direction from what we hypothesized. Essentially, non-depleted participants cheated to a greater extent---about 2.5 matrices more, on average---than depleted participants.</p></font>
  <div>&nbsp;</div>
  <center>
  ```{r echo = FALSE, message = FALSE}
library(sjPlot)
tab_model(lm(amt_cheat ~ depleted, data = MBDSproject), pred.labels = c("(Intercept)", "Depleted"), dv.labels = "Amount Cheated", show.stat = TRUE, show.r2 = TRUE, digits.p = 4, string.stat = "t-stat.", string.p = "p-value", show.ci = FALSE)
```
</center>
  
  <h3 align = justify>Irrespective of whether or not a participant was depleted of self-control, was the effect of framing on whether or not someone cheated significant, and if so, did loss framing lead to more instances of cheating than gain framing?</h3>
  ####**Answer: <font color="red";>NO**</font>
  <font size = 3><p align = justify>While we do find that gain framing led people to cheat less often than loss framing, at the 95% confidence level we find this effect to be statistically insignificant ($\beta$ = -0.0430, p = 0.4677).</p></font>  
  ```{r echo = FALSE}
summary(glm(cheated ~ gain, data = MBDSproject))
```
<div>&nbsp;</div>
  
  <h3 align = justify>Irrespective of whether or not a participant was depleted of self control, was the effect of framing on the amount someone cheated significant, and if so did loss framing lead to greater magnitudes of cheating than gain framing?</h3>
  ####**Answer: <font color="red";>NO**</font>
  <font size = 3><p align = justify>While we do find that gain framing led people to cheat to a lesser degree than loss framing, at the 95% confidence level we find this effect to be statistically insignificant ($\beta$ = -2.3750, p = 0.0723).</p></font>  
  <center>
  
  ```{r echo = FALSE}
tab_model(lm(amt_cheat ~ gain, data = MBDSproject), pred.labels = c("(Intercept)", "Gain Framing"), dv.labels = "Amount Cheated", show.stat = TRUE, show.r2 = TRUE, digits.p = 4, string.stat = "t-stat.", string.p = "p-value", show.ci = FALSE)
```
</center>
  <div>&nbsp;</div>
  
  <h3 align = justify>Is there a statistically significant interaction effect between self-control depletion and framing on whether or not a participant cheated?</h3>
  ####**Answer: <font color="red";>NO**</font>
  <font size = 3><p align = justify>At the 95% confidence level we find that while the effects of self-control depletion and gain framing on cheating are in the directions we would expect them to be, positive for the former ($\beta$ = 0.1431) and negative for the latter ($\beta$ = -0.2231), such effects along with their interaction ($\beta$ = -0.2973) are all statistically insignificant.</p></font> 
  ```{r echo =FALSE}
summary(glm(cheated ~ depleted*gain, data = MBDSproject, family = binomial))
```
<div>&nbsp;</div>
  
  ```{r echo = FALSE}
my_aov <- aov(amt_cheat ~ condition, data = MBDSproject)
summary(my_aov)
plot(TukeyHSD(my_aov), las = 1)
```
<div>&nbsp;</div>
  <h3 align = justify>Is there a statisically significant interaction effect between self-control depletion and framing on the magnitude of cheating demonstrated by participants?</h3>
  ####**Answer: <font color="red";>NO**</font>
  <font size = 3><p align = justify>While the effect of depletion (F = 7.386, p = 0.01760) and framing (F = 7.042, p = 0.0199) on the magnitude of cheating are each statistically significant at the 95% confidence level, that of their interaction is not (F = 3.868, 0.0709).</p></font>
  ```{r echo = FALSE}
summary(aov(MBDSproject$amt_cheat ~ MBDSproject$depleted*MBDSproject$gain))

<div>&nbsp;</div>
  
var =   
pDL <- 5/31
pDG <- 4/39
pNL <- 4/28
pNG <- 4/34

varDL <- (pDL/(1 - pDL))/31
varDG <- (pDG/(1 - pDG))/39
varNL <- (pNL/(1 - pNL))/28
varNG <- (pNG/(1 - pNG))/34

sdDL <- sqrt(varDL)
sdDG <- sqrt(varDG)
sdNL <- sqrt(varNL)
sdNG <- sqrt(varNG)

means <- c(mean(MBDSproject$cheated[MBDSproject$condition == "DL"])*100,
           mean(MBDSproject$cheated[MBDSproject$condition == "DG"])*100,
           mean(MBDSproject$cheated[MBDSproject$condition == "NL"])*100,
           mean(MBDSproject$cheated[MBDSproject$condition == "NG"])*100)

semeans <- c(sdDL*100, sdDG*100, sdNL*100, sdNG*100)
bp <- barplot(means, ylim = c(0,25), las = 1, 
              main = "Frequency of Cheating Across All Treatment Groups (n = 132)", 
              ylab = "% of Participants Who Cheated",
              names.arg = c("Depleted_Loss", "Depleted_Gain", "Non-depleted_Loss", "Non-depleted_Gain"), 
              col = c("royalblue4", "slategray1", "deepskyblue4", "deepskyblue"),)
arrows(x0=bp, y0=means-semeans, y1=means+semeans, code=3, angle=90)
text(x = bp, y = means, label = c("16%", "10%", "14%", "12%"), pos = 3, cex = 1)

pDepletion <- (5+4)/(31+39)
pNonDepletion <- (4+4)/(28+34)
varDepletion <- (pDepletion/(1 - pDepletion))/70
varNonDepletion <- (pNonDepletion/(1 - pNonDepletion))/62

freq_depleted_cheat <- c(mean(MBDSproject$cheated[MBDSproject$depleted == "1"])*100, 
                         mean(MBDSproject$cheated[MBDSproject$depleted == "0"])*100)
seDepletion <- sqrt(varDepletion)
seNonDepletion <- sqrt(varNonDepletion)

se_depletion_cheat <- c(seDepletion*100, seNonDepletion*100)

bp_freq_depleted_cheat <- barplot(freq_depleted_cheat, ylim = c(0, 20), las = 1, 
                                  main = "Frequency of Cheating Across Depletion Condition (n = 132)", 
                                  ylab = "% of Participants Who Cheated",
                                  names.arg = c("Depleted", "Non-depleted"), col = c("mistyrose2", "ivory"))
arrows(x0=bp_freq_depleted_cheat, y0=freq_depleted_cheat-se_depletion_cheat, y1=freq_depleted_cheat+se_depletion_cheat, code=3, angle=90)
text(x = bp_freq_depleted_cheat, y = freq_depleted_cheat, label = c("12.86%", "12.90%"), pos = 3, cex = 1)



pLossFrame <- (5+4)/(31+28)
varLossFrame <- (pLossFrame/(1 - pLossFrame))/59
seLossFrame <- sqrt(varLossFrame)
pGainFrame <- (4+4)/(39+34)
varGainFrame <- (pGainFrame/(1 - pGainFrame))/73
seGainFrame <- sqrt(varGainFrame)

se_framing_cheat <- c(seLossFrame*100, seGainFrame*100)
mean_framing_cheat <- c(mean(MBDSproject$cheated[MBDSproject$gain == "0"])*100, 
                        mean(MBDSproject$cheated[MBDSproject$gain == "1"])*100)


bp_mean_framing_cheat <- barplot(mean_framing_cheat, ylim = c(0, 25), las = 1, col = c("salmon", "darkolivegreen3"), 
                                 main = "Frequency of Cheating Across Framing Conditions (n = 132)", 
                                 ylab = "% of Participants Who Cheated",
                                 names.arg = c("Loss Framing", "Gain Framing"))
arrows(x0=bp_mean_framing_cheat, y0=mean_framing_cheat-se_framing_cheat, y1=mean_framing_cheat+se_framing_cheat, code=3, angle=90)
text(x = bp_mean_framing_cheat, y = mean_framing_cheat, label = c("15.25%", "10.96%"), pos = 3, cex = 1)

