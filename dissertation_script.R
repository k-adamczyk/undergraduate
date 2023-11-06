#base sweep descriptive stats
setwd("~/Desktop/dissertation")
#take 1 sweep! use 30smt and if missing then 26, other bcs education - what variables have they used
library(haven)
#insert ready only child dataset
onlychild <- read_dta('usethisonly.dta')

library(dplyr)
sibshipsize <- select(onlychild, bcsid, onlychild, num_siblings, 
                      a0005a, a0248, first_birth, a0255, a0009, a0010,
                      a0012, a0014, a0018, k036, k037)

sibshipsize <- sibshipsize %>% rename(delivery_age = a0005a)
sibshipsize <- sibshipsize %>% rename(mum_age_left_ft=a0009)
sibshipsize <- sibshipsize %>% rename(dad_age_left_ft=a0010)
sibshipsize <- sibshipsize %>% rename(marital_stat=a0012)
sibshipsize <- sibshipsize %>% rename(dad_sc=a0014)                       
sibshipsize <- sibshipsize %>% rename(mum_sc=a0018)  
sibshipsize <- sibshipsize %>% rename(math_score_at10=k036)  
sibshipsize <- sibshipsize %>% rename(read_score_at10=k037)  

sibshipsize$mum_age_left_ft[sibshipsize$mum_age_left_ft<=0] <- NA
sibshipsize$mum_age_left_ft[sibshipsize$mum_age_left_ft==97] <- NA
sibshipsize$dad_age_left_ft[sibshipsize$dad_age_left_ft<=0] <- NA
sibshipsize$dad_age_left_ft[sibshipsize$dad_age_left_ft==97] <- NA
sibshipsize$marital_stat[sibshipsize$marital_stat<0] <- NA
sibshipsize$math_score_at10[sibshipsize$math_score_at10<0] <- NA
sibshipsize$read_score_at10[sibshipsize$read_score_at10<0] <- NA

library(labelled)

sibshipsize$marital_stat_nr <- ifelse(sibshipsize$marital_stat == 2, 1, 0)
sibshipsize$dad_sc[sibshipsize$dad_sc<0] <- NA
sibshipsize$dad_sc[sibshipsize$dad_sc>6] <- NA
sibshipsize$mum_sc[sibshipsize$mum_sc<0] <- NA
#7 means housewives but what does that even mean
sibshipsize$mum_sc[sibshipsize$mum_sc>5] <- NA

#mediators: right now 1 means well and 2 not so well, change 2 to 0 so its more indicative
sibshipsize$math_score_at10 <- ifelse(sibshipsize$math_score_at10==2, 0, sibshipsize$math_score_at10)
sibshipsize$read_score_at10 <- ifelse(sibshipsize$read_score_at10==2, 0, sibshipsize$read_score_at10)

sibshipsize$num_siblings_cat <- ifelse(sibshipsize$num_siblings>2, '2+', sibshipsize$num_siblings)

#explore missingness on the basis of read and math scores and whether inf on only child is missing
#as this 25% where it is will be ommitted

sibshipsize$R <- ifelse(is.na(sibshipsize$onlychild), 1, 0)

summary(sibshipsize$math_score_at10[sibshipsize$R==1])
#3590 NAs, 1063 answers

summary(sibshipsize$R==1)
#4653 in total

summary(sibshipsize$read_score_at10[sibshipsize$R==1])
#3584 NAs, 1069 answers

t.test((sibshipsize$math_score_at10[sibshipsize$R==1]), (sibshipsize$math_score_at10[sibshipsize$R==0]))
#3.311e-06
#as p-value is statistically significant we cannot reject the null hypothesis that 
#there is a difference in means. In particular, respondents that will be analysed,
#the ones on whom we have information whether they are only children or not, have 
#on average statistically significantly higher math score at 10

t.test((sibshipsize$read_score_at10[sibshipsize$R==1]), (sibshipsize$read_score_at10[sibshipsize$R==0]))
#0.2715
#p-value is not statistically significant, thus we can reject the null hypothesis
#that the true difference in means is not equal to 0, i.e. differences in reading scores
#at 10 are not statistically significantly different between individuals that will be
#analysed - information on whether they are only children or not is available,
#and those that are not analysed - where this information wasn't available


complete_only <- sibshipsize[complete.cases(sibshipsize$onlychild), ]
complete_only <- complete_only %>% rename(sex = a0255)

table(complete_only$num_siblings_cat)

cbind(table(complete_only$num_siblings_cat), 
      round(prop.table(table(complete_only$num_siblings_cat)) * 100, 2))

complete_only$sex[complete_only$sex<0] <- NA
summary(complete_only$sex)
table(complete_only$sex)

cbind(table(complete_only$sex), 
      round(prop.table(table(complete_only$sex)) * 100, 2))


#descriptives on the confounders
summary(complete_only$dad_age_left_ft)

table(complete_only$marital_stat_nr)
cbind(table(complete_only$marital_stat_nr), 
      round(prop.table(table(complete_only$marital_stat_nr)) * 100, 2))

table(complete_only$dad_sc)
cbind(table(complete_only$dad_sc), 
      round(prop.table(table(complete_only$dad_sc)) * 100, 2))

summary(complete_only$delivery_age)

#abilities in maths/reading: 0 - not so well, 1 - well
table(complete_only$math_score_at10)
cbind(table(complete_only$math_score_at10), 
      round(prop.table(table(complete_only$math_score_at10)) * 100, 2))

table(complete_only$read_score_at10)
cbind(table(complete_only$read_score_at10), 
      round(prop.table(table(complete_only$read_score_at10)) * 100, 2))

#sweep 7
sweep7 <- read_dta('bcs_2004_followup.dta')

sweep7chosen <- select(sweep7, bcsid, bd7hq5, b7lftme2)

sweep7chosen$b7lftme2[sweep7chosen$b7lftme2<0] <- NA
summary(sweep7chosen$b7lftme2)
#only 33 NAs

#rename to highest qual
sweep7chosen <- sweep7chosen %>% rename(highest_qual = bd7hq5)
sweep7chosen <- sweep7chosen %>% rename(age_left_edu = b7lftme2)

sweep7chosen$highest_qual[sweep7chosen$highest_qual<0] <- NA


withsweep7 <- merge(complete_only,sweep7chosen,by="bcsid")

summary(withsweep7$onlychild)

#descriptive statistics
summary(withsweep7$onlychild)
#1040 NAs
#lm omits NAs automatically so no point to delete NAs manually
table(withsweep7$onlychild)
#need to delete NAs so that sex produces values only for those who reposnded whether they r only children or not
complete7 <- withsweep7[complete.cases(withsweep7$onlychild), ]

summary(complete7$age_left_edu)
summary(complete7$highest_qual)

complete7$R <- ifelse(is.na(complete7$highest_qual), 1, 0)
complete7$Redu <- ifelse(is.na(complete7$age_left_edu), 1, 0)

#are units missing at random
t.test((complete7$R[complete7$onlychild==1]), (complete7$R[complete7$onlychild==0]))
#p-value equals 0.8262, thus it is not statistically significant -> therefore, the 
#null hypothesis stating that true difference in means is not equal to 0 is rejected
#units are missing at random in case of whether individuals are only children or not
t.test((complete7$Redu[complete7$onlychild==1]), (complete7$Redu[complete7$onlychild==0]))
#p-value 0.2543

#descriptives run before missing values of dependent variables are deleted
table(complete7$num_siblings_cat)
cbind(table(complete7$num_siblings_cat), 
      round(prop.table(table(complete7$num_siblings_cat)) * 100, 2))

complete7$sex[complete7$sex<0] <- NA
summary(complete7$sex)
table(complete7$sex)
cbind(table(complete7$sex), 
      round(prop.table(table(complete7$sex)) * 100, 2))
summary(complete7$highest_qual)
#no NAs

#descriptives on the confounders

summary(complete7$dad_age_left_ft)
summary(complete7$mum_age_left_ft)
table(complete7$marital_stat_nr)
cbind(table(complete7$marital_stat_nr), 
      round(prop.table(table(complete7$marital_stat_nr)) * 100, 2))

table(complete7$dad_sc)
cbind(table(complete7$dad_sc), 
      round(prop.table(table(complete7$dad_sc)) * 100, 2))

table(complete7$mum_sc)

summary(complete7$delivery_age)

#abilities in maths/reading: 0 - not so well, 1 - well
table(complete7$math_score_at10)
cbind(table(complete7$math_score_at10), 
      round(prop.table(table(complete7$math_score_at10)) * 100, 2))

table(complete7$read_score_at10)
cbind(table(complete7$read_score_at10), 
      round(prop.table(table(complete7$read_score_at10)) * 100, 2))


#----------

table(complete7$highest_qual)
cbind(table(complete7$highest_qual), 
      round(prop.table(table(complete7$highest_qual)) * 100, 2))

summary(complete7$age_left_edu)

barplot(table(complete7$highest_qual))

library(ggplot2)

tab <- table(complete7$highest_qual)

# Create a data frame of the table
df <- data.frame(highest_qual = names(tab), count = as.numeric(tab))

# Create a barplot with ggplot2
ggplot(df, aes(x = highest_qual, y = count)) +
  geom_bar(stat = "identity", fill = "#4c72b0") +
  xlab("Highest Qualification") +
  ylab("Count") +
  ggtitle("Distribution of Highest Qualification") +
  theme_minimal() +
  scale_x_discrete(labels = c("None", "CSE", "GCSE*",'A Level**', 
                              'Degree***',
                              'Higher degree****')) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face='bold'),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = 'black'))

ggsave("hqual_sweep7.png", width = 6, height = 4, dpi = 300)

summary(complete7$age_left_edu)

#deleting missing
complete7 <- complete7[complete.cases(complete7$highest_qual), ]
complete7 <- complete7[complete.cases(complete7$age_left_edu), ]

means_qual <- aggregate(highest_qual ~ num_siblings_cat, data = complete7, mean)

hqual_cat <- ggplot(complete7, aes(x = num_siblings_cat, y = highest_qual)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Highest qualification obtained dependant on different sibship size categories") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  xlab("Number of Siblings") +
  ylab("Highest Qualification") +
  geom_point(data = means_qual, aes(x = num_siblings_cat, y = highest_qual), color = "red", size = 3)

ggsave("hqual_cat.png", hqual_cat, width = 8, height = 6, dpi = 300)

library(ggplot2)

hqual <- ggplot(complete7, aes(x = num_siblings, y = highest_qual)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "glm", color = "red") +
  ggtitle("Highest qualification obtained dependant on number of siblings") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  xlab("Number of Siblings") +
  ylab("Highest Qualification") 

ggsave("hqual.png", hqual, width = 8, height = 6, dpi = 300)

#ANALYSE THOSE
summary(glm(highest_qual ~ onlychild, data=complete7))
#compare to regression with confounders: mum_age_left, dad_age_left_ft, 
#marital_stat, dad_sc, mum_sc 
summary(glm(highest_qual ~ onlychild + mum_age_left_ft + dad_age_left_ft + marital_stat_nr +
              dad_sc + mum_sc + delivery_age, data=complete7))

#not accounting for categories
summary(glm(highest_qual ~ num_siblings, data=complete7))
summary(glm(highest_qual ~ num_siblings + mum_age_left_ft + dad_age_left_ft + marital_stat_nr 
            + dad_sc + mum_sc + delivery_age, data=complete7))

#accounting for categories
summary(glm(highest_qual ~ num_siblings_cat, data=complete7))
exp(0.09962)
exp(-0.16048)
exp(-0.41605)
#AIC: 29680
summary(glm(highest_qual ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
              dad_sc + delivery_age, data=complete7))
exp(-0.0111244)
exp(-0.245494)
exp(-0.424837)
exp(0.130584)
exp(0.101842)
exp(-0.215719)
exp(0.015601)

#AIC: 24489
summary(glm(highest_qual ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
              dad_sc + delivery_age + math_score_at10 + read_score_at10, data=complete7))
#AIC: 20643
exp(-0.009109)
exp(-0.218712)
exp(-0.373973)
exp(0.121410)
exp(0.032678)
exp(-0.197224)
exp(0.017583)
exp(0.441994)
exp(0.334247)

final <- glm(highest_qual ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
               dad_sc + delivery_age + math_score_at10 + read_score_at10, data=complete7)

library(sjPlot)
odds_table <- tab_model(final, show.aic = T, title = "Final model's odds ratios table")

#best AIC in last one

#ROBUSTNESS TEST

library(lmtest)
library(sandwich)

# Test for heteroscedasticity using the Breusch-Pagan test
bptest(glm(highest_qual ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
             dad_sc + delivery_age + math_score_at10 + read_score_at10, data=complete7), studentize=FALSE)

summary(aov(glm(highest_qual ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
                  dad_sc + delivery_age + math_score_at10 + read_score_at10, data=complete7)))

#Cook's distance
model_qual <- glm(highest_qual ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
                    dad_sc + delivery_age + math_score_at10 + read_score_at10, data=complete7)

# Calculate Cook's distance
cooks_dist_qual <- cooks.distance(model_qual)

# Create a Cook's distance plot
qual_cook <- ggplot(data.frame(x = cooks_dist_qual), aes(x = seq_along(x), y = x)) +
  geom_point() +
  geom_hline(yintercept = 4/(nrow(mtcars) - length(model_qual$coefficients) - 1), color = "red", linetype = "dashed") +
  xlab("Observation") +
  ylab("Cook's distance") +
  ggtitle("Cook's Distance Plot")
ggsave("qual_cook.png", qual_cook, width = 8, height = 6, dpi = 300)

#pseudo R^2
null_model_qual <- glm(highest_qual ~ 1, data=complete7)
unadj_model_qual <- glm(highest_qual ~ num_siblings_cat, data=complete7)
adj_model_qual <- glm(highest_qual ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
                        dad_sc + delivery_age, data=complete7)


null_loglik_qual <- logLik(null_model_qual)
unadj_loglik_qual <- logLik(unadj_model_qual)
adj_loglik_qual <- logLik(adj_model_qual)
full_loglik_qual <- logLik(model_qual)

pseudo_1_qual <- 1- (unadj_loglik_qual/null_loglik_qual)
#0.0048
pseudo_2_qual <- 1- (adj_loglik_qual/null_loglik_qual)
#0.1791866
pseudo_r2_qual <- 1 - (full_loglik_qual/null_loglik_qual)
pseudo_r2_qual
#0.3083289

#age left edu

means_edu <- aggregate(age_left_edu ~ num_siblings_cat, data = complete7, mean)

edu_cat <- ggplot(complete7, aes(x = num_siblings_cat, y = age_left_edu)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Length of education dependant on different sibship size categories") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  xlab("Number of Siblings") +
  ylab("Age Left FT Education") +
  geom_point(data = means_edu, aes(x = num_siblings_cat, y = age_left_edu), color = "red", size = 3)

ggsave("edu_cat.png", edu_cat, width = 8, height = 6, dpi = 300)


# Create the scatter plot with regression line and centered title
edu <- ggplot(complete7, aes(x = num_siblings, y = age_left_edu)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Length of education dependant on amount of siblings") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  xlab("Number of Siblings") +
  ylab("Age Left FT Education") 

ggsave("edu.png", edu, width = 8, height = 6, dpi = 300)


summary(lm(age_left_edu ~ onlychild, data=complete7))
#compare to regression with confounders: mum_age_left, dad_age_left_ft, 
#marital_stat, dad_sc, mum_sc, delivery_age, parental_turbulence
summary(lm(age_left_edu ~ onlychild + mum_age_left_ft + dad_age_left_ft + marital_stat_nr + 
             dad_sc + mum_sc + delivery_age, data=complete7))
#confounders: getting rid of the least statistically singificant ones, till all our statistically
#significant on 5% alpha level

summary(lm(age_left_edu ~ num_siblings, data=complete7))
summary(lm(age_left_edu ~ num_siblings + mum_age_left_ft + dad_age_left_ft + 
             mum_sc + dad_sc + delivery_age + marital_stat_nr, data=complete7))

#UNADJUSTED
summary(lm(age_left_edu ~ num_siblings_cat, data=complete7))
#R^2 0.002262, cat2+ stat signiff
#ADJUSTED
summary(lm(age_left_edu ~ num_siblings_cat + mum_age_left_ft + dad_age_left_ft +
             mum_sc + dad_sc + delivery_age + marital_stat_nr, data=complete7))

#checking if there is significance between mum_age_left_ft and dad_age_left_ft
anova((lm(age_left_edu ~ num_siblings_cat + mum_age_left_ft * dad_age_left_ft +
            mum_sc + dad_sc + delivery_age + marital_stat_nr, data=complete7)), 
      (lm(age_left_edu ~ num_siblings_cat + mum_age_left_ft + dad_age_left_ft +
            mum_sc + dad_sc + delivery_age + marital_stat_nr, data=complete7)))
#interaction is highly significant -> should delete mum_age_left_ft from the analysis

anova(lm(age_left_edu ~ num_siblings_cat + dad_age_left_ft +
           mum_sc * dad_sc + delivery_age + marital_stat_nr, data=complete7), 
      (lm(age_left_edu ~ num_siblings_cat + dad_age_left_ft +
            mum_sc + dad_sc + delivery_age + marital_stat_nr, data=complete7)))
#interaction between mum_sc and dad_sc is significant as well

#delete mum_sc and mum_age_left_ft
summary(lm(age_left_edu ~ num_siblings_cat + dad_age_left_ft +
             dad_sc + delivery_age + marital_stat_nr, data=complete7))
#R^2 0.1048, cat2+ stat signiff

edu2 <- lm(age_left_edu ~ num_siblings_cat + dad_age_left_ft +
             dad_sc + delivery_age + marital_stat_nr, data=complete7)

model2edu <- tab_model(final, show.aic = T, title = "2nd model's table")


#ADJUSTED WITH MEDIATORS
summary(lm(age_left_edu ~ num_siblings_cat + dad_age_left_ft +
             dad_sc + delivery_age + marital_stat_nr + math_score_at10 + 
             read_score_at10, data=complete7))

#R^2 0.1064, 2+ stat signiff

#AIC -> choose best model, lower the AIC the better the model
AIC(lm(age_left_edu ~ num_siblings_cat, data=complete7))
#46873.29
AIC(lm(age_left_edu ~ num_siblings_cat + dad_age_left_ft +
         dad_sc + delivery_age + marital_stat_nr, data=complete7))
#39434.96
AIC(lm(age_left_edu ~ num_siblings_cat + dad_age_left_ft +
         dad_sc + delivery_age + marital_stat_nr + math_score_at10 + read_score_at10, data=complete7))
#33584.18

#mediators-adjusted best (confirmed by R^2 and AIC), running anova and bptest on this

#ANOVA
#mediators-adjusted
summary(aov(lm(age_left_edu ~ num_siblings_cat + dad_age_left_ft +
                 dad_sc + delivery_age + marital_stat_nr + math_score_at10 + 
                 read_score_at10, data=complete7)))

#all apart from marital status are stat signiff

#use aiv to compare unadjusted model with adjusted one and adjusted one with 
#mediators-adjusted one
#ROBUSTNESS TEST

library(lmtest)
library(sandwich)

# Test for heteroscedasticity using the Breusch-Pagan test
bptest(lm(age_left_edu ~ num_siblings_cat + dad_age_left_ft + 
            dad_sc + delivery_age + marital_stat_nr + math_score_at10 + 
            read_score_at10, data=complete7), studentize=FALSE)
#p-value -> 5.089e-10

# Load the ggplot2 package
library(ggplot2)

#Cook's distance
# Fit a linear regression model to the mtcars data
age_model <- lm(age_left_edu ~ num_siblings_cat + dad_age_left_ft + 
                  dad_sc + delivery_age + marital_stat_nr + math_score_at10 + 
                  read_score_at10, data=complete7)

# Calculate Cook's distance
cooks_dist <- cooks.distance(age_model)

# Create a Cook's distance plot
age_cook <- ggplot(data.frame(x = cooks_dist), aes(x = seq_along(x), y = x)) +
  geom_point() +
  geom_hline(yintercept = 4/(nrow(mtcars) - length(age_model$coefficients) - 1), color = "red", linetype = "dashed") +
  xlab("Observation") +
  ylab("Cook's distance") +
  ggtitle("Cook's Distance Plot")
ggsave("age_cook.png", age_cook, width = 8, height = 6, dpi = 300)

#sweep 10
sweep10 <- read_dta('bcs_age46_main.dta')
sweep10chosen <- select(sweep10, BCSID, B10INCAMT, B10FINNOW)

sweep10chosen$B10FINNOW[sweep10chosen$B10FINNOW<0] <- NA
summary(sweep10chosen$B10FINNOW)
table(sweep10chosen$B10FINNOW)

#Value = 1.0	Label = Living comfortably     
#Value = 2.0	Label = Doing all right
#Value = 3.0	Label = Just about getting by  
#Value = 4.0	Label = Finding it quite difficult     
#Value = 5.0	Label = Finding it very difficult      


#recode this -> scale 1-5, 5 the best

library(labelled)
sweep10chosen$B10FINNOW <- to_factor(recode(sweep10chosen$B10FINNOW, "1" = "5",
                                            "2" = "4",
                                            "3" = "3",
                                            '4' = '2',
                                            '5' = '1'))
table(sweep10chosen$B10FINNOW)


sweep10chosen$B10INCAMT[sweep10chosen$B10INCAMT<0] <- NA
#Total take home income after tax and deductions: amount
summary(sweep10chosen$B10INCAMT)
#only 672 NAs - DO THIS ONE!!! instead of annual income in sweep 8
table(sweep10chosen$B10INCAMT)

sweep10chosen <- sweep10chosen %>%rename(income=B10INCAMT)
sweep10chosen <- sweep10chosen %>%rename(bcsid=BCSID)
sweep10chosen <- sweep10chosen %>%rename(finsit=B10FINNOW)

withsweep10 <- merge(complete_only,sweep10chosen,by="bcsid")

summary(withsweep10$onlychild)

summary(withsweep10$income)
summary(withsweep10$finsit)

#descriptive statistics
summary(withsweep10$onlychild)
#no missingness here
table(withsweep10$onlychild)
#1 - "Only", 0 - "Sibling"
#567 only children, 6912 with siblings

withsweep10 <- withsweep10[complete.cases(withsweep10$onlychild), ]

withsweep10$R <- ifelse(is.na(withsweep10$income), 1, 0)
withsweep10$Rfin <- ifelse(is.na(withsweep10$income), 1, 0)

#are units missing at random - income
t.test((withsweep10$R[withsweep10$onlychild==1]), (withsweep10$R[withsweep10$onlychild==0]))
#p-value = 0.7354

#are units missing at random - finsit
t.test((withsweep10$Rfin[withsweep10$onlychild==1]), (withsweep10$Rfin[withsweep10$onlychild==0]))
#p-value = 0.7354

summary(withsweep10$income)
#566 NAs, withsweep10 now - 7479

#DESCRIPTIVE STATS
#withsweep10 now - 6913
table(withsweep10$onlychild)

table(withsweep10$num_siblings_cat)
cbind(table(withsweep10$num_siblings_cat), 
      round(prop.table(table(withsweep10$num_siblings_cat)) * 100, 2))


#boys/girls, boy = 1, girl = 2
summary(withsweep10$sex)
table(withsweep10$sex)
withsweep10$sex[withsweep10$sex<0] <- NA
summary(withsweep10$sex)
table(withsweep10$sex)
cbind(table(withsweep10$sex), 
      round(prop.table(table(withsweep10$sex)) * 100, 2))


#descriptives on the confounders
summary(withsweep10$dad_age_left_ft)
summary(withsweep10$mum_age_left_ft)
table(withsweep10$marital_stat_nr)
cbind(table(withsweep10$marital_stat_nr), 
      round(prop.table(table(withsweep10$marital_stat_nr)) * 100, 2))

table(withsweep10$dad_sc)
cbind(table(withsweep10$dad_sc), 
      round(prop.table(table(withsweep10$dad_sc)) * 100, 2))

table(withsweep10$mum_sc)

summary(withsweep10$delivery_age)

#abilities in maths/reading: 0 - not so well, 1 - well
table(withsweep10$math_score_at10)
cbind(table(withsweep10$math_score_at10), 
      round(prop.table(table(withsweep10$math_score_at10)) * 100, 2))

table(withsweep10$read_score_at10)
cbind(table(withsweep10$read_score_at10), 
      round(prop.table(table(withsweep10$read_score_at10)) * 100, 2))

summary(withsweep10$income)

barplot(table(withsweep10$income), col = "steelblue", border = NA, 
        main = "Distribution of income at 46", xlab = "Income", 
        ylab = "Nr of individuals", cex.lab = 1.2, cex.main = 1.5)

summary(withsweep10$income)
summary(withsweep10$finsit)
table(withsweep10$finsit)
cbind(table(withsweep10$finsit), 
      round(prop.table(table(withsweep10$finsit)) * 100, 2))

withsweep10 <- withsweep10[complete.cases(withsweep10$income), ]
withsweep10 <- withsweep10[complete.cases(withsweep10$finsit), ]

withsweep10$log_income <- log(withsweep10$income)

sum(withsweep10$income>100000)
#217 PPL EARN OVER 217
# Create a histogram with bins of width 1000 for income
#hist(withsweep10$income, breaks = seq(0, max(withsweep10$income), by = 1000),
#    xlab = "Income", ylab = "Frequency", main = "Income Distribution")

# Create a histogram with log-transformed values of income
png(file = "hist_income.png", width = 800, height = 600)
hist(withsweep10$log_income, breaks = seq(0, max(withsweep10$log_income) + 1, by = 0.1),
     xlab = "Log(Income)", ylab = "Nr of individuals", main = "Income Distribution (Log Scale)",
     col = "steelblue")
dev.off()

library(ggplot2)

#medians rather than means as it's income so outliers would mess it up a lot
medians_inc <- aggregate(log_income ~ num_siblings_cat, data = withsweep10, median)

income_cat <- ggplot(withsweep10, aes(x = num_siblings_cat, y = log_income)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Income(log) dependant on different sibship size categories") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  xlab("Number of Siblings") +
  ylab("Income(log)") +
  geom_point(data = medians_inc, aes(x = num_siblings_cat, y = log_income), color = "red", size = 3)

ggsave("income_cat.png", income_cat, width = 8, height = 6, dpi = 300)

inc <- ggplot(withsweep10, aes(x = num_siblings, y = log_income)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Income(log) dependant on number of siblings") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  xlab("Number of Siblings") +
  ylab("Income(log") 

ggsave("inc.png", inc, width = 8, height = 6, dpi = 300)

#ANALYSE LINEAR REGRESSIONS, only for num_siblings_cat as it's been proven to be 
#the most indicative one, lm cuz log(income) is continuous

sum(is.na(withsweep10$log_income))
is.numeric(withsweep10$log_income)

sum(is.na(withsweep10$num_siblings_cat))
is.numeric(withsweep10$num_siblings_cat)

#need to convert num_siblings_cat to factor cuz lm treats it as numeric and it's not
#not sure why it worked without converting in previous models
withsweep10$num_siblings_fac <- factor(withsweep10$num_siblings_cat)

summary(lm(income ~ num_siblings_fac, data=withsweep10))

any(withsweep10$income == 0)
#as there are 0s running a regression analysis on log will be impossible
#need to add a small constant value to income
withsweep10$log_income <- log(withsweep10$income + 0.1)

summary(lm(log_income ~ num_siblings_fac, data=withsweep10))
#including all of the confounders at first, will be deleting one by one
summary(lm(log_income ~ num_siblings_cat +  dad_age_left_ft + marital_stat_nr + 
             delivery_age + dad_sc, data=withsweep10))
#with mediators
summary(lm(log_income ~ num_siblings_cat +  dad_age_left_ft + marital_stat_nr + 
             delivery_age + dad_sc + math_score_at10 + read_score_at10, data=withsweep10))

#compare by AICs, by R^2 last model's best fitted
AIC(lm(log_income ~ num_siblings_fac, data=withsweep10))
#30309.19
AIC(lm(log_income ~ num_siblings_cat +  dad_age_left_ft + marital_stat_nr + 
         delivery_age + dad_sc, data=withsweep10))
#25982.52
AIC(lm(log_income ~ num_siblings_cat +  dad_age_left_ft + marital_stat_nr + 
         delivery_age + dad_sc + math_score_at10 + read_score_at10, data=withsweep10))
#22229.32

#AIC confirms last model's best fitted

model <- lm(log_income ~ num_siblings_cat +  dad_age_left_ft + marital_stat_nr + 
              delivery_age + dad_sc + math_score_at10 + read_score_at10, data=withsweep10)

(exp(coef(model)[4]) - 1) * 100
#Having more than 2 siblings is associated with 24.76% decrease in one's income

#ROBUSTNESS TEST

library(lmtest)
library(sandwich)

# Test for heteroscedasticity using the Breusch-Pagan test
bptest(model, studentize=FALSE)
#p-value = 0.00095

summary(aov(model))

# Calculate Cook's distance
cooks_dist_income <- cooks.distance(model)

# Create a Cook's distance plot
income_cook <- ggplot(data.frame(x = cooks_dist_income), aes(x = seq_along(x), y = x)) +
  geom_point() +
  geom_hline(yintercept = 4/(nrow(mtcars) - length(model$coefficients) - 1), color = "red", linetype = "dashed") +
  xlab("Observation") +
  ylab("Cook's distance") +
  ggtitle("Cook's Distance Plot")
ggsave("income_cook.png", income_cook, width = 8, height = 6, dpi = 300)

#FINSIT

library(ggplot2)

withsweep10$finsit_numeric <- as.numeric(as.character(withsweep10$finsit))

means_finsit <- aggregate(finsit_numeric ~ num_siblings_cat, data = withsweep10, mean)

finsit_cat <- ggplot(withsweep10, aes(x = num_siblings_cat, y = finsit_numeric)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Financial satisfaction dependant on different sibship size categories") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  xlab("Number of Siblings") +
  ylab("Financial satisfaction") +
  geom_point(data = means_finsit, aes(x = num_siblings_cat, y = finsit_numeric), color = "red", size = 3)

ggsave("finsit_cat.png", finsit_cat, width = 8, height = 6, dpi = 300)

finsit <- ggplot(withsweep10, aes(x = num_siblings, y = finsit_numeric)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Financial satisfaction dependant on number of siblings") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  xlab("Number of Siblings") +
  ylab("Financial satisfaction") 

ggsave("finsit.png", inc, width = 8, height = 6, dpi = 300)

#ANALYSE LINEAR REGRESSIONS, only for num_siblings_cat as it's been proven to be 
#the most indicative one, logistic cuz finsit is categorical

#ANALYSE THOSE
summary(glm(finsit_numeric ~ onlychild, data=withsweep10))
summary(glm(finsit_numeric ~ onlychild + mum_age_left_ft + dad_age_left_ft + marital_stat_nr + 
              delivery_age + dad_sc + mum_sc, data=withsweep10))

#getting rid of mum_age_left_ft
summary(glm(finsit_numeric ~ onlychild + marital_stat_nr + dad_age_left_ft +
              delivery_age + dad_sc + mum_sc, data=withsweep10))

#getting rid of marital stat
summary(glm(finsit_numeric ~ onlychild + dad_age_left_ft +
              delivery_age + dad_sc + mum_sc, data=withsweep10))

#getting rid of delivery age
summary(glm(finsit_numeric ~ onlychild + dad_age_left_ft +
              dad_sc + mum_sc, data=withsweep10))
#all confounders signiff, only child (IV) not

#not accounting for categories
summary(glm(finsit_numeric ~ num_siblings, data=withsweep10))
summary(glm(finsit_numeric ~ num_siblings + mum_age_left_ft + dad_age_left_ft + marital_stat_nr + 
              delivery_age + dad_sc + mum_sc, data=withsweep10))


#accounting for categories
summary(glm(finsit_numeric ~ num_siblings_cat, data=withsweep10))
#AIC: 18540, cat1 stat signiff
exp(0.08475)
exp(0.03037)
exp(-0.05525)


summary(glm(finsit_numeric ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
              delivery_age + dad_sc, data=withsweep10))
#AIC: 15909, no cats stat signiff
exp(0.056567)
exp(0.010576)
exp(-0.052567)
exp(0.012841)
exp(0.189371)
exp(0.001718)
exp(-0.048327)

summary(glm(finsit_numeric ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
              delivery_age + dad_sc + math_score_at10 + read_score_at10, data=withsweep10))
#AIC: 13408, no cats stat signiff
exp(0.035110)
exp(-0.006300)
exp(-0.071615)
exp(0.012665)
exp(0.162234)
exp(0.002642)
exp(-0.035853)
exp(0.165347)
exp(0.025378)

#best AIC is last model

#ROBUSTNESS TEST

library(lmtest)
library(sandwich)

# Test for heteroscedasticity using the Breusch-Pagan test
bptest(glm(finsit_numeric ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
             delivery_age + dad_sc + math_score_at10 + read_score_at10, data=withsweep10), studentize=FALSE)
#p-value 0.008029

summary(aov(glm(finsit_numeric ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
                  delivery_age + dad_sc + math_score_at10 + read_score_at10, data=withsweep10)))
#all apart from marital status nr, delivery age and read score at 10 are stat signiff

#Cook's distance
model_finsit <- glm(finsit_numeric ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
                      delivery_age + dad_sc + math_score_at10 + read_score_at10, data=withsweep10)

# Calculate Cook's distance
cooks_dist_finsit <- cooks.distance(model_finsit)

# Create a Cook's distance plot
finsit_cook <- ggplot(data.frame(x = cooks_dist_finsit), aes(x = seq_along(x), y = x)) +
  geom_point() +
  geom_hline(yintercept = 4/(nrow(mtcars) - length(model_finsit$coefficients) - 1), color = "red", linetype = "dashed") +
  xlab("Observation") +
  ylab("Cook's distance") +
  ggtitle("Cook's Distance Plot")
ggsave("finsit_cook.png", finsit_cook, width = 8, height = 6, dpi = 300)

#pseudo R^2
null_model_fin <- glm(finsit_numeric ~ 1, data=withsweep10)
unadj_model_fin <- glm(finsit_numeric ~ num_siblings_cat, data=withsweep10)
adj_model_fin <- glm(finsit_numeric ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
                       delivery_age + dad_sc, data=withsweep10)


null_loglik_fin <- logLik(null_model_fin)
unadj_loglik_fin <- logLik(unadj_model_fin)
adj_loglik_fin <- logLik(adj_model_fin)
full_loglik_fin <- logLik(model_finsit)

pseudo_1_fin <- 1- (unadj_loglik_fin/null_loglik_fin)
#0.001050541
pseudo_2_fin <- 1- (adj_loglik_fin/null_loglik_fin)
#0.1432978
pseudo_r2_fin <- 1 - (full_loglik_fin/null_loglik_fin)
#0.2783611

#sweep 5 subset
sweep5 <- read_dta('bcs96x.dta')
sweep5chosen <- select(sweep5, bcsid, b960132, hqual26a, q5a05, b960312, b960259, b960246)
#clean the new variables
#b980132: -8 to NA
sweep5chosen$b960132[sweep5chosen$b960132==-8] <- NA
sweep5chosen$b960312[sweep5chosen$b960312<0] <- NA
summary(sweep5chosen$b960312) #1992 Nas
#sub select only with those who in ft job
table(sweep5chosen$b960259) #2968 NAs, 6035 yes
ftjob <- subset(sweep5chosen, b960259 == 1)
summary(ftjob$b960312)

ftjob <- ftjob %>% rename(net_pay=b960312)  
#join with main
ftjob_merged <- merge(complete_only,ftjob,by="bcsid")

ftjob_merged <- ftjob_merged[complete.cases(ftjob_merged$onlychild), ]

summary(ftjob_merged$onlychild)
summary(ftjob_merged$net_pay)

ftjob_merged$R <- ifelse(is.na(ftjob_merged$net_pay), 1, 0)
#are units missing at random
t.test((ftjob_merged$R[ftjob_merged$onlychild==1]), (ftjob_merged$R[ftjob_merged$onlychild==0]))
#p-value (0.4394) not significant, missingness at random

#descriptives before deleting net pay NAs

table(ftjob_merged$num_siblings_cat)
cbind(table(ftjob_merged$num_siblings_cat), 
      round(prop.table(table(ftjob_merged$num_siblings_cat)) * 100, 2))


summary(ftjob_merged$sex)
table(ftjob_merged$sex)
cbind(table(ftjob_merged$sex), 
      round(prop.table(table(ftjob_merged$sex)) * 100, 2))
#2374 males, 2407 females, 301 NAs
#rename to age_left_ft

summary(ftjob_merged$onlychild)
table(ftjob_merged$onlychild)
#4656 non-only, 426 only

#for descriptive stats table

#descriptives on the confounders
summary(ftjob_merged$dad_age_left_ft)
summary(ftjob_merged$mum_age_left_ft)
table(ftjob_merged$marital_stat_nr)
cbind(table(ftjob_merged$marital_stat_nr), 
      round(prop.table(table(ftjob_merged$marital_stat_nr)) * 100, 2))

table(ftjob_merged$dad_sc)
cbind(table(ftjob_merged$dad_sc), 
      round(prop.table(table(ftjob_merged$dad_sc)) * 100, 2))

table(ftjob_merged$mum_sc)

summary(ftjob_merged$delivery_age)

#abilities in maths/reading: 0 - not so well, 1 - well
table(ftjob_merged$math_score_at10)
cbind(table(ftjob_merged$math_score_at10), 
      round(prop.table(table(ftjob_merged$math_score_at10)) * 100, 2))

table(ftjob_merged$read_score_at10)
cbind(table(ftjob_merged$read_score_at10), 
      round(prop.table(table(ftjob_merged$read_score_at10)) * 100, 2))

summary(ftjob_merged$net_pay)

ftjob_merged <-  ftjob_merged[complete.cases(ftjob_merged$net_pay), ]

ftjob_merged$log_pay <- log(ftjob_merged$net_pay)

png(file = "hist_pay26.png", width = 800, height = 600)
hist(ftjob_merged$log_pay, breaks = seq(0, max(ftjob_merged$log_pay) + 1, by = 0.1),
     xlab = "Log(Pay)", ylab = "Nr of individuals", main = "Net Pay at 26 Distribution (Log Scale)",
     col = "steelblue")
dev.off()

medianspay <- aggregate(log_pay ~ num_siblings_cat, data = ftjob_merged, median)

library(ggplot2)
logpay_cat <- ggplot(ftjob_merged, aes(x = num_siblings_cat, y = log_pay)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Net pay at 26 (log) dependant on different sibship size categories") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  xlab("Number of Siblings") +
  ylab("Net pay at 26 (log)") +
  geom_point(data = medianspay, aes(x = num_siblings_cat, y = log_pay), color = "red", size = 3)

ggsave("logpay_cat.png", logpay_cat, width = 8, height = 6, dpi = 300)


# Create the scatter plot with regression line and centered title
logpay <- ggplot(ftjob_merged, aes(x = num_siblings, y = log_pay)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Net pay at 26 (log) dependant on amount of siblings") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  xlab("Number of Siblings") +
  ylab("Net pay at 26 (log)") 

ggsave("logpay.png", logpay, width = 8, height = 6, dpi = 300)

#ANALYSIS
summary(lm(log_pay ~ num_siblings_cat, data=ftjob_merged))
#including all of the confounders at first, will be deleting one by one
summary(lm(log_pay ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
             delivery_age + dad_sc, data=ftjob_merged))

#mediators-adjusted
summary(lm(log_pay ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
             delivery_age + dad_sc + math_score_at10 + read_score_at10, data=ftjob_merged))

#AIC:
AIC(lm(log_pay ~ num_siblings_cat, data=ftjob_merged))
#16751.64
AIC(lm(log_pay ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
         delivery_age + dad_sc, data=ftjob_merged))
#14358.69
AIC(lm(log_pay ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
         delivery_age + dad_sc + math_score_at10 + read_score_at10, data=ftjob_merged))
#12286.39

#R^2 and AIC confirm that the last one is best, it will be analysed further

modelnet <- lm(log_pay ~ num_siblings_cat + dad_age_left_ft + marital_stat_nr + 
                 delivery_age + dad_sc + math_score_at10 + read_score_at10, data=ftjob_merged)

(exp(coef(modelnet)[4]) - 1) * 100
#-21.20

#ROBUSTNESS TEST

library(lmtest)
library(sandwich)

# Test for heteroscedasticity using the Breusch-Pagan test
bptest(modelnet, studentize=FALSE)
#p-value = 0.001207

summary(aov(modelnet))

#Cook's distance

# Calculate Cook's distance
cooks_dist_net <- cooks.distance(modelnet)

# Create a Cook's distance plot
net_cook <- ggplot(data.frame(x = cooks_dist_net), aes(x = seq_along(x), y = x)) +
  geom_point() +
  geom_hline(yintercept = 4/(nrow(mtcars) - length(modelnet$coefficients) - 1), color = "red", linetype = "dashed") +
  xlab("Observation") +
  ylab("Cook's distance") +
  ggtitle("Cook's Distance Plot")
ggsave("net_cook.png", net_cook, width = 8, height = 6, dpi = 300)
