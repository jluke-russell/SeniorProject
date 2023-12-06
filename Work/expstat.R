# Load libraries
library(tidyverse)
library(mosaic)
library(car)
library(readxl)
library(dplyr)
# Load Data 
main <- read_excel("TeacherPrepPacini_092823.xlsx")
dan <- read_excel("Danielson.xlsx") %>% filter(Course == "ED 492")
disp <- read_excel("Disposition.xlsx")
cert <- read_excel("Certification Column Pacini.xlsx")

# Created Percent Praxis passed variable as percPassed:
main2 <- main %>% mutate(percPassed = PassedPraxisTests / AttemptedPraxisTests * 100)

# Joining data 

result <- full_join(disp, dan, by = "StudentID")

# Turn categorical into numeric for evaluation data 
result2 <- result %>%
  mutate(
    Q5_1 = case_when(Q5_1 == 'Not Observed' ~ 0,
                     Q5_1 == 'Unacceptable' ~ 1,
                     Q5_1 == 'Developing' ~ 2,
                     Q5_1 == 'Proficient' ~ 3,
                     Q5_1 == 'Exemplary' ~ 4),
    
    Q5_2 = case_when(Q5_2 == 'Not Observed' ~ 0,
                     Q5_2 == 'Unacceptable' ~ 1,
                     Q5_2 == 'Developing' ~ 2,
                     Q5_2 == 'Proficient' ~ 3,
                     Q5_2 == 'Exemplary' ~ 4),
    
    Q5_3 = case_when(Q5_3 == 'Not Observed' ~ 0,
                     Q5_3 == 'Unacceptable' ~ 1,
                     Q5_3 == 'Developing' ~ 2,
                     Q5_3 == 'Proficient' ~ 3,
                     Q5_3 == 'Exemplary' ~ 4),
    
    Q5_4 = case_when(Q5_4 == 'Not Observed' ~ 0,
                     Q5_4 == 'Unacceptable' ~ 1,
                     Q5_4 == 'Developing' ~ 2,
                     Q5_4 == 'Proficient' ~ 3,
                     Q5_4 == 'Exemplary' ~ 4),
    
    Q4_1 = case_when(Q4_1 == 'Not Observed' ~ 0,
                     Q4_1 == 'Unacceptable' ~ 1,
                     Q4_1 == 'Developing' ~ 2,
                     Q4_1 == 'Proficient' ~ 3,
                     Q4_1 == 'Exemplary' ~ 4),
    
    Q4_2 = case_when(Q4_2 == 'Not Observed' ~ 0,
                     Q4_2 == 'Unacceptable' ~ 1,
                     Q4_2 == 'Developing' ~ 2,
                     Q4_2 == 'Proficient' ~ 3,
                     Q4_2 == 'Exemplary' ~ 4),
    
    Q4_3 = case_when(Q4_3 == 'Not Observed' ~ 0,
                     Q4_3 == 'Unacceptable' ~ 1,
                     Q4_3 == 'Developing' ~ 2,
                     Q4_3 == 'Proficient' ~ 3,
                     Q4_3 == 'Exemplary' ~ 4),
    
    Q4_4 = case_when(Q4_4 == 'Not Observed' ~ 0,
                     Q4_4 == 'Unacceptable' ~ 1,
                     Q4_4 == 'Developing' ~ 2,
                     Q4_4 == 'Proficient' ~ 3,
                     Q4_4 == 'Exemplary' ~ 4),
    
    Q2.1_1 = case_when(Q2.1_1 == '!-UNRATED-!' ~ 0,
                       Q2.1_1 == 'Unsatisfactory (1)' ~ 1,
                       Q2.1_1 == 'Basic (2)' ~ 2,
                       Q2.1_1 == 'Proficient (3)' ~ 3),
    
    Q2.1_2 = case_when(Q2.1_2 == '!-UNRATED-!' ~ 0,
                       Q2.1_2 == 'Unsatisfactory (1)' ~ 1,
                       Q2.1_2 == 'Basic (2)' ~ 2,
                       Q2.1_2 == 'Proficient (3)' ~ 3),
    
    Q2.1_3 = case_when(Q2.1_3 == '!-UNRATED-!' ~ 0,
                       Q2.1_3 == 'Unsatisfactory (1)' ~ 1,
                       Q2.1_3 == 'Basic (2)' ~ 2,
                       Q2.1_3 == 'Proficient (3)' ~ 3),
    
    Q2.1_4 = case_when(Q2.1_4 == '!-UNRATED-!' ~ 0,
                       Q2.1_4 == 'Unsatisfactory (1)' ~ 1,
                       Q2.1_4 == 'Basic (2)' ~ 2,
                       Q2.1_4 == 'Proficient (3)' ~ 3),
    
    Q2.1_5 = case_when(Q2.1_5 == '!-UNRATED-!' ~ 0,
                       Q2.1_5 == 'Unsatisfactory (1)' ~ 1,
                       Q2.1_5 == 'Basic (2)' ~ 2,
                       Q2.1_5 == 'Proficient (3)' ~ 3),
    
    Q2.1_6 = case_when(Q2.1_6 == '!-UNRATED-!' ~ 0,
                       Q2.1_6 == 'Unsatisfactory (1)' ~ 1,
                       Q2.1_6 == 'Basic (2)' ~ 2,
                       Q2.1_6 == 'Proficient (3)' ~ 3),
    
    Q3.1_1 = case_when(Q3.1_1 == '!-UNRATED-!' ~ 0,
                       Q3.1_1 == 'Unsatisfactory (1)' ~ 1,
                       Q3.1_1 == 'Basic (2)' ~ 2,
                       Q3.1_1 == 'Proficient (3)' ~ 3),
    
    Q3.1_2 = case_when(Q3.1_2 == '!-UNRATED-!' ~ 0,
                       Q3.1_2 == 'Unsatisfactory (1)' ~ 1,
                       Q3.1_2 == 'Basic (2)' ~ 2,
                       Q3.1_2 == 'Proficient (3)' ~ 3),
    
    Q3.1_3 = case_when(Q3.1_3 == '!-UNRATED-!' ~ 0,
                       Q3.1_3 == 'Unsatisfactory (1)' ~ 1,
                       Q3.1_3 == 'Basic (2)' ~ 2,
                       Q3.1_3 == 'Proficient (3)' ~ 3),
    
    Q3.1_4 = case_when(Q3.1_4 == '!-UNRATED-!' ~ 0,
                       Q3.1_4 == 'Unsatisfactory (1)' ~ 1,
                       Q3.1_4 == 'Basic (2)' ~ 2,
                       Q3.1_4 == 'Proficient (3)' ~ 3),
    
    Q3.1_5 = case_when(Q3.1_5 == '!-UNRATED-!' ~ 0,
                       Q3.1_5 == 'Unsatisfactory (1)' ~ 1,
                       Q3.1_5 == 'Basic (2)' ~ 2,
                       Q3.1_5 == 'Proficient (3)' ~ 3),
    
    Q3.4_1 = case_when(Q3.4_1 == '!-UNRATED-!' ~ 0,
                       Q3.4_1 == 'Unsatisfactory (1)' ~ 1,
                       Q3.4_1 == 'Basic (2)' ~ 2,
                       Q3.4_1 == 'Proficient (3)' ~ 3),
    
    Q3.4_2 = case_when(Q3.4_2 == '!-UNRATED-!' ~ 0,
                       Q3.4_2 == 'Unsatisfactory (1)' ~ 1,
                       Q3.4_2 == 'Basic (2)' ~ 2,
                       Q3.4_2 == 'Proficient (3)' ~ 3),
    
    Q3.4_3 = case_when(Q3.4_3 == '!-UNRATED-!' ~ 0,
                       Q3.4_3 == 'Unsatisfactory (1)' ~ 1,
                       Q3.4_3 == 'Basic (2)' ~ 2,
                       Q3.4_3 == 'Proficient (3)' ~ 3),
    
    Q3.4_4 = case_when(Q3.4_4 == '!-UNRATED-!' ~ 0,
                       Q3.4_4 == 'Unsatisfactory (1)' ~ 1,
                       Q3.4_4 == 'Basic (2)' ~ 2,
                       Q3.4_4 == 'Proficient (3)' ~ 3),
    
    Q3.4_5 = case_when(Q3.4_5 == '!-UNRATED-!' ~ 0,
                       Q3.4_5 == 'Unsatisfactory (1)' ~ 1,
                       Q3.4_5 == 'Basic (2)' ~ 2,
                       Q3.4_5 == 'Proficient (3)' ~ 3),
    
    Q4.1_1 = case_when(Q4.1_1 == '!-UNRATED-!' ~ 0,
                       Q4.1_1 == 'Unsatisfactory (1)' ~ 1,
                       Q4.1_1 == 'Basic (2)' ~ 2,
                       Q4.1_1 == 'Proficient (3)' ~ 3),
    
    Q4.1_2 = case_when(Q4.1_2 == '!-UNRATED-!' ~ 0,
                       Q4.1_2 == 'Unsatisfactory (1)' ~ 1,
                       Q4.1_2 == 'Basic (2)' ~ 2,
                       Q4.1_2 == 'Proficient (3)' ~ 3),
    
    Q4.1_3 = case_when(Q4.1_3 == '!-UNRATED-!' ~ 0,
                       Q4.1_3 == 'Unsatisfactory (1)' ~ 1,
                       Q4.1_3 == 'Basic (2)' ~ 2,
                       Q4.1_3 == 'Proficient (3)' ~ 3),
    
    Q4.1_4 = case_when(Q4.1_4 == '!-UNRATED-!' ~ 0,
                       Q4.1_4 == 'Unsatisfactory (1)' ~ 1,
                       Q4.1_4 == 'Basic (2)' ~ 2,
                       Q4.1_4 == 'Proficient (3)' ~ 3),
    
    Q4.1_5 = case_when(Q4.1_5 == '!-UNRATED-!' ~ 0,
                       Q4.1_5 == 'Unsatisfactory (1)' ~ 1,
                       Q4.1_5 == 'Basic (2)' ~ 2,
                       Q4.1_5 == 'Proficient (3)' ~ 3),
    
    Q4.1_6 = case_when(Q4.1_6 == '!-UNRATED-!' ~ 0,
                       Q4.1_6 == 'Unsatisfactory (1)' ~ 1,
                       Q4.1_6 == 'Basic (2)' ~ 2,
                       Q4.1_6 == 'Proficient (3)' ~ 3)
  )

# Create final dataset 

full_data <- full_join(main2, result2, by = "StudentID")

full_data <- full_join(full_data, cert, by = "StudentID")

# Danielson Columns for averaging 
columns_to_average <- c("Q2.1_1", "Q2.1_2", "Q2.1_3", "Q2.1_4", "Q2.1_5", "Q2.1_6",
                        "Q3.1_1", "Q3.1_2", "Q3.1_3", "Q3.1_4", "Q3.1_5",
                        "Q3.4_1", "Q3.4_2", "Q3.4_3", "Q3.4_4", "Q3.4_5",
                        "Q4.1_1", "Q4.1_2", "Q4.1_3", "Q4.1_4", "Q4.1_5", "Q4.1_6")

#DanAVG <- sum(colMeans(full_data[columns_to_average], na.rm = TRUE)) / length(columns_to_average)

# Disposition Columns for averaging 
columns_to_average_disp <- c("Q4_1", "Q4_2", "Q4_3", "Q4_4", "Q5_1", "Q5_2", "Q5_3", "Q5_4")

#DispAaa <- sum(colMeans(full_data[columns_to_average_disp], na.rm = TRUE)) / length(columns_to_average_disp)

# Create row level means for danielson and disposition using rowMeans

DispAVG <- rowMeans(full_data[columns_to_average_disp], na.rm = TRUE)

DanAVG <- rowMeans(full_data[columns_to_average], na.rm = TRUE)

# Add new columns to data set full_data. 
# These columns are already found in PBI file no need to export 
full_data$DispAVG <- DispAVG
full_data$DanAVG <- DanAVG

####################################
# Old Joins 

#yeet <- inner_join(main2, result, by = "StudentID")

#result2 <- full_join(result, disp, by = "StudentID")

#result_no_na <- na.omit(result2)

#join1 <- merge(main2, dan, by='StudentID') %>% filter(Course == "ED 492")

#join2 <- merge(join1, disp, by = "StudentID")
####################################

# Export to a CSV

write.csv(full_data, file = "full_data.csv")

write.csv(result, file = "eval_data.csv")



# evaluators are on spectrum where they sometimes do not rank higher than others 
# like on averages a 2.5 and another 3.5


# look at first attepmts on praxis to mitigate learning bias also cuz standardized test

# some fun ANOVA's  
gaov <- aov(percPassed ~ Gender, data = main2)
anova(gaov)

ethaov <- aov(percPassed ~ Ethnicity, data = main2)
anova(ethaov)

pell <- aov(percPassed ~ as.factor(ReceivedPell), data = main2)
anova(pell)

mission <- aov(percPassed ~ ServedMission, data = main2)
anova(mission)

gmaov <- aov(percPassed ~ Gender*ServedMission, data = main2)
anova(gmaov)

mart <- aov(percPassed ~ TermMaritalStatus, data = main2)
anova(mart)

mart2 <- aov(percPassed ~ TermMaritalStatus*TermAge, data = main2)
anova(mart2)

class <- aov(percPassed ~ TermClassificaionBegin, data = main2)
anova(class)

classa <- aov(percPassed ~ TermClassificaionBegin*TermAge, data = main2)
anova(classa)

major <- aov(percPassed ~ TermMajor, data = main2)
anova(major)

###################

# Danielson is huge 
# Does praxis predict good teachers? 
# Relationships between praxis and danielson and disposition scores 
# Control for GPA 
# Outcomes: Praxis, Final Danielson score, Graduated? Received credentials? use EarnedTeacherPrep...
# Employer survey data, self-report data 
# Check validity on disposition data (run assumption) 
# What is causing people to drop out of the teacher program
###################


# Logistic Regression models

miss <- glm(percPassed >= 70 ~ TermCumGPA + as.factor(ServedMission), data = full_data, family = binomial)
summary(miss) 
b <- coef(miss)
plot(percPassed >= 70 ~ TermCumGPA, data = full_data, col = as.factor(ServedMission), main = "Mission")
curve(exp(b[1]+b[2]*x)/(1+exp(b[1]+b[2]*x)), add = TRUE, lwd = 2) 

marr <- glm(percPassed >= 70 ~ TermCumGPA + as.factor(TermMaritalStatus), data = full_data, family = binomial)
summary(marr) 
b <- coef(marr)
plot(percPassed >= 70 ~ TermCumGPA, data = full_data, pch = 16, col = as.factor(TermMaritalStatus), main = "Married")
curve(exp(b[1]+b[2]*x)/(1+exp(b[1]+b[2]*x)), add = TRUE, lwd = 2) 


tpd <- glm(percPassed >= 70 ~ DispAVG + as.factor(EarnedTeacherPrepDegree), data = full_data, family = binomial)
summary(tpd) 
b <- coef(tpd)
plot(percPassed >= 70 ~ TermCumGPA, data = full_data, pch = 16, col = as.factor(EarnedTeacherPrepDegree), main = "Teacher Prep Deg")
curve(exp(b[1]+b[2]*x)/(1+exp(b[1]+b[2]*x)), add = TRUE, lwd = 2) 


#########################

mymajors <- filter(full_data, TermMajorCode %in% c('990', '850', '980', '815', '799', '795', '836'))


# Top 5 Major Boxplots (by student count)

ggplot(data = mymajors, aes(TermMajor,DispAVG)) + 
  geom_boxplot() +
  labs(title = "Major & Disposition", x = "Student Major", y = "Average Disposition Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) # Center the title
  

ggplot(data = mymajors, aes(TermMajor,DanAVG)) + 
  geom_boxplot() +
  labs(title = "Major & Danielson", x = "Student Major", y = "Average Danielson Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # Center the title

ggplot(data = mymajors, aes(TermMajor,TermCumGPA)) + 
  geom_boxplot() +
  labs(title = "Major & GPA",x = "Student Major", y = "Student GPA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # Center the title

ggplot(data = mymajors, aes(TermMajor,percPassed)) + 
  geom_boxplot() +
  labs(title = "Major & Praxis Tests", x = "Student Major", y = "% Praxis Test Passed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # Center the title

# Demographic Boxplots 

ggplot(data = mymajors, aes(Ethnicity,DispAVG)) + 
  geom_boxplot() +
  labs(title = "Race & Disposition", x = "Race", y = "Average Disposition Rating") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1),
    plot.title = element_text(hjust = 0.5))  # Center the title
  

ggplot(data = mymajors, aes(Ethnicity,DanAVG)) + 
  geom_boxplot() +
  labs(title = "Race & Danielson", x = "Race", y = "Average Danielson Rating") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # Center the title

ggplot(data = mymajors, aes(Ethnicity,TermCumGPA)) + 
  geom_boxplot() +
  labs(title = "Race & GPA", x = "Race", y = "Student GPA") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # Center the title


ggplot(data = mymajors, aes(Ethnicity,percPassed)) + 
  geom_boxplot() +
  labs(title = "Race & Praxis", x = "Race", y = "% Praxis Tests Passed") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # Center the title

# My Major:Ethnicity models 

dispaov <- aov(DispAVG ~ TermMajor*Ethnicity, data = mymajors)
anova(dispaov)

danaov <- aov(DanAVG ~ TermMajor*Ethnicity, data = mymajors)
anova(danaov)

gpaov <- aov(TermCumGPA ~ TermMajor*Ethnicity, data = mymajors)
anova(gpaov)

praov <- aov(percPassed ~ TermMajor*Ethnicity, data = mymajors)
anova(praov)

# Ethnicity Only

dispeaov <- aov(DispAVG ~ Ethnicity, data = mymajors)
anova(dispeaov)

daneaov <- aov(DanAVG ~ Ethnicity, data = mymajors)
anova(daneaov)

gpeaov <- aov(TermCumGPA ~ Ethnicity, data = mymajors)
anova(gpeaov)

preaov <- aov(percPassed ~ Ethnicity, data = mymajors)
anova(preaov)

# Majors Only

dispmaov <- aov(DispAVG ~ TermMajor, data = mymajors)
anova(dispmaov)

danmaov <- aov(DanAVG ~ TermMajor, data = mymajors)
anova(danmaov)

gpmaov <- aov(TermCumGPA ~ TermMajor, data = mymajors)
anova(gpmaov)

prmaov <- aov(percPassed ~ TermMajor, data = mymajors)
anova(prmaov)


