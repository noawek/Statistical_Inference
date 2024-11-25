install.packages('tidyverse')
install.packages('dplyr')
install.packages('boot')
install.packages('coin')
install.packages('ggpubr')
install.packages('ggplot2')
install.packages('car')
install.packages('carData')
install.packages('data.table')
install.packages('afex')
install.packages("lme4")
install.packages('emmeans')
install.packages('MASS')
install.packages('Matrix')
install.packages('effectsize')

library('effectsize')
library('MASS')
library('Matrix')
library('emmeans')
library('lme4')
library('afex')
library ('dplyr')
library('tidyverse')
library ('ggplot2')
library('boot')
library('coin')
library('ggpubr')
library('car')
library('carData')
library('data.table')


data <- read.csv("322592999_208076315.csv")
data$TestAVG <- apply (data[,6:8],1,mean)       # adding an average test scores column


#-------------------------------------------start of work---------------------------------------------------


# Exercise 1



# Descriptive statistics


#Number of observations
count(data, data$gender == "female")
count(data, data$gender == "male")

#Math scores means based on gender
MathMeanF <- mean (data$math.score [data$gender == "female"])
MathMeanM <- mean (data$math.score [data$gender == "male"])

#Math scores standard deviations based on gender
MathSDF <-  sd (data$math.score [data$gender == "female"])
MathSDM <-  sd (data$math.score [data$gender == "male"])



# Examination of the assumptions


GirlsMathGrades <- data$math.score[data$gender=="female"]
BoysMathGrades <- data$math.score [data$gender == "male"]

# Normality examination (in 3 ways)

#1-histograms
hist(BoysMathGrades, breaks = 50)
hist(GirlsMathGrades, breaks = 50)

#2-Normal probability plot (QQ plot)
ggqqplot(BoysMathGrades)
ggqqplot(GirlsMathGrades)

#3-Shapiro-Wilk test
shapiro.test(BoysMathGrades)
shapiro.test(GirlsMathGrades)


# Variances equality check

leveneTest(data$math.score ~ data$gender , data, center = "mean")



# Proceeding with Welch T test (after checking the assumptions) 

t.test(data$math.score [data$gender == "female"]
       ,data$math.score [data$gender == "male"],
       alternative = c("two.sided"), 
       paired = FALSE, conf.level = 0.95, var.equal = TRUE)  # P-value < 0.05 - significant finding



# Exercise 6 - attached to the word file












# Exercise 2



# Descriptive statistics


#Number of observations
count(data, data$race.ethnicity == "group A")
count(data, data$race.ethnicity == "group B")
count(data, data$race.ethnicity == "group C")
count(data, data$race.ethnicity == "group D")
count(data, data$race.ethnicity == "group E")

#Average test scores mean by groups
TestMeanGA <- mean (data$TestAVG [data$race.ethnicity == "group A"])
TestMeanGB <- mean (data$TestAVG [data$race.ethnicity == "group B"])
TestMeanGC <- mean (data$TestAVG [data$race.ethnicity == "group C"])
TestMeanGD <- mean (data$TestAVG [data$race.ethnicity == "group D"])
TestMeanGE <- mean (data$TestAVG [data$race.ethnicity == "group E"])

#Average test scores standard deviation by groups
sd (data$TestAVG [data$race.ethnicity == "group E"])
sd (data$TestAVG [data$race.ethnicity == "group D"])
sd (data$TestAVG [data$race.ethnicity == "group C"])
sd (data$TestAVG [data$race.ethnicity == "group B"])
sd (data$TestAVG [data$race.ethnicity == "group A"])



# Examination of the assumptions


# Normality examination (in 3 ways)

#1-histograms
hist(data$TestAVG [data$race.ethnicity == "group A"], breaks = 50)
hist(data$TestAVG [data$race.ethnicity == "group B"], breaks = 50)
hist(data$TestAVG [data$race.ethnicity == "group C"], breaks = 50)
hist(data$TestAVG [data$race.ethnicity == "group D"], breaks = 50)
hist(data$TestAVG [data$race.ethnicity == "group E"], breaks = 50)  

#2-Normal probability plot (QQ plot)
ggqqplot(data$TestAVG [data$race.ethnicity == "group A"])
ggqqplot(data$TestAVG [data$race.ethnicity == "group B"])
ggqqplot(data$TestAVG [data$race.ethnicity == "group C"])
ggqqplot(data$TestAVG [data$race.ethnicity == "group D"])
ggqqplot(data$TestAVG [data$race.ethnicity == "group E"])       

#3-Shapiro-Wilk test
shapiro.test(data$TestAVG [data$race.ethnicity == "group A"])
shapiro.test(data$TestAVG [data$race.ethnicity == "group B"])
shapiro.test(data$TestAVG [data$race.ethnicity == "group C"])
shapiro.test(data$TestAVG [data$race.ethnicity == "group D"])
shapiro.test(data$TestAVG [data$race.ethnicity == "group E"])   #it appears that groups A,D,E are approximately normal and B,C are not.


# Variances equality check

leveneTest(data$TestAVG ~ data$race.ethnicity , data, center = "mean")   #p-value = .69 -> equal variances can be assumed.



# Proceeding with one way ANOVA test (after checking the assumptions)

anova <- aov_ez(id = 'ID'
               ,dv = 'TestAVG',
                between = c('race.ethnicity')  ,
                data = data)


anova   #See the results- Significant finding



# Exercise 7 (Effect size)     

eta_squared(anova)



# Exercise 5 - attached to the word file





# Exercise 3



# Descriptive statistics  
#same as Exercise 2



# Examination of the assumptions 
#Normality examination and Variances equality check - same as Exercise 2

# An addition: 
#the assumption of Independent Samples (Orthogonal comparisons)- Explained and showed in the Word file



# Proceeding with Follow-up analysis (for Exercise 2)


#creating a data frame of Orthogonal comparisons
orthogonalContranst <- data.frame(A_vs_all = c(4,-1,-1,-1,-1),
                                  B_vs_CDE = c(0,3,-1,-1,-1),
                                  C_vs_DE = c(0,0,2,-1,-1),
                                  D_vs_E = c(0,0,0,1,-1)
                                  )      
# the orthogonality of the comparisons is presented and explained properly in the word file


#creating a temporary variable to hold the data from the emmeans function
m_eff <- emmeans(anova, ~ race.ethnicity)


# Contrasts (the actual comparisons)
GroupContrast <- contrast(m_eff, method=orthogonalContranst)

GroupContrast          # all results are significant



# Exercise 4 (creating an *extraordinary* graph)

# utilizing resampling methods to enhance the visualization of the distribution in the graph
DistGradeGroup <- sample(data$TestAVG [data$race.ethnicity == "group A"],1000, replace = TRUE)
DistGradeGroupB <- sample(data$TestAVG [data$race.ethnicity == "group B"],1000,replace = TRUE)
DistGradeGroupC <- sample(data$TestAVG [data$race.ethnicity == "group C"],1000,replace = TRUE)
DistGradeGroupD <- sample(data$TestAVG [data$race.ethnicity == "group D"],1000,replace = TRUE)
DistGradeGroupE <- sample(data$TestAVG [data$race.ethnicity == "group E"],1000,replace = TRUE)      

# creating a matrix with the data
plotData <- data.frame(DistGradeGroup,DistGradeGroupB,DistGradeGroupC,DistGradeGroupD,DistGradeGroupE)

# creating the masterpiece
ggplot(data = plotData,aes(x= DistGradeGroup, colour = data$race.ethnicity),)+geom_density() + facet_grid(~data$race.ethnicity)

