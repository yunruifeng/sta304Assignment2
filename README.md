# sta304Assignment2
sta304Assignment2


According to a five-year study from Edith Cowan University in Australia, families with more children have a higher level of life satisfaction rather than feeling overwhelmed by the big family size. The study interviewed hundreds of parents from families of different structures. They found that larger families benefit from increased support and therefore have the highest overall life satisfaction. Our aim in this report is to investigate whether we can observe similar associations between life satisfaction and household size. To test the hypothesis that there is a positive correlation between household size and life satisfaction, we make a choice of using a linear regression model to analyze their quantitative relationship as they are both continuous variables. The dependent variable is life satisfaction and the independent variable is household size. The analysis does not show a statistically significant result on the correlation between life satisfaction and household size, but it does show that household size larger than one has a higher level of life satisfaction than a single person. We conclude that household size larger than one has a higher level of life satisfaction.


## Introduction
Feelings of life, in other words, life satisfaction is an overall assessment of feelings and attitudes about one's life as a whole. Numerous studies show that life satisfaction has a positive impact on our health, and it is one of the major indicators of well-being. For instance, researchers at Chapman University published a research on life satisfaction and its association with reducing risk of mortality. Regarding the importance of improving life satisfaction, we are interested in the factors associated with the changes in the level of life satisfaction. We use the data from the Public Use Microdata File (PUMF) of the 2017 General Social Survey (GSS) on the Family. We use the ‘gss_cleaning’ code provided to clean up the data and put it into a tidy format. Since we are investigating the relationship between life satisfaction and household size, we extract the columns containing the corresponding data and put them in a data frame for analysis. In the report, we create plots and investigate mean life satisfaction scores for each size of the household, confidence intervals,  interquartile range of the residuals, estimated coefficients along with its standard error and significance test, and other model statistics. We discuss the strengths and weaknesses of the data and come to a conclusion. 



GITHUB LINK
https://github.com/yunruifeng/sta304Assignment2.git
## Data
##### The data used for analysis is from the 2017 General Social Survey. The target population of this survey includes "all non-institutionalized persons 15 years of age and older living in the ten provinces of Canada". The frame population is created using the lists of telephone numbers in use available to Statistics Canada and The Address Register. And the survey uses stratified random sampling to collect the data. There are two columns of data that are selected, one measures household size ("household_size") and the other one measures the household's feelings about life ("feelings_life). The variable "household_size" has only integer values from 1 to 6, the variable "feelings_life" also only has integer values but it is in a scale of 1 to 10.

##### There are many studies which study the factors that affect an individual's life satisfaction. And some of them conclude that people who live in larger families have higher overall life satisfaction. So it will be interesting to use some data to test this conclusion and try to make some new inferences. Moreover, the dataset has very large number of observations and there is no na value within those observation. So the selected data can be modlled and analysed without too much "cleaning" work. 

##### Furthermore, the selected data also has its drawbacks. For example, this dataset only contains discrete variables and the variable "household_size" will have to be treated as a categorical variable. So it will be harder to fit a model into the data nicely, thus negatively affects the model's ability to predict the dependent variable "feelings_life".

## Model
```{r, echo = FALSE}
happyness<- tibble(gss$hh_size,gss$feelings_life)
happyness <- happyness[complete.cases(happyness),]
happyness <-
  happyness%>%
  rename(household_size = `gss$hh_size`,
         feelings_life = `gss$feelings_life`)
plot(as.factor(happyness$household_size),happyness$feelings_life, xlab = "Household Size", ylab = "Happyness Score", main = "Distribution of life satisfaction scores in each household size")
my_LRmodel <- lm(happyness$feelings_life ~ as.factor(happyness$household_size))
summary(my_LRmodel)
r <- rstudent(my_LRmodel)
plot(happyness$household_size, r, xlab = "Household Size", ylab = "Standardized Residuals",
     main = "Standardized Residuals vs. Household Size")
#她说要draw on relevant literature, 我刚好找到一篇关于这个的“Study finds larger families have highest overall life satisfaction”
```
 
##### The model used here is a mutiple linear regression model: feelings_life = 7.69396 + 0.61378x1 + 0.40607x2 + 0.54916x3 + 0.52602x4 + 0.67527x5. 
##### Note: x1, x2, x3, x4 and x5 are all dummy variables, x1 is 1 only when household size is 2; x2 is 1 only when household size is 3; x3 is 1 only when household size is 4; x4 is 1 only when household size is 5; x4 is 1 only when household size is 5; x5 is 1 only when household size is 6.
##### A linear regression model is used here rather than a logistic regression model because the dependent variable "feelings_life" is not binary. And a linear regression model is good to compare the mean differences of life satisfaction scores between each size of the household. It is useful to find out whether there is a general pattern between household size and life satisfaction. Moreover, population correction is not used because the sample size is less than 5% of the total population.
##### For diagnosis checking, because all independent variables are dummy variables, the linearity assumption is certainly satisfied. The residual plot does not show any patterns or specific clusters of data points thus both the homoscedasticity and normality assumptions are met. Therefore, this should be a valid model to analyze further.


## Results

```{r, echo = FALSE}
summary(my_LRmodel)
```

#### This summary contains the results of the estimated model includes the interquartile range of the residuals, the estimated coefficients along with its standard error and significance test, and other model statistics such as R-squared. This summary will be called table 1.

```{r, echo = FALSE}
avg_hz2 <- as.numeric(my_LRmodel$coefficients[2]) + as.numeric(my_LRmodel$coefficients[1]) 
avg_hz3 <- as.numeric(my_LRmodel$coefficients[3]) + as.numeric(my_LRmodel$coefficients[1]) 
avg_hz4 <- as.numeric(my_LRmodel$coefficients[4]) + as.numeric(my_LRmodel$coefficients[1]) 
avg_hz5 <- as.numeric(my_LRmodel$coefficients[5]) + as.numeric(my_LRmodel$coefficients[1]) 
avg_hz6 <- as.numeric(my_LRmodel$coefficients[6]) + as.numeric(my_LRmodel$coefficients[1])

upper_confidence_interval_hz1 <- as.numeric(my_LRmodel$coefficients[1]) + 1.96*0.02149
upper_confidence_interval_hz2 <- avg_hz2 + 1.96*0.02840  
upper_confidence_interval_hz3 <- avg_hz3 + 1.96*0.03726 
upper_confidence_interval_hz4 <- avg_hz4 + 1.96*0.03782 
upper_confidence_interval_hz5 <- avg_hz5 + 1.96*0.05591 
upper_confidence_interval_hz6 <- avg_hz6 + 1.96*0.08504 

lower_confidence_interval_hz1 <- as.numeric(my_LRmodel$coefficients[1]) - 1.96*0.02149
lower_confidence_interval_hz2 <- avg_hz2 - 1.96*0.02840 
lower_confidence_interval_hz3 <- avg_hz3 - 1.96*0.03726 
lower_confidence_interval_hz4 <- avg_hz4 - 1.96*0.03782 
lower_confidence_interval_hz5 <- avg_hz5 - 1.96*0.05591 
lower_confidence_interval_hz6 <- avg_hz6 - 1.96*0.08504

avg_hz <- matrix(c(as.numeric(my_LRmodel$coefficients[1]),lower_confidence_interval_hz1, upper_confidence_interval_hz1, avg_hz2, lower_confidence_interval_hz2,upper_confidence_interval_hz2,  avg_hz3, lower_confidence_interval_hz3, upper_confidence_interval_hz3,  avg_hz4, lower_confidence_interval_hz4, upper_confidence_interval_hz4, avg_hz5, lower_confidence_interval_hz5, upper_confidence_interval_hz5, avg_hz6, lower_confidence_interval_hz6, upper_confidence_interval_hz6), ncol = 3, byrow = TRUE)
colnames(avg_hz) <- c("AVG_Feelings_Life", "95% CI (Lower Bound)", "95% CL (Upper Bound)")
rownames(avg_hz) <- c("Household_size_1","Household_size_2","Household_size_3","Household_size_4","Household_size_5","Household_size_6")
avg_hz <- as.table(avg_hz)
avg_hz
```

#### This table contains the mean life satisfaction scores for each size of the household and their corresponding 95% confidence intervals. And it will be called table 2.


```{r, echo = FALSE}
ggplot(happyness, aes(x = household_size,y=feelings_life))+geom_point()+geom_smooth(method=lm)+ labs(title="Scatterplot of household size versus feeling about life") +  xlim(1,6) + ylim(0,10)
```

#### This graph shows the fitted value visually by adding the model prediction (blue line) to the scatter plot of the original dataset. And it will be called figure 3.


## Discussion

Here you will discuss conclusions drawn from the results and comment on how it relates to the original goal of the study (which was specified in the Introduction).

# Weaknesses

Here we discuss weaknesses of the study, data, analysis, etc. You can also discuss areas for improvement.

# Next Steps

Here you discuss subsequent work to be done after this report. This can include next steps in terms of statistical analysis (perhaps there is a more efficient algorithm available, or perhaps there is a caveat in the data that would allow for some new technique). Future steps should also be specified in terms of the study setting (eg. including a follow-up survey on something, or a subsequent study that would complement the conclusions of your report).


## References
https://www150.statcan.gc.ca/n1/daily-quotidien/170913/t001a-eng.htm

https://sda-artsci-utoronto-ca.myaccess.library.utoronto.ca/sdaweb/dli2/gss/gss31/gss31/more_doc/GSS31_User_Guide.pdf