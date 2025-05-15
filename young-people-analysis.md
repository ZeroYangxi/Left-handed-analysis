Are left-handed young people creative genius with vulnerable hearts?
Data says it might not.
================
2025-05-09

- Introduction: dataset description and problem statement
- Methodology: techniques used and justification
- Results: findings from your analysis
- Discussion: interpretation of results and limitations
- Conclusion: summary and potential future work
- References: cite all sources used

# Introduction

## dataset description

This analysis use the Young People Survey collected by Statistics
students at FSEV UK in 2013 on Kaggle (Sabo, 2015).

You can find the exact dataset and description of each item in this
link:
<https://www.kaggle.com/datasets/miroslavsabo/young-people-survey?select=columns.csv>
(Sabo, 2015))

To run the analysis, download the dataset from kaggle and set the
directory using the setwd() syntax here:

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
setwd("/Users/riveryu/Documents/Cornell/STSCI6020/final exam/young people dataset") #set your directory of the downloaded data csv here
all_data <- read.csv("responses.csv")
# names(all_data)
```

## problem statement

Roughly 10% of the human population is left-handed (Sha et al., 2021).
When you type “left-handed people are” in Google search bar, the first
few search recommendations include “left-handed people are intelligent”,
“left-handed people are lucky” and “left-handed people are more
artistic”. It seems like left-handedness is related to a smart and
creative impression. However, as the saying goes: “there’s a fine line
between genius and madness.” Creative genius might suffer from some
mental health concerns. In a word, can left-handedness be predicted
using psychological traits and creative interest?

Based on this, this study generates the following research questions
using: 1. Can creativity-related interests and psychological traits
predict whether a young person (aged 18-25) is left-handed or
right-handed? 2. Which specific factors are the strongest predictors of
handedness in young adults?

By clarifying these relationships in this dataset, it can help
understand left-handedness and possibly demystify it. It also has
important implications to identify potential psychological and
health-related needs. It can also fulfill our curiosity appetite!

We only focus on these variables of the dataset: \#### Demographics
Left…right.handed: A categorical variable indicating whether the
respondent is left-handed or right-handed. Age: numeric

#### Creativity Related Variables (Interest in Art)

all are integer variables rated 1-5. Writing: Interest in poetry
writing, where 1 = Not interested and 5 = Very interested Reading:
Interest in poetry reading, where 1 = Not interested and 5 = Very
interested Musical.instruments: Interest in playing musical instruments,
where 1 = Not interested and 5 = Very interested Art.exhibitions:
Interest in art exhibitions, where 1 = Not interested and 5 = Very
interested Dancing: Interest in dancing, where 1 = Not interested and 5
= Very interested Theatre: Interest in theatre, where 1 = Not interested
and 5 = Very interested Musical: Preference for musicals, where 1 =
Don’t enjoy and 5 = Enjoy very much

#### Psychological Traits

(1-5, where 1 = Strongly disagree and 5 = Strongly agree):
Happiness.in.life: “I am 100% happy with my life.” Energy.levels: “I am
always full of life and energy.” Personality: “I believe all my
personality traits are positive.”

reverse coded: Hypochondria: “I am a hypochondriac.” Mood.swings: “My
moods change quickly.” Getting.angry: “I can get angry very easily.”
Self.criticism: “I often think about and regret the decisions I make.”
Loneliness: “I feel lonely in life.” Unpopularity: “I will find a fault
in myself if people don’t like me.”

#### Health Habits

Healthy.eating: “I live a very healthy lifestyle” (1-5 agreement scale)

# Methodology

## Data pre-processing and visualization

``` r
selected_vars <- c(
  "Left...right.handed",
  
  # creativity related variables
  # writing and reading refers to poetry reading and writing
  "Writing" , "Reading", "Musical.instruments", "Art.exhibitions", 
  "Dancing", "Theatre", "Musical",
  
  # psychological traits
  "Hypochondria", "Mood.swings", "Getting.angry", "Self.criticism",
  "Empathy", "Loneliness", "Life.struggles", "Happiness.in.life",
  "Energy.levels", "Personality", "Unpopularity",
  
  # health lifestyle
  "Healthy.eating",
  
  # controlling demographics
  "Age", "Gender"
)

data <- all_data[, selected_vars]

#print("First few column names:")
#print(head(colnames(data)))
```

Transform categorical variable (text input) into numeric values

``` r
#  Right handed = 0, Left handed = 1
data$Left_handed <- ifelse(data$Left...right.handed == "left handed", 1, 0)

# Gender: Male = 0, Female = 1
data$Gender_numeric <- ifelse(data$Gender == "Female", 1, 0)
```

Check for missing values

``` r
sample_size = nrow(data)
missing_count <- numeric(ncol(data))

names(missing_count) <- colnames(data)
  
for(i in 1:ncol(data)) {
    missing_count[i] <- sum(is.na(data[,i]))
  }
  
print("Number of missing values per column:")
```

    ## [1] "Number of missing values per column:"

``` r
print(missing_count[missing_count > 0])
```

    ##             Writing             Reading Musical.instruments     Art.exhibitions 
    ##                   6                   6                   1                   6 
    ##             Dancing             Theatre             Musical        Hypochondria 
    ##                   3                   8                   2                   4 
    ##         Mood.swings       Getting.angry      Self.criticism             Empathy 
    ##                   4                   4                   5                   5 
    ##          Loneliness      Life.struggles   Happiness.in.life       Energy.levels 
    ##                   1                   3                   4                   5 
    ##         Personality        Unpopularity      Healthy.eating                 Age 
    ##                   4                   3                   3                   7

``` r
missing_percent <- (missing_count / sample_size)
print("Missing ratio:")
```

    ## [1] "Missing ratio:"

``` r
print(missing_percent[missing_percent > 0])
```

    ##             Writing             Reading Musical.instruments     Art.exhibitions 
    ##         0.005940594         0.005940594         0.000990099         0.005940594 
    ##             Dancing             Theatre             Musical        Hypochondria 
    ##         0.002970297         0.007920792         0.001980198         0.003960396 
    ##         Mood.swings       Getting.angry      Self.criticism             Empathy 
    ##         0.003960396         0.003960396         0.004950495         0.004950495 
    ##          Loneliness      Life.struggles   Happiness.in.life       Energy.levels 
    ##         0.000990099         0.002970297         0.003960396         0.004950495 
    ##         Personality        Unpopularity      Healthy.eating                 Age 
    ##         0.003960396         0.002970297         0.002970297         0.006930693

By looking at the missing values, there’s relatively low proportion. The
largest missing value is under item “Unpopularity”, which is only 0.2%.
The highest frequency of missing value for each item is 8, which is a
relatively small number given the total 1010 entries.

Let’s try calculating the number if we emit these missing rows:

``` r
data_without_na <- na.omit(data)

cat("Original number of rows:", nrow(data), "\n")
```

    ## Original number of rows: 1010

``` r
cat("Number of Rows after removing NA:", nrow(data_without_na), "\n")
```

    ## Number of Rows after removing NA: 938

``` r
cat("Number of Rows removed:", nrow(data) - nrow(data_without_na), "\n")
```

    ## Number of Rows removed: 72

If we emit all the columns containing the missing values, we need to
remove 98 rows. So, for convenience, we can simply emitted them.

Now, let’s filter age and only include ages from 18 to 25:

``` r
data_cleaned <- data_without_na[data_without_na$Age >= 18 & data_without_na$Age <= 25, ]
cat("Number of rows after age filtering:", nrow(data_cleaned), "\n")
```

    ## Number of rows after age filtering: 788

``` r
cat("Number of left-handed rows after age filtering:", nrow(data_cleaned[data_cleaned$Left_handed == 1, ]), "\n")
```

    ## Number of left-handed rows after age filtering: 74

``` r
cat("Number of right-handed rows after age filtering:", nrow(data_cleaned[data_cleaned$Left_handed == 0, ]))
```

    ## Number of right-handed rows after age filtering: 714

The total number of data entries we will be dealing with is 765. The
data includes 71 left-handed people and 694 right-handed people, This
left-handed and right-handed ratio is similar to the 10% of left-handed
people in general population mentioned before.

Now, let’s culculate the average of each dimensions.

``` r
# Interest in art (creativity) (7 items)
creativity_vars <- c("Writing", "Reading", "Musical.instruments", 
                    "Art.exhibitions", "Dancing", "Theatre", "Musical")
data_cleaned$Creativity_avg <- rowMeans(data_cleaned[, creativity_vars], na.rm = TRUE)

# Psychological wellbeing/personality traits (11 items)
psych_traits_vars <- c("Hypochondria", "Mood.swings", "Getting.angry", "Self.criticism", "Empathy", "Loneliness", "Life.struggles", "Happiness.in.life", "Energy.levels", "Personality", "Unpopularity")
data_cleaned$Psych_traits_avg <- rowMeans(data_cleaned[, psych_traits_vars], na.rm = TRUE)


# Psychological traits
# reverse coded variables for negative traits, so higher psychological trait scores means better psychological wellbeing
data_cleaned$Hypochondria_r <- 6 - data_cleaned$Hypochondria
data_cleaned$Mood.swings_r <- 6 - data_cleaned$Mood.swings
data_cleaned$Getting.angry_r <- 6 - data_cleaned$Getting.angry
data_cleaned$Self.criticism_r <- 6 - data_cleaned$Self.criticism
data_cleaned$Loneliness_r <- 6 - data_cleaned$Loneliness
data_cleaned$Unpopularity_r <- 6 - data_cleaned$Unpopularity

# calculate average 
psych_traits_vars <- c("Hypochondria_r", "Mood.swings_r", "Getting.angry_r", "Self.criticism_r", "Empathy", "Loneliness_r", "Life.struggles", "Happiness.in.life", "Energy.levels", "Personality", "Unpopularity_r")
data_cleaned$Psych_traits_avg <- rowMeans(data_cleaned[,psych_traits_vars], na.rm = TRUE)

# Healthy lifestyle (1 item)
health_vars <- c("Health")
data_cleaned$Health_avg <- data_cleaned$Health

# dimensions summary 
dimensions_summary <- data.frame(
  Dimension = c("Interest in Art", "Psychological_traits", "Health_habits"),
  Number_of_items = c(length(creativity_vars), length(psych_traits_vars), length(health_vars)),
  Mean_score = c(
    mean(data_cleaned$Creativity_avg, na.rm = TRUE),
    mean(data_cleaned$Psych_traits_avg, na.rm = TRUE),
    mean(data_cleaned$Health_avg, na.rm = TRUE)
  )
)

# dimensions summary 
dimensions_summary <- data.frame(
  Dimension = c("Interest in Art", "Psychological_traits", "Healthy_Lifestyle"), Number_of_items = c(length(creativity_vars), length(psych_traits_vars), length(health_vars)),  Mean_score = c(mean(data_cleaned$Creativity_avg),
                mean(data_cleaned$Psych_traits_avg),
                mean(data_cleaned$Health_avg))
)

print(dimensions_summary)
```

    ##              Dimension Number_of_items Mean_score
    ## 1      Interest in Art               7   2.592640
    ## 2 Psychological_traits              11   3.232234
    ## 3    Healthy_Lifestyle               1   3.036802

``` r
# check the mean scores by handedness
handedness_comparison <- data_cleaned %>%
  group_by(Left_handed) %>%
  summarise(
    Creativity_mean = mean(Creativity_avg),
    Psych_traits_mean = mean(Psych_traits_avg),
    Health_mean = mean(Health_avg)
  )

print(handedness_comparison)
```

    ## # A tibble: 2 × 4
    ##   Left_handed Creativity_mean Psych_traits_mean Health_mean
    ##         <dbl>           <dbl>             <dbl>       <dbl>
    ## 1           0            2.59              3.23        3.02
    ## 2           1            2.60              3.30        3.18

## Model Buiding Assumption Testing

Since the predicted variable will be handedness, which is a binary
outcome, I will go for logistic regression before I test its
assumptions. \#### Model 1: Using composite scores

``` r
model1 <- glm(Left_handed ~ Creativity_avg + Psych_traits_avg + Health_avg + 
              Age + Gender_numeric,
              data = data_cleaned,
              family = binomial(link = "logit"))

summary(model1)
```

    ## 
    ## Call:
    ## glm(formula = Left_handed ~ Creativity_avg + Psych_traits_avg + 
    ##     Health_avg + Age + Gender_numeric, family = binomial(link = "logit"), 
    ##     data = data_cleaned)
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                   Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)      -4.570835   1.732621  -2.638  0.00834 **
    ## Creativity_avg   -0.002979   0.144603  -0.021  0.98357   
    ## Psych_traits_avg  0.396768   0.295486   1.343  0.17935   
    ## Health_avg        0.172782   0.138830   1.245  0.21330   
    ## Age               0.023718   0.067465   0.352  0.72517   
    ## Gender_numeric          NA         NA      NA       NA   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 490.91  on 787  degrees of freedom
    ## Residual deviance: 486.98  on 783  degrees of freedom
    ## AIC: 496.98
    ## 
    ## Number of Fisher Scoring iterations: 5

# Model 2: Full model with individual variables to identify strongest predictors

This model includes every item of the different dimensions to allow
deeper examination. This model uses the reversed coded variables to
ensure the psychological trait scores are all positively coded.

``` r
model2 <- glm(Left_handed ~ Writing + Reading + Musical.instruments + Art.exhibitions + Dancing + Theatre + Musical + Hypochondria + Mood.swings + Getting.angry + Self.criticism + Empathy + Loneliness + Life.struggles + Happiness.in.life + Energy.levels + Personality + Unpopularity + Healthy.eating + Age + Gender_numeric, data = data_cleaned, family = binomial(link = "logit"))

summary(model2)
```

    ## 
    ## Call:
    ## glm(formula = Left_handed ~ Writing + Reading + Musical.instruments + 
    ##     Art.exhibitions + Dancing + Theatre + Musical + Hypochondria + 
    ##     Mood.swings + Getting.angry + Self.criticism + Empathy + 
    ##     Loneliness + Life.struggles + Happiness.in.life + Energy.levels + 
    ##     Personality + Unpopularity + Healthy.eating + Age + Gender_numeric, 
    ##     family = binomial(link = "logit"), data = data_cleaned)
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                      Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)         -2.995476   2.055321  -1.457   0.1450  
    ## Writing              0.220805   0.105765   2.088   0.0368 *
    ## Reading             -0.072053   0.102656  -0.702   0.4828  
    ## Musical.instruments -0.043528   0.091227  -0.477   0.6333  
    ## Art.exhibitions      0.067621   0.125013   0.541   0.5886  
    ## Dancing              0.007757   0.095837   0.081   0.9355  
    ## Theatre             -0.066418   0.131351  -0.506   0.6131  
    ## Musical             -0.026736   0.114060  -0.234   0.8147  
    ## Hypochondria         0.020659   0.116446   0.177   0.8592  
    ## Mood.swings          0.051161   0.140345   0.365   0.7155  
    ## Getting.angry        0.101937   0.118710   0.859   0.3905  
    ## Self.criticism      -0.167433   0.111630  -1.500   0.1336  
    ## Empathy              0.184796   0.121939   1.515   0.1297  
    ## Loneliness          -0.120580   0.133142  -0.906   0.3651  
    ## Life.struggles      -0.099349   0.108673  -0.914   0.3606  
    ## Happiness.in.life   -0.054529   0.186943  -0.292   0.7705  
    ## Energy.levels       -0.022219   0.149700  -0.148   0.8820  
    ## Personality          0.195039   0.217098   0.898   0.3690  
    ## Unpopularity        -0.126642   0.115912  -1.093   0.2746  
    ## Healthy.eating       0.184858   0.143150   1.291   0.1966  
    ## Age                  0.007840   0.069944   0.112   0.9108  
    ## Gender_numeric             NA         NA      NA       NA  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 490.91  on 787  degrees of freedom
    ## Residual deviance: 473.11  on 767  degrees of freedom
    ## AIC: 515.11
    ## 
    ## Number of Fisher Scoring iterations: 5

## Assumption Testing

There are five assumptions that needed to be looked at before I go into
interpretation the model fitting results. 1. Independence of
Observations: Each data entry is independent from each other since one
participant only fill in the questionnaire one time. There are no
repeated measures or nested data structures. 2. Linearity: The
relationship between continuous predictors and log odds should be linear
3. No multicollinearity: Predictors shouldn’t be highly correlated. 4.
No influential outliers. 5. Adequate sample size: Generally need 10-20
observations per predictor.

For model2, with only 72 left-handed individuals, the large number of
variables clearly violates the adequate sample size assumption.

Therefore, I will tend to select the most important variable first for
model 2. Since there are many variables, I will use the forward
selection method. It also helps to avoid over-fitting.

Now I will conduct forward selection.

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
# Forward Selection
null_model <- glm(Left_handed ~ 1, data = data_cleaned, family = binomial)

full_model <- glm(Left_handed ~ Writing + Reading + Musical.instruments + Art.exhibitions + Dancing + Theatre + Musical + Hypochondria + Mood.swings + Getting.angry + Self.criticism + Empathy + Loneliness + Life.struggles + Happiness.in.life + Energy.levels + Personality + Unpopularity + Healthy.eating + Age + Gender_numeric, data = data_cleaned, family = binomial)

cat("Forward Selection \n")
```

    ## Forward Selection

``` r
model2_forward <- stepAIC(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward", trace = FALSE) 

cat("\nFinal Model from Forward Selection\n")
```

    ## 
    ## Final Model from Forward Selection

``` r
summary(model2_forward)
```

    ## 
    ## Call:
    ## glm(formula = Left_handed ~ Unpopularity + Writing + Self.criticism + 
    ##     Healthy.eating, family = binomial, data = data_cleaned)
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)    -1.98464    0.64801  -3.063  0.00219 **
    ## Unpopularity   -0.18422    0.10869  -1.695  0.09010 . 
    ## Writing         0.16379    0.08862   1.848  0.06456 . 
    ## Self.criticism -0.17033    0.10361  -1.644  0.10018   
    ## Healthy.eating  0.19654    0.13744   1.430  0.15270   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 490.91  on 787  degrees of freedom
    ## Residual deviance: 479.78  on 783  degrees of freedom
    ## AIC: 489.78
    ## 
    ## Number of Fisher Scoring iterations: 5

Running the forward selection, we only have 4 variables left to predict
handedness: Unpopularity: “I will find a fault in myself if people don’t
like me.” Writing: “(Interest in) Poetry writing” Self.criticism: “I
often think about and regret the decisions I make.” Healthy.eating: “I
live a very healthy lifestyle”.

These four predictors can now met the sample size assumptions.

Now, the author choose AUC as the primary model performance metric due
to the imbalanced samples in which there are only 10% left-handed
individual. Because AUC measures the model’s ability to separate left
and right-handed subjects at all the classification thresholds, it is
more robust in unbalanced sample.

``` r
library(pROC)
```

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
roc_obj <- roc(data_cleaned$Left_handed, fitted(model2_forward))
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
auc_value <- auc(roc_obj)
cat("AUC:", round(auc_value, 3), "\n")
```

    ## AUC: 0.608

``` r
# Calculate Odds Ratios with 95% CI
OR_results <- exp(cbind(OR = coef(model2_forward), confint(model2_forward)))
```

    ## Waiting for profiling to be done...

``` r
print(round(OR_results, 3))
```

    ##                   OR 2.5 % 97.5 %
    ## (Intercept)    0.137 0.037  0.472
    ## Unpopularity   0.832 0.672  1.031
    ## Writing        1.178 0.985  1.396
    ## Self.criticism 0.843 0.689  1.035
    ## Healthy.eating 1.217 0.933  1.600

The confidence intervals of all four predictors includes 1, so none of
them are significant.

``` r
selected_variables <- names(coef(model2_forward))[-1] # remove intercept
cat("\nSelected variables:\n")
```

    ## 
    ## Selected variables:

``` r
print(selected_variables)
```

    ## [1] "Unpopularity"   "Writing"        "Self.criticism" "Healthy.eating"

``` r
cat("\nNumber of selected variables:", length(selected_variables), "\n")
```

    ## 
    ## Number of selected variables: 4

``` r
n_events <- sum(data_cleaned$Left_handed)
n_predictors <- length(selected_variables)
events_per_predictor <- n_events / n_predictors

if(any(abs(coef(model2_forward)[-1]) > 10)) {
  cat("\nWarning: Some coefficients are very large (|β| > 10)\n")
  large_coef <- which(abs(coef(model2_forward)[-1]) > 10)
  cat("Variables with large coefficients:", names(coef(model2_forward)[-1])[large_coef], "\n")
}

cat("\nModel performance:\n")
```

    ## 
    ## Model performance:

``` r
cat("AIC:", AIC(model2_forward), "\n")
```

    ## AIC: 489.7777

``` r
predicted_prob <- predict(model2_forward, type = "response")
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
accuracy <- mean(predicted_class == data_cleaned$Left_handed)
cat("Classification accuracy:", round(accuracy * 100, 2), "%\n")
```

    ## Classification accuracy: 90.61 %

``` r
confusion_matrix <- table(Actual = data_cleaned$Left_handed, 
                         Predicted = predicted_class)
cat("\nConfusion Matrix:\n")
```

    ## 
    ## Confusion Matrix:

``` r
print(confusion_matrix)
```

    ##       Predicted
    ## Actual   0
    ##      0 714
    ##      1  74

``` r
model2_final <- model2_forward
```

# Results & Discussion

Althought the model achieved decent AIC = 489.7777 and 90%
classification accuracy, it is probably that the model just predict
right-handedness all the time. The sample is too unbalanced to have 10%
left-handedness people and 90% right-handedness people.

The forward selection produced four predictors for left-handedness:
unpopularity (cares about what others think of oneself), interest in
poetry writing, self-criticism, and healthy lifestyle.

These are the exact items a and its variable name: Unpopularity: “I will
find a fault in myself if people don’t like me.” Writing: “(Interest in)
Poetry writing” Self.criticism: “I often think about and regret the
decisions I make.” Healthy.eating: “I live a very healthy lifestyle”.

However, none of the four variables are statistically significant enough
to say that it can predict left-handedness. The AUC is 0.608, which
suggests that

Interest in poetry writing showed the strongest association (p = 0.065).
Although it suggests a potential link between creative pursuits and
left-handedness aligning with popular beliefs, considering other
non-selected variables that encompassing many creative fields and
interest, we cannot conclude that left-handed people are more creative
using this sample.

However, poetry writing can be associated with abstract thinking. For
future research on creativity and left-handedness, it is recommended
that to look into abstract thinking domain. Abstract thinking might be a
strength of left-handed people

For psychological traits, unpopularity (p = 0.090) and self-criticism (p
= 0.100) showed negative associations with left-handedness.

It seems like left-handed young people are neither mentally vulnerable
nor leading a unhealthy lifestyle in these dimensions.

### Limitation

There are also limitations of the current study. With only 72
left-handed individuals in the sample, the study may not have the enough
power to detect subtle associations. Moreover, future research can use
more estabilished scales to evaluate different traits. Additionally, the
non-significant results could indicate that handedness is determined by
factors not captured in this survey.

# Reference

Sabo, M. (2015). Young People Survey \[Data set\]. Kaggle.
<https://www.kaggle.com/datasets/miroslavsabo/young-people-survey>

Sha, Z., Pepe, A., Schijven, D., Carrión-Castillo, A., Roe, J.M.,
Westerhausen, R., Joliot, M., Fisher, S.E., Crivello, F., & Francks, C.
(2021). Handedness and its genetic influences are associated with
structural asymmetries of the cerebral cortex in 31,864 individuals.
Proceedings of the National Academy of Sciences of the United States of
America, 118.
