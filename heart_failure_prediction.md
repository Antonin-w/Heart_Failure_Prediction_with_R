<!-- omit in toc -->
# Heart Failure Prediction
<!-- omit in toc -->

2024-09-24 | Antonin Weber <br /> 

- [Data importation and curation](#data-importation-and-curation)
- [Visual representation of the data](#visual-representation-of-the-data)
- [Machine learning models](#machine-learning-models)
  - [Linear regression](#linear-regression)
    - [Preparing data](#preparing-data)
    - [Creating model](#creating-model)
  - [k-Nearest Neighbors](#k-nearest-neighbors)
    - [Preparing data](#preparing-data-1)
    - [Creating model](#creating-model-1)
  - [Random forest](#random-forest)
    - [Preparing data](#preparing-data-2)
    - [Creating model](#creating-model-2)
- [Results](#results)

 <br /> 

<details>
  <summary>
    Click to see library used
  </summary>

``` r
library(ggplot2)
library(hrbrthemes)
library(scales)
library(waffle)
library(ggthemes)
library(dplyr)
library(patchwork)
library(gridExtra)
library(ggpubr)
library(skimr)
library(corrplot)
library(caret) 
library(olsrr)
library(class)
library(fastDummies)
library(randomForest)
library(knitr)
```
</details>
 <br /> 

# Data importation and curation

``` r
set.seed(123)
data = read.csv("/Users/antonin/Documents/Github/Heart_Failure_Prediction_with_R/data/heart.csv", stringsAsFactors = T)
data$HeartDisease <- as.factor(data$HeartDisease)
data$FastingBS <- as.factor(data$FastingBS)
data$Cholesterol <- NULL 
```

The Cholesterol column bas been removed because many 0 within population
with Heart disease. Biologically, a Cholesterol level cannot be equal to
0, probably NAs values that have been replaced with 0.

To use this column next time, a benchmarking has to be done with the
different models created with the following options :

- Rows with Cholesterol = 0 removed.  
- Replace 0 values with the mean.  
- Replace 0 with the median.  
- Replace 0 values using the k-mean algorithm.

Observe prediction results and choose the best option.

``` r
skim(data)
```

|                                                  |      |
|:-------------------------------------------------|:-----|
| Name                                             | data |
| Number of rows                                   | 918  |
| Number of columns                                | 11   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| factor                                           | 7    |
| numeric                                          | 4    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: factor**

| skim_variable  | n_missing | complete_rate | ordered | n_unique | top_counts                           |
|:---------------|----------:|--------------:|:--------|---------:|:-------------------------------------|
| Sex            |         0 |             1 | FALSE   |        2 | M: 725, F: 193                       |
| ChestPainType  |         0 |             1 | FALSE   |        4 | ASY: 496, NAP: 203, ATA: 173, TA: 46 |
| FastingBS      |         0 |             1 | FALSE   |        2 | 0: 704, 1: 214                       |
| RestingECG     |         0 |             1 | FALSE   |        3 | Nor: 552, LVH: 188, ST: 178          |
| ExerciseAngina |         0 |             1 | FALSE   |        2 | N: 547, Y: 371                       |
| ST_Slope       |         0 |             1 | FALSE   |        3 | Fla: 460, Up: 395, Dow: 63           |
| HeartDisease   |         0 |             1 | FALSE   |        2 | 1: 508, 0: 410                       |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |   mean |    sd |   p0 | p25 |   p50 |   p75 |  p100 | hist  |
|:--------------|----------:|--------------:|-------:|------:|-----:|----:|------:|------:|------:|:------|
| Age           |         0 |             1 |  53.51 |  9.43 | 28.0 |  47 |  54.0 |  60.0 |  77.0 | ▁▅▇▆▁ |
| RestingBP     |         0 |             1 | 132.40 | 18.51 |  0.0 | 120 | 130.0 | 140.0 | 200.0 | ▁▁▃▇▁ |
| MaxHR         |         0 |             1 | 136.81 | 25.46 | 60.0 | 120 | 138.0 | 156.0 | 202.0 | ▁▃▇▆▂ |
| Oldpeak       |         0 |             1 |   0.89 |  1.07 | -2.6 |   0 |   0.6 |   1.5 |   6.2 | ▁▇▆▁▁ |

``` r
summary(data$RestingBP)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     0.0   120.0   130.0   132.4   140.0   200.0

Minimum 0 as repos resting mean the person is death, we can presume an
anomali in the data. Let’s check how many rows are concerned.

``` r
data[data$RestingBP == 0,]
```

    ##     Age Sex ChestPainType RestingBP FastingBS RestingECG MaxHR ExerciseAngina
    ## 450  55   M           NAP         0         0     Normal   155              N
    ##     Oldpeak ST_Slope HeartDisease
    ## 450     1.5     Flat            1

Only one, let’s replace its value with the mean of the other row to fix
that.

``` r
data[450,"RestingBP"] <- mean(data$RestingBP[-450])
```

# Visual representation of the data

<details>
  <summary>
    Code of the differents plots. Click to expand.
  </summary>

``` r
######################

# Age distribution by Sex 
data_summary <- data %>%
  group_by(Age, Sex) %>%
  summarise(Count = n(), .groups = 'drop')

age_distribution_sex <- ggplot(data_summary, aes(x = Age, y = ifelse(Sex == "M", -Count, Count), fill = Sex, width=1)) +
  geom_bar(stat="identity", color="#000000") +
  coord_flip() + 
  scale_y_continuous(labels = abs) + 
  labs(title = "", x = "Age", y = "Number of Patients") +
  scale_fill_manual(values = c("M" = "#399ce3", "F" = "#ff7f0e")) +
  theme_classic() + 
  guides(fill = guide_legend(reverse = TRUE, position = "bottom"))

######################

# Resting electrocardiogram results
data$RestingECG <- factor(data$RestingECG, levels = c("ST", "LVH", "Normal"))

resting_ecg_plot <- ggplot(data, aes(x=HeartDisease, fill=RestingECG)) + 
    geom_bar(color = "black", position="fill") + 
    theme_classic() + 
    scale_y_continuous(labels = scales::percent) +
    ylab("") + 
    scale_fill_manual(name = "", values = c("Normal" = "#446455", "LVH" = "#FDD262", "ST" = "#D3DDDC" ), label = c("ST" = "ST-T wave abnormality", "LVH" = "Left ventricular hypertrophy")) +
    scale_x_discrete(labels=c('No Heart Disease', 'Heart Disease')) +
    theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "top") +
    guides(fill=guide_legend(nrow=3,byrow=TRUE))

######################

# Chest pain type 
data$ChestPainType <- factor(data$ChestPainType, levels = c("TA", "ATA", "NAP", "ASY"))

chest_pain_plot <- ggplot(data, aes(x=HeartDisease, fill=ChestPainType)) + 
    geom_bar(color = "black", position="fill") + 
    theme_classic() + 
    scale_y_continuous(labels = scales::percent) +
    ylab("") + 
    scale_fill_manual(name = "", values = c("ASY" = "#446455", "NAP" = "#FDD262", "ATA" = "#D3DDDC", "TA" = "#C7B19C"),
                      label = c("ASY" = "Asymptomatic", "NAP" = "Non-Anginal Pain", "ATA" = "Atypical Angina", "TA" = "Typical Angina", ""))+
    scale_x_discrete(labels=c('No Heart Disease', 'Heart Disease')) +
    theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "top") +
    guides(fill=guide_legend(nrow=4,byrow=TRUE))

######################

# Resting blood pressure
custom_colors <- c("#0c7d34", "#bc5090")
restingBP_boxplot <- ggplot(data, aes(x=HeartDisease, y=RestingBP, fill = HeartDisease)) + 
    geom_boxplot() + 
    ylab("Resting blood pressure mmHg") + 
    scale_fill_manual(name = "Heart Condition", values = custom_colors, labels = c("No", "Yes")) +
    theme_classic() + 
    theme(legend.position = "bottom", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

######################

# Maximum heart rate achieved
max_heart_rate_boxplot <- ggplot(data, aes(x=HeartDisease, y=MaxHR, fill = HeartDisease)) + 
    geom_boxplot() + 
    scale_fill_manual(name = "Heart Condition", values = custom_colors, labels = c("No", "Yes")) +
    theme_classic() + 
    ylab("Maximum heart rate achieved ") + 
    theme(legend.position = "bottom", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

######################
# Slope of the peak exercise ST segment
st_slope_plot <- ggplot(data, aes(x=HeartDisease, fill=ST_Slope)) + 
    geom_bar(color = "black", position="fill") + 
    theme_classic() + 
    scale_y_continuous(labels = scales::percent) +
    ylab("") + 
    scale_fill_manual(name = "", values = c("Up" = "#446455", "Flat" = "#FDD262", "Down" = "#D3DDDC" ), label = c("Up" = "Upsloping", "Down" = "Downsloping")) +
    scale_x_discrete(labels=c('No Heart Disease', 'Heart Disease')) +
    theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "top") +
    guides(fill=guide_legend(nrow=3,byrow=TRUE))

######################
# Fasting blood sugar
  ## No Heart Disease samples
fasting_normal <- data[data$HeartDisease == "0",]$FastingBS
fasting_normal <- data.frame("Heart Condition" = fasting_normal)

palette <- c("#0c7d34", "#bc5090")

fasting_plot_0 <- ggplot(fasting_normal, aes(x = Heart.Condition, fill = Heart.Condition)) +
  geom_bar(colour = "black") +
  theme_classic() + 
  ylim(0,375) +
  ylab("Patients with fasting blood sugar <= 120 mg/dl") +
  scale_fill_manual(name = "Heart Condition", values = palette, labels = c("No", "Yes")) +
  theme(plot.title = element_text(hjust = 0.5, size = 7),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "bottom")

  ## Heart Disease samples
fasting_disease <- data[data$HeartDisease == "1",]$FastingBS
fasting_disease <- data.frame("Heart Condition" = fasting_disease)

fasting_plot_1 <- ggplot(fasting_disease, aes(x = Heart.Condition, fill = Heart.Condition)) +
  geom_bar(colour = "black") +
  theme_classic() + 
  ylim(0,375) +
  ylab("Patients with fasting blood sugar > 120 mg/dl") +
  scale_fill_manual(name = "Heart Condition", values = palette, labels = c("No", "Yes")) +
  theme(plot.title = element_text(hjust = 0.5, size = 7),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "bottom")

# Exercice Angina
  ## No Heart Disease samples
exercice_angina_0 <- data[data$HeartDisease == "0",]$ExerciseAngina
exercice_angina_0 <- data.frame("Heart Condition" = exercice_angina_0)

palette <- c("#0c7d34", "#bc5090")

exercice_angina_plot_0 <- ggplot(exercice_angina_0, aes(x = Heart.Condition, fill = Heart.Condition)) +
  geom_bar(colour = "black") +
  ylim(0,375) +
  ylab("Patients without exercise-induced angina") + 
  theme_classic() + 
  scale_fill_manual(name = "Heart Condition", values = palette, labels = c("No", "Yes")) +
  theme(plot.title = element_text(hjust = 0.5, size = 7),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "bottom")

  ## Heart Disease samples
exercice_angina_1 <- data[data$HeartDisease == "1",]$FastingBS
exercice_angina_1 <- data.frame("Heart Condition" = exercice_angina_1)

exercice_angina_plot_1 <- ggplot(exercice_angina_1, aes(x = Heart.Condition, fill = Heart.Condition)) +
  geom_bar(colour = "black") + 
  ylim(0,375) +
  ylab("Patients with exercise-induced angina") + 
  theme_classic() + 
  scale_fill_manual(name = "Heart Condition", values = palette, labels = c("No", "Yes")) +
  theme(plot.title = element_text(hjust = 0.5, size = 7),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "bottom")

######################
# OldPeak - Numeric value measured in depression
palette <- c("#0c7d34", "#bc5090")

Oldpeak_0 <- data.frame(data$Oldpeak, data$HeartDisease)

Oldpeak_plot <- ggplot(data=Oldpeak_0, aes(x=data.Oldpeak, group=data.HeartDisease, fill=data.HeartDisease)) +
    geom_density(adjust=1, alpha=0.5) +
    theme_classic() + 
    scale_fill_manual(name = "Heart Condition", values = palette) + 
    labs(title = "", x = "ST depression value mesured", y = "") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_fill_manual(name = "Heart Condition", values = custom_colors, labels = c("No", "Yes"))

######################
# Merge plots

boxplots <- ggarrange(restingBP_boxplot, max_heart_rate_boxplot, ncol=2, nrow=1, common.legend = TRUE, legend="bottom", labels = c("B", "C")) 
fasting <- ggarrange(fasting_plot_0, fasting_plot_1, ncol=3, nrow=1, common.legend = TRUE, legend="none")
angina <- ggarrange(exercice_angina_plot_0, exercice_angina_plot_1, ncol=2, nrow=1, common.legend = TRUE, legend="right")
```
</details>
<br /> 

**Figures**:

- **A.** Age distribution by sex
- **B.** Resting blood pressure \[mm Hg\]
- **C.** Maximum heart rate achieved \[Beats per minute\]
- **D.** Count of patients with fasting blood sugar above and below 120
  mg/dl
- **E.** Count of patients with and without exercise-induced angina
- **F.** Distribution of chest pain type (%)
- **G.** Distribution of resting electrocardiogram results \[Normal:
  Normal, ST: having ST-T wave abnormality (T wave inversions and/or ST
  elevation or depression of \> 0.05 mV), LVH: showing probable or
  definite left ventricular hypertrophy by Estes’ criteria\] (%)
- **H.** Distribution of the slope of the peak exercise ST segment (%)
- **I.** Density of oldpeak ST \[Numeric value measured in depression\]

``` r
# Adding labels
ggarrange(age_distribution_sex, boxplots, ncol = 2, nrow = 1, labels = c("A"))
```

![](https://github.com/Antonin-w/Heart_Failure_Prediction_with_R/blob/e03e32ba66a6f35d47b223cc78ab9d34b08edd0a/heart_failure_prediction_files/figure-gfm/unnamed-chunk-8-1.png?raw=true)<!-- -->

``` r
ggarrange(fasting, angina, ncol=2, nrow=1, labels = c("D", "E")) 
```

![](https://github.com/Antonin-w/Heart_Failure_Prediction_with_R/blob/e03e32ba66a6f35d47b223cc78ab9d34b08edd0a/heart_failure_prediction_files/figure-gfm/unnamed-chunk-8-2.png?raw=true)<!-- -->

``` r
ggarrange(chest_pain_plot, resting_ecg_plot, st_slope_plot, ncol=3, nrow=1, labels = c("F", "G", "H"))
```

![](https://github.com/Antonin-w/Heart_Failure_Prediction_with_R/blob/e03e32ba66a6f35d47b223cc78ab9d34b08edd0a/heart_failure_prediction_files/figure-gfm/unnamed-chunk-8-3.png?raw=true)<!-- -->

``` r
ggarrange(Oldpeak_plot, ncol=1, nrow=1, labels = c("I"))
```

![](https://github.com/Antonin-w/Heart_Failure_Prediction_with_R/blob/e03e32ba66a6f35d47b223cc78ab9d34b08edd0a/heart_failure_prediction_files/figure-gfm/unnamed-chunk-8-4.png?raw=true)<!-- -->

At first glance, we can clearly see a surepresentation of Male within
the data, and a sample age tending toward 55 years old (Fig. A). The
boxplot showing the resting blood pressure (Fig. B) doesn’t visually
give the impression having a link with heart diseases, contrary to the
plot of the maximum heart rate achieved (Fig. C). To confirm or infirm
our observation, we can statistically prove or not the correlation.

Before that, we have to test if the Resting blood pressure results and
maximum heart rate achieved follow a normal distribution.

``` r
shapiro.test(data$RestingBP) 
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  data$RestingBP
    ## W = 0.97127, p-value = 1.743e-12

``` r
shapiro.test(data$MaxHR)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  data$MaxHR
    ## W = 0.99267, p-value = 0.0001683

Both p-value are below 0.05, so we can conclude they do not follow a
normal distribution, and we will have to perform non-parametric
correlation test (Spearman).

``` r
cor.test(data$RestingBP, as.numeric(data$HeartDisease), method = "spearman", exact = FALSE)
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  data$RestingBP and as.numeric(data$HeartDisease)
    ## S = 113979002, p-value = 0.0004284
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.1160075

``` r
cor.test(data$MaxHR, as.numeric(data$HeartDisease), method = "spearman", exact = FALSE)
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  data$MaxHR and as.numeric(data$HeartDisease)
    ## S = 181133616, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.4048268

The hypothesis H0 for cor.test is : There is no association between the
two variables. However, in both case, p-value \< 0.05, so we rejet the
nul hypothesis. It can be concluded that both RestingBP and MaxHR do
have a correclation with heart diseases. The correlation coefficient is
around 0.12 for RestingBP, and -0.40 for MaxHR. Therefore, MaxHR has a
higher impact on heart diseases than RestingBP.

A higher amount of patients can be seen with high fasting blood sugar
\[\>120 mg/dl\] and exercice-induces angina among heart disease victims
(Fig. D and Fig. E), both could be an important factor.

From Fig. F, patients affected by heart diseases don’t have any chest
pain (asymptomatic) and generally a flat slope of the peak exercise
(Fig. H).

Finally, Fig. I indicates that most of the patients without heart
condition hasn’t ST depression on their electrocardiogram results, when
the ST segment is abnormally low below the baseline, it could be linked
with heart diseases.

To have a better understanding if some variable are correlated between
each others, we can generate a correlation matrix and observe the
correlation values between each conditions.

``` r
######################
# Correlation matrix

table <- data.frame(as.numeric(data$Sex), data$Age, data$RestingBP, data$MaxHR, data$Oldpeak, as.numeric(data$FastingBS), as.numeric(data$ExerciseAngina), as.numeric(data$RestingECG))
names(table) <- c("Sex", "Age", "Resting BP", "Max HR", "Oldpeak", "FastingBS", "Exercice Angina", "Resting ECG")
red <- cor(table)
correlation_matrix <- corrplot(red, method="color", type="lower", addCoef.col = T,
         col = colorRampPalette(c("#446455", "#FDD262", "#D3DDDC"))(100),
         tl.col = "black", tl.srt = 45)
```

![](https://github.com/Antonin-w/Heart_Failure_Prediction_with_R/blob/e03e32ba66a6f35d47b223cc78ab9d34b08edd0a/heart_failure_prediction_files/figure-gfm/unnamed-chunk-11-1.png?raw=true)<!-- -->

# Machine learning models

## Linear regression

### Preparing data

``` r
data_linear <- data %>%
  mutate(HeartDisease = ifelse(HeartDisease == 1, TRUE, FALSE))
```

Before training any model, the dataset has to be split in two, 80% of
the dataset will be used to train the model, and the other 20% will
serve to test it.

``` r
eighty_percents <- round(nrow(data_linear) * 0.2)  # Calculate how many sample represent 20% of the dataset
choice <-  sample(1:nrow(data_linear), size = eighty_percents, replace = F) # Randomly selecting 20% of the number of patient

data.test <- data_linear[choice, ] # Keep 20% of the dataset
data.train <- data_linear[-choice,] # Keep the other 80% of the dataset
```

### Creating model

``` r
linear_reg_all <- lm(HeartDisease ~ ., data = data.train)
```

``` r
data.test$pred = predict(linear_reg_all, data.test) > 0.5
confusion_matrix_linear_reg_all <- table(data.test$HeartDisease, data.test$pred)

stats_linear_reg_all <- confusionMatrix(confusion_matrix_linear_reg_all)

stats_linear_reg_all$overall
```

    ##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
    ##   8.369565e-01   6.630448e-01   7.754995e-01   8.872100e-01   6.141304e-01 
    ## AccuracyPValue  McnemarPValue 
    ##   3.771024e-11   2.012426e-01

``` r
stats_linear_reg_all$byClass
```

    ##          Sensitivity          Specificity       Pos Pred Value 
    ##            0.8450704            0.8318584            0.7594937 
    ##       Neg Pred Value            Precision               Recall 
    ##            0.8952381            0.7594937            0.8450704 
    ##                   F1           Prevalence       Detection Rate 
    ##            0.8000000            0.3858696            0.3260870 
    ## Detection Prevalence    Balanced Accuracy 
    ##            0.4293478            0.8384644

The best model isn’t necessary the one with the best accuracy, because
it’s gonna be always one with all the factors, we can try to simplify as
much as we can the model by removing the factor not bringing enough
information to help for the prediction.

From the package `olsrr` (similar to `leaps`), the funcion
ols_step_best_subset will perform all the combination and return the
best predictors. The Mallows’s C(p) is a good indicator to know which
model choose at the end, balancing between complexity and accuracy.

``` r
ols_step_best_subset(linear_reg_all)
```

    ##                                          Best Subsets Regression                                         
    ## ---------------------------------------------------------------------------------------------------------
    ## Model Index    Predictors
    ## ---------------------------------------------------------------------------------------------------------
    ##      1         ST_Slope                                                                                   
    ##      2         ChestPainType ST_Slope                                                                     
    ##      3         Sex ChestPainType ST_Slope                                                                 
    ##      4         Sex ChestPainType ExerciseAngina ST_Slope                                                  
    ##      5         Sex ChestPainType FastingBS ExerciseAngina ST_Slope                                        
    ##      6         Sex ChestPainType FastingBS ExerciseAngina Oldpeak ST_Slope                                
    ##      7         Sex ChestPainType FastingBS MaxHR ExerciseAngina Oldpeak ST_Slope                          
    ##      8         Age Sex ChestPainType FastingBS MaxHR ExerciseAngina Oldpeak ST_Slope                      
    ##      9         Age Sex ChestPainType FastingBS RestingECG MaxHR ExerciseAngina Oldpeak ST_Slope           
    ##     10         Age Sex ChestPainType RestingBP FastingBS RestingECG MaxHR ExerciseAngina Oldpeak ST_Slope 
    ## ---------------------------------------------------------------------------------------------------------
    ## 
    ##                                                      Subsets Regression Summary                                                     
    ## ------------------------------------------------------------------------------------------------------------------------------------
    ##                        Adj.        Pred                                                                                              
    ## Model    R-Square    R-Square    R-Square      C(p)        AIC          SBIC         SBC         MSEP       FPE       HSP      APC  
    ## ------------------------------------------------------------------------------------------------------------------------------------
    ##   1        0.3714      0.3697      0.3659    322.0903    725.5991    -1360.7880    743.9931    114.5505    0.1567    2e-04    0.6320 
    ##   2        0.4844      0.4808      0.4744    139.3975    586.2117    -1503.7212    618.4013     94.0954    0.1292    2e-04    0.5199 
    ##   3        0.5164      0.5124      0.5055     87.8768    541.1281    -1548.6219    577.9162     88.3701    0.1216    2e-04    0.4889 
    ##   4        0.5393      0.5349      0.5273     51.5675    507.4698    -1581.9984    548.8564     84.2953    0.1161    2e-04    0.4670 
    ##   5        0.5602      0.5554      0.5475     18.6359    475.3728    -1613.6393    521.3579     80.5797    0.1111    2e-04    0.4470 
    ##   6        0.5651      0.5597      0.5513     12.4283    469.1261    -1619.7299    519.7097     79.7892    0.1102    2e-04    0.4432 
    ##   7        0.5679      0.5619      0.5528      9.8999    466.5362    -1622.1979    521.7183     79.4011    0.1098    1e-04    0.4417 
    ##   8        0.5692      0.5627      0.5529      9.5958    466.1898    -1622.4525    525.9704     79.2570    0.1098    1e-04    0.4415 
    ##   9        0.5695      0.5617      0.5505     13.1321    469.7166    -1620.8725    538.6943     79.3155    0.1101    2e-04    0.4424 
    ##  10        0.5696      0.5612       0.549     15.0000    471.5818    -1618.9619    545.1579     79.4108    0.1104    2e-04    0.4435 
    ## ------------------------------------------------------------------------------------------------------------------------------------
    ## AIC: Akaike Information Criteria 
    ##  SBIC: Sawa's Bayesian Information Criteria 
    ##  SBC: Schwarz Bayesian Criteria 
    ##  MSEP: Estimated error of prediction, assuming multivariate normality 
    ##  FPE: Final Prediction Error 
    ##  HSP: Hocking's Sp 
    ##  APC: Amemiya Prediction Criteria

The best model following Mallows’s C(p) include only the OldPeak factor
instead of the 10 of the previous model.

``` r
linear_reg_1 <- lm(HeartDisease ~ ST_Slope, data = data.train)

data.test$pred = predict(linear_reg_1, data.test) > 0.5
confusion_matrix_linear_reg_1 <- table(data.test$HeartDisease, data.test$pred)

stats_linear_reg_1 <- confusionMatrix(confusion_matrix_linear_reg_1)

stats_linear_reg_1$overall
```

    ##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
    ##   8.423913e-01   6.758202e-01   7.815544e-01   8.918348e-01   5.978261e-01 
    ## AccuracyPValue  McnemarPValue 
    ##   5.705698e-13   4.576141e-01

``` r
stats_linear_reg_1$byClass
```

    ##          Sensitivity          Specificity       Pos Pred Value 
    ##            0.8378378            0.8454545            0.7848101 
    ##       Neg Pred Value            Precision               Recall 
    ##            0.8857143            0.7848101            0.8378378 
    ##                   F1           Prevalence       Detection Rate 
    ##            0.8104575            0.4021739            0.3369565 
    ## Detection Prevalence    Balanced Accuracy 
    ##            0.4293478            0.8416462

``` r
accuracy_linear <- stats_linear_reg_1$overall[1]
results_byClass <- data.frame(stats_linear_reg_1$byClass)
```

The performances statistics of the model are quite similar to the
previous one while removing 9 factors.

## k-Nearest Neighbors

### Preparing data

As we have qualitative values in our dataset, we’d like to convert them
in dummy variables. But before that, let’s scale the variable Age,
RestingBP, MaxHR and OldPeak because they aren’t on the same metrics and
could be a problem for the knn algorithm.

``` r
data[,c("Age", "RestingBP", "MaxHR", "Oldpeak")] <- scale(data[,c("Age", "RestingBP", "MaxHR", "Oldpeak")])

dummy_two <- dummy_cols(data, select_columns = c("Sex", "FastingBS", "ExerciseAngina", "HeartDisease"), remove_first_dummy = TRUE) # Dummy code variables that have just two levels
dummy_three_plus <- dummy_cols(data, select_columns = c("ChestPainType", "RestingECG", "ST_Slope")) # Dummy code variables that have three levels or plus


# Creation of a dataframe with all factors represented as dummy variables: 
dummy_data <- data.frame(dummy_two$Sex_M, 
                         dummy_two$Age, 
                         dummy_two$FastingBS_1,
                         dummy_two$ExerciseAngina_Y,
                         
                         data$RestingBP,
                         data$MaxHR,
                         data$Oldpeak,
                         
                         dummy_three_plus$ChestPainType_ASY,
                         dummy_three_plus$ChestPainType_ATA,
                         dummy_three_plus$ChestPainType_NAP,
                         dummy_three_plus$ChestPainType_TA,
                         dummy_three_plus$RestingECG_LVH,
                         dummy_three_plus$RestingECG_Normal,
                         dummy_three_plus$RestingECG_ST,
                         dummy_three_plus$ST_Slope_Down,
                         dummy_three_plus$ST_Slope_Flat,
                         dummy_three_plus$ST_Slope_Up
                         )
```

Same as before, we have to split the dataset in training and testing
sets.

``` r
eighty_percents <- round(nrow(dummy_data) * 0.2)  # Calculate how many sample represent 20% of the dataset
choice <-  sample(1:nrow(dummy_data), size = eighty_percents, replace = F) # Randomly selecting 20% of the number of patient

data.test <- dummy_data[choice, ] # Keep 20% of the dataset
data.train <- dummy_data[-choice,] # Keep the other 80% of the dataset

disease.test <- data$HeartDisease[choice]
disease.train <- data$HeartDisease[-choice]
```

### Creating model

To start with, it is recommended to use k (Number of Nearest Neighbors
used for the prediction) as `k = sqrt(N)/2`.

``` r
l = round(sqrt(nrow(data.train))/2) # Determine k 

heart_pred_knn <- knn(train = data.train, test = data.test, cl = disease.train, k=l) 

compare.pred <- table(heart_pred_knn, disease.test)

stats_knn_1 <- confusionMatrix(compare.pred)

stats_knn_1$overall
```

    ##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
    ##   8.750000e-01   7.440116e-01   8.183623e-01   9.190843e-01   5.597826e-01 
    ## AccuracyPValue  McnemarPValue 
    ##   2.377624e-20   2.109029e-01

``` r
stats_knn_1$byClass
```

    ##          Sensitivity          Specificity       Pos Pred Value 
    ##            0.8148148            0.9223301            0.8918919 
    ##       Neg Pred Value            Precision               Recall 
    ##            0.8636364            0.8918919            0.8148148 
    ##                   F1           Prevalence       Detection Rate 
    ##            0.8516129            0.4402174            0.3586957 
    ## Detection Prevalence    Balanced Accuracy 
    ##            0.4021739            0.8685725

However, the goal is to play with the k value to see which one best fit
our dataset. To do se, we can use the `train` funcion of the `caret`
package. It’s gonna automatically run the algorithm with a
cross-validation (we keep p = 0.8, 80% train / 20% test) for the
differents k-values we’ll set up (here, from 1 to 20).

``` r
grid = expand.grid(k = seq(1, 20, by = 2)) # From k = 1 to k = 20, only odds. 

pred_knn_caret <- train(dummy_data, data$HeartDisease, method = "knn", preProcess = c("center","scale"),
                    trControl = trainControl(method = 'cv',
                                             search = "grid",
                                             p = 0.8),
                     tuneGrid = grid)
```

``` r
pred_knn_caret
```

    ## k-Nearest Neighbors 
    ## 
    ## 918 samples
    ##  17 predictor
    ##   2 classes: '0', '1' 
    ## 
    ## Pre-processing: centered (17), scaled (17) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 826, 826, 826, 826, 826, 826, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   k   Accuracy   Kappa    
    ##    1  0.8148352  0.6255346
    ##    3  0.8485667  0.6930798
    ##    5  0.8474916  0.6894437
    ##    7  0.8474558  0.6893665
    ##    9  0.8464047  0.6869399
    ##   11  0.8420091  0.6790224
    ##   13  0.8420091  0.6792757
    ##   15  0.8463569  0.6882406
    ##   17  0.8430960  0.6814051
    ##   19  0.8420210  0.6795008
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was k = 3.

``` r
plot(pred_knn_caret)
```

![](https://github.com/Antonin-w/Heart_Failure_Prediction_with_R/blob/e03e32ba66a6f35d47b223cc78ab9d34b08edd0a/heart_failure_prediction_files/figure-gfm/unnamed-chunk-22-1.png?raw=true)<!-- -->
It is recommended to choose an odd value for k to avoid ties and thus NA
values in the predictions.

``` r
heart_pred_knn <- knn(train = data.train, test = data.test, cl = disease.train, k=pred_knn_caret$bestTune) # We put k as pred_knn_caret$bestTune (k with highest accuracy)

compare.pred <- table(heart_pred_knn, disease.test)

stats_knn_2 <- confusionMatrix(compare.pred)

stats_knn_2$overall
```

    ##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
    ##   8.369565e-01   6.656572e-01   7.754995e-01   8.872100e-01   5.597826e-01 
    ## AccuracyPValue  McnemarPValue 
    ##   1.149062e-15   2.012426e-01

``` r
stats_knn_2$byClass
```

    ##          Sensitivity          Specificity       Pos Pred Value 
    ##            0.7654321            0.8932039            0.8493151 
    ##       Neg Pred Value            Precision               Recall 
    ##            0.8288288            0.8493151            0.7654321 
    ##                   F1           Prevalence       Detection Rate 
    ##            0.8051948            0.4402174            0.3369565 
    ## Detection Prevalence    Balanced Accuracy 
    ##            0.3967391            0.8293180

``` r
accuracy_knn <- stats_knn_2$overall[1]
results_byClass <- cbind(results_byClass, as.data.frame(stats_knn_2$byClass))
```

## Random forest

### Preparing data

``` r
eighty_percents <- round(nrow(data) * 0.2)  # Calculate how many sample represent 20% of the dataset
choice <-  sample(1:nrow(data), size = eighty_percents, replace = F) # Randomly selecting 20% of the number of patient

data.test <- data[choice, ] # Keep 20% of the dataset
data.train <- data[-choice,] # Keep the other 80% of the dataset
```

### Creating model

``` r
forest_1 = randomForest(HeartDisease ~ . , data.train, ntree = 1000, mtry = 2)

varImpPlot(forest_1)
```

![](https://github.com/Antonin-w/Heart_Failure_Prediction_with_R/blob/e03e32ba66a6f35d47b223cc78ab9d34b08edd0a/heart_failure_prediction_files/figure-gfm/unnamed-chunk-25-1.png?raw=true)<!-- -->

``` r
plot(forest_1$err.rate[, 1], type = "l", xlab = "Tree number", ylab = "OOB Error")
```

![](https://github.com/Antonin-w/Heart_Failure_Prediction_with_R/blob/e03e32ba66a6f35d47b223cc78ab9d34b08edd0a/heart_failure_prediction_files/figure-gfm/unnamed-chunk-25-2.png?raw=true)<!-- -->

It can be seen that above 500 trees, the OOB Error is globally the same.
To save some time, we will set up ntree = 500.

``` r
forest_2 = randomForest(HeartDisease ~ . , data.train, ntree = 500, mtry = 2)

pred_forest_2 <- predict(forest_2, data.test)
confusionMatrix(pred_forest_2, data.test$HeartDisease)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  0  1
    ##          0 79  8
    ##          1 11 86
    ##                                           
    ##                Accuracy : 0.8967          
    ##                  95% CI : (0.8434, 0.9367)
    ##     No Information Rate : 0.5109          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.7932          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.6464          
    ##                                           
    ##             Sensitivity : 0.8778          
    ##             Specificity : 0.9149          
    ##          Pos Pred Value : 0.9080          
    ##          Neg Pred Value : 0.8866          
    ##              Prevalence : 0.4891          
    ##          Detection Rate : 0.4293          
    ##    Detection Prevalence : 0.4728          
    ##       Balanced Accuracy : 0.8963          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
tunegrid <- expand.grid(.mtry = (1:10)) 

forest_3 <- train(HeartDisease ~ ., data = data.train, method = "rf",
                  trControl = trainControl(method = 'cv', p = 0.8),
                  tuneGrid = tunegrid,
                  metric = 'Accuracy',
                  ntree = 500)

plot(forest_3)
```

![](https://github.com/Antonin-w/Heart_Failure_Prediction_with_R/blob/e03e32ba66a6f35d47b223cc78ab9d34b08edd0a/heart_failure_prediction_files/figure-gfm/unnamed-chunk-27-1.png?raw=true)<!-- -->

``` r
pred_forest_3 <- predict(forest_3, data.test)
confusionMatrix(pred_forest_3, data.test$HeartDisease)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  0  1
    ##          0 76  9
    ##          1 14 85
    ##                                           
    ##                Accuracy : 0.875           
    ##                  95% CI : (0.8184, 0.9191)
    ##     No Information Rate : 0.5109          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.7496          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.4042          
    ##                                           
    ##             Sensitivity : 0.8444          
    ##             Specificity : 0.9043          
    ##          Pos Pred Value : 0.8941          
    ##          Neg Pred Value : 0.8586          
    ##              Prevalence : 0.4891          
    ##          Detection Rate : 0.4130          
    ##    Detection Prevalence : 0.4620          
    ##       Balanced Accuracy : 0.8743          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
forest_4 = randomForest(HeartDisease ~ . , data.train, ntree = 500, mtry = 4)

pred_forest_4 <- predict(forest_4, data.test)
stats_random_forest <- confusionMatrix(pred_forest_4, data.test$HeartDisease)

accuracy_forest <- stats_random_forest$overall[1]
results_byClass <- cbind(results_byClass, as.data.frame(stats_random_forest$byClass))
```

# Results

``` r
results_clean <- results_byClass[c("Precision", "Recall", "F1"),]
results_clean <- as.data.frame(t(results_clean))
results_clean$Accuracy <- c(accuracy_linear, accuracy_knn, accuracy_forest)
rownames(results_clean) <- c("Linear Regression", "Knn", "Random Forest")
```

``` r
kable(results_clean, format = "markdown")
```

|                   | Precision |    Recall |        F1 |  Accuracy |
|:------------------|----------:|----------:|----------:|----------:|
| Linear Regression | 0.7848101 | 0.8378378 | 0.8104575 | 0.8423913 |
| Knn               | 0.8493151 | 0.7654321 | 0.8051948 | 0.8369565 |
| Random Forest     | 0.8965517 | 0.8666667 | 0.8813559 | 0.8858696 |
