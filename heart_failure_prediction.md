Heart Failure Prediction
================
2024-09-19

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
```

# Data importation and curation

``` r
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

Code of the differents plots. Click to expand.

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

![](heart_failure_prediction_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggarrange(fasting, angina, ncol=2, nrow=1, labels = c("D", "E")) 
```

![](heart_failure_prediction_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
ggarrange(chest_pain_plot, resting_ecg_plot, st_slope_plot, ncol=3, nrow=1, labels = c("F", "G", "H"))
```

![](heart_failure_prediction_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
ggarrange(Oldpeak_plot, ncol=1, nrow=1, labels = c("I"))
```

![](heart_failure_prediction_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

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

![](heart_failure_prediction_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

# Model creation
