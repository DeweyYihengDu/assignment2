# The meta-analysis about fish behaviors
## The data and required packages

### Data
The original data has three parts, 
one data is from the material of the previous workshop, 
another data is from the data of the meta-analysis and a related information of the article about the data of the workshop.

### Required packages 

The required packages:
```
library(tidyverse)
library(metafor)
library(orchaRd)
library(here)
library(pacman)
library(flextable)
```

## Tidy the data

1. Read the data.
1. Get the `sd`, `mean`, `n` from the origin data. (Calculated by the function created by myself)
    ```
    tidydata <- function(i){
      control <- data1 %>% 
        filter(species == i) %>%
        filter(treatment=="control") %>% 
        select(activity) %>% 
        drop_na()
      oa <- data1 %>% 
        filter(species == i) %>%
        filter(treatment=="CO2") %>% 
        select(activity) %>% 
        drop_na()
      Species = i
      ctrl.sd = sd(control$activity)
      ctrl.mean=mean(control$activity)
      ctrl.n=nrow(control)
      oa.sd = sd(oa$activity)
      oa.mean=mean(oa$activity)
      oa.n=nrow(oa)
      data.frame(Species,ctrl.mean,ctrl.sd,ctrl.n, oa.mean, oa.sd, oa.n)
    }
    a=rbind(tidydata("ambon"),tidydata("lemon"),tidydata("chromis"),tidydata("acantho"),tidydata("humbug"),tidydata("whitedams"))
    ```
1. Merge the combined summary statistics and metadata from Clark et al. (2020) (output from 1 & 2) into the larger meta-analysis dataset 
1. tidy the data and reneme the colnames.
    ```
    colnames(meta_data)[3] <- 'Year_online'
    colnames(meta_data)[4] <- 'Year_print'
    colnames(meta_data)[11] <- 'Climate'
    colnames(meta_data)[15] <- 'Life_stage'
    colnames(meta_data)[10] <- "Effect_type"
    meta_data <- meta_data %>% mutate(residual = 1:n())
    ```
## Start the analysis

### Get the log response ration(LnRR) Use the function `escalc` and the `measure = "ROM"`

### Create meta-analytic model fitted to the data that controls for the sampling variance of lnRR.

### Analyze the result of  the meta-regression.

* 95% Confidence Intervals in this model
* Measures of heterogeneity in effect size estimates across studies ($I^2$) and `predict()`

### The forest plot about the meta analysis

Use the function `orchaRd::orchard_plot` and the model chose the 1, Climate, Lify_style, and Effect_type.

The overall effect on fish is negative.

+ Fig1 Orchard plot of the main model
+ Fig2 Orchard plot about the Climate
+ Fig3 Orchard plot of the meta-regression model about Life Stage
+ Fig4 Orchard plot of the meta-regression model about effect type

### Visual inspections of possible publication bias – funnel plots

Use 2 types bias-funnel plots to find the relationship between Years and the published possiblity.

+ Fig5 Funnel plot showing the precision of effects against their correlation
+ Fig 6 The plot of Sampling Variance of LnRR against LnRR

### Fitting a Multilevel Meta-Regression model to Test and Correct for Publication bias.

+ Fig7 Plot of LnRR as a function of publication year

### Formal meta-regression model that includes year as a moderator (fixed effect) to test for time-lag bias

```
lastone <- rma.mv(LnRR ~ 1/(v*LnRR), V = v, random = list(~1 | Study), test = "t", dfs = "contain", data = arnold_data_endo)
summary(lastone)
```

Use the v and LnRR as mators to formal meta-regression model.
From the result of meta-regression model, we can get the estimate is very little so that it is helpful for the modelling.

## Discussion

### The result of meta-analysis

There appears to be a temporal publication bias in the data. 
In the beginning it may have been due to the low importance given to the results of studies with small samples, which led to a large sample size in some of the first published articles. As time progresses, the experimental samples and effects gradually tend toward the mean. 
The large variance in the first few years may be due to the immaturity of the experimental methods and techniques, resulting in large experimental results.

Climate and life stage are also the publication biases of such studies. 
Among the climates, deep climate has the most bias. 
The reason for the higher bias in deep may be that there are more species studied in this category and there is a better research base. 
There are two possible reasons for more deviations in juveniles: one is that juveniles are more sensitive to external influences, and the other is that juveniles are easier to study and more obvious results can easily appear.

The effect type has a small effect on publication, and the number of publications is high for both positive and negative effects and no effect. 
The reason for this may be that this type of research is still in the development stage and no traditional view has been formed. All types of studies are in an equal state and the acceptance of their findings is high.

### Compare with a meta-analysis by others.

At the study of fish behavior, we can find many publication biases at present, these include years, climate and life stage. Compared with the result of Cleement et. al.(2022), I add the years to this deviation during the model of meta-regression analysis. From the Years of the possibility of publication bias, the bias is larger in the earlier years, but decreases as the years increase, which represents that such studies are becoming more standardized and the results do not produce more variation.

The results are consistent with his study in that there is a large deviation in the direction of climate and life stage. It is possible that the publication bias is influenced by the simplicity of the studies on the non-larval stage or the greater experience in pre-coldwater fish studies.

There are two possibilities for the lack of publication bias in effect style: one is that the research is not yet mature and people have not yet formed traditional ideas about this type of research, and the other possibility is that the research is already very mature and there is more inclusiveness about the results of this type of research, so there is no publication bias about effect style.

And combined with the above discussion, we can learn that the current research at this stage is still in the development stage, without the formation of a more solid traditional concept. There is still specificity in the selection of some, for example, climate and life style, so there are large deviations in these areas. If this situation persists, it may result in some duplicate or similar experiments, which may create a waste. And under the influence of some, for example, ocean acidification, no significant deviations have been found for such effects.

In general, the majority of the results show that CO2 rise reduces the activity of fish, but there are more differences between different climates or life styles. Therefore, in future studies, we should focus on different species, different living environments, and different life states to achieve additional clarification in this field.

## Reference
