# Third Facemask Study 

## Setting up the environment
setwd("/Users/pengjunning/Desktop/Project Mask/third study")
library(readr)
library(effsize)
library(afex)
library(emmeans)
library(sjPlot)
library(lme4)
library(lmerTest)
library(nlme)
library(performance)
library(tidyverse)

## Import data
raw_data_third <- read.csv("Experiment_3.csv")
raw_data_third$Sex <- factor(raw_data_third$Sex, 
                               levels = c("Male", "Female"))

## Clean data set
clean_data_third <- raw_data_third %>% ## Identify whether participants selected the masked picture
  mutate(main_task_choice = if_else(main_task == 1, Pic.Label.1, Pic.Label.2)) %>% 
  mutate(main_task_choice = case_when(
    endsWith(main_task_choice, "U") ~ "unmask",
    endsWith(main_task_choice, "M") ~ "mask"
  )) %>% ## Check the checking quesitons - the amount should be the same with the check quesiton
  mutate(understanding = if_else(check_question == Amount, 
                                       "Correctly understand", "Did not undersand")) %>% 
  filter(understanding == "Correctly understand") %>% ## Figuring out whether particiapnt offered £20 to masked person
  mutate(whether_mask_got_20 = ifelse(Amount == "£10" & main_task_choice == "unmask", "Yes", 
                                      ifelse(Amount == "£20" & main_task_choice == "mask", 
                                             "Yes", "No"))) %>% 
  mutate(mask_got_20 = case_when(## turning it into categorical values
    startsWith(whether_mask_got_20, "Y") ~ 1,
    startsWith(whether_mask_got_20, "N") ~ 0))

clean_data_third$main_task_choice <- factor(clean_data_third$main_task_choice, 
                                            levels = c("mask", "unmask"))
clean_data_third$Sex <- factor(clean_data_third$Sex, 
                               levels = c("Male", "Female"))
clean_data_third$whether_mask_got_20 <- factor(clean_data_third$whether_mask_got_20, 
                                               levels = c("Yes", "No"))

clean_data_third$Amount <- factor(clean_data_third$Amount, 
                                  levels = c("£10", "£20"))

clean_data_third$mask_perception <-  factor(clean_data_third$mask_perception,levels = c("Equally themselves and others",
                                                                                        "Only themselves", "Mostly themselves", 
                                                                                        "Only others", "Mostly others"))
clean_data_third$mask_wearing <- factor(clean_data_third$mask_wearing, 
                                  levels = c("Yes", "No"))



## Running logistic regression with 0 predictor
set_sum_contrasts()
r0 <-  glm(mask_got_20 ~ 1, family = "binomial", data = clean_data_third)
summary(r0)

# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.5810  -1.5810   0.8218   0.8218   0.8218  

# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   0.9122     0.1691   5.393 6.91e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

#     Null deviance: 204.87  on 170  degrees of freedom
# Residual deviance: 204.87  on 170  degrees of freedom
# AIC: 206.87

# Number of Fisher Scoring iterations: 4

## Coverting log-odds into odds ratio
exp(coef(r0))
# (Intercept) 
#    2.489796 

# A more detailed table
table <- clean_data_third %>%
  group_by(mask_got_20) %>%
  summarise(freq=n(),prob=(n()/nrow(.)),odds=prob/(1-prob), logodds=log(odds)) %>%
  round(.,5)

# A tibble: 2 x 5
#   mask_got_20  freq  prob  odds logodds
#         <dbl> <dbl> <dbl> <dbl>   <dbl>
# 1           0    49 0.287 0.402  -0.912
# 2           1   122 0.713 2.49    0.912

# The probability of masked picture receiving £20 is p = 0.713

# The odds of the probability of masked picture receiving £20 is Odds = 2.49, 
# which means that participants in our experiment are 2.49 times more likely 
# to offer £20 to a masked picture than offer to a unmasked picture

# The log odds of the probability of masked picture receiving £20 log(O) = 0.91.-- the intercept


## Running logistic regression with 1 predictor -- Sex 
set_sum_contrasts()
r1 <-  glm(mask_got_20 ~ 1, family = "binomial", data = clean_data_third)
summary(r1)

# contr.sum(2)
# contrasts(clean_data_third$Sex) = contr.sum(2)
# contrasts(clean_data_third$Amount) = contr.sum(2)

r1 <-  glm(mask_got_20 ~ Sex, family = "binomial", data = clean_data_third)
summary(r1)
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.6169  -1.5468   0.7944   0.8485   0.8485  

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   0.9139     0.1694   5.396  6.8e-08 ***
# Sex1          0.0777     0.1694   0.459    0.646    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 204.87  on 170  degrees of freedom
# Residual deviance: 204.66  on 169  degrees of freedom
# AIC: 208.66

# Number of Fisher Scoring iterations: 4

## Coverting log-odds into odds ratio
exp(coef(r1))
# (Intercept)   SexFemale 
#    2.494140    1.080794

# A more detailed table
table01 <- clean_data_third %>%
  mutate(sex_num = ifelse(Sex == "Female", 1, 0)) %>%
  group_by(sex_num,mask_got_20 ) %>%
  summarise(freq=n()) %>%
  mutate(all=sum(freq),prob=freq/all,odds=prob/(1-prob),logodds=log(odds)) %>%
  round(.,5)

#   sex_num mask_got_20  freq   all  prob  odds logodds
#     <dbl>       <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
# 1    (M)0           0    23    85 0.271 0.371  -0.992
# 2       0           1    62    85 0.729 2.70    0.992
# 3    (F)1           0    26    86 0.302 0.433  -0.836
# 4       1           1    60    86 0.698 2.31    0.836


## Running logistic regression with 2 predictors -- Sex, Amount allocation scenarios 
set_sum_contrasts()
r2 <-  glm(mask_got_20 ~ Sex + Amount, family = "binomial", data = clean_data_third)
summary(r2)

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.91823    0.16998   5.402 6.59e-08 ***
# Sex1         0.07361    0.16976   0.434    0.665    
# Amount1      0.06495    0.17021   0.382    0.703    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

confint(r2)

tab_model(r2, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)

## Coverting log-odds into odds ratio
exp(coef(r2))
# (Intercept)        Sex1     Amount1 
#    2.504864    1.076386    1.067104 

# A more detailed table
table02 <- clean_data_third %>%
  mutate(sex_num = ifelse(Sex == "Female", 1, 0)) %>%
  mutate(amount_num = ifelse(Amount == "£20", 1, 0)) %>%
  group_by(sex_num, amount_num, mask_got_20) %>%
  summarise(freq=n()) %>%
  mutate(all=sum(freq),prob=freq/all,odds=prob/(1-prob),logodds=log(odds)) %>%
  round(.,5)

#   sex_num amount_num mask_got_20  freq   all  prob  odds logodds
#     <dbl>      <dbl>       <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
# 1    (M)0     (£10)0           0    11    43 0.256 0.344  -1.07 
# 2       0          0           1    32    43 0.744 2.91    1.07 
# 3       0     (£20)1           0    12    42 0.286 0.4    -0.916
# 4       0          1           1    30    42 0.714 2.5     0.916
# 5    (F)1          0           0    11    38 0.289 0.407  -0.898
# 6       1          0           1    27    38 0.711 2.45    0.898
# 7       1          1           0    15    48 0.312 0.455  -0.788
# 8       1          1           1    33    48 0.688 2.2     0.788


## Exploratory section -- adding participant's age and two survey questions into the logistic regression 
set_sum_contrasts()
r3 <-  glm(mask_got_20 ~ Sex + Amount + age, family = "binomial", data = clean_data_third)
summary(r3)

# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)  
# (Intercept)  1.117587   0.531110   2.104   0.0354 *
# Sex1         0.073451   0.169852   0.432   0.6654  
# Amount1      0.065496   0.170305   0.385   0.7005  
# age         -0.004856   0.012201  -0.398   0.6906  
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tab_model(r3, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)

exp(coef(r3))

set_default_contrasts()
set_sum_contrasts()
r4 <-  glm(mask_got_20 ~ Sex + Amount + mask_perception , family = "binomial", data = clean_data_third)
summary(r4)
confint(r4)
tab_model(r4, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)

# glm(formula = mask_got_20 ~ Sex + Amount + mask_wearing, family = "binomial", 
#    data = clean_data_third)

# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    1.50966    0.27831   5.424 5.82e-08 ***
# Sex1           0.13040    0.17904   0.728 0.466395    
# Amount1        0.07623    0.17914   0.426 0.670433    
# mask_wearing1  1.02371    0.27780   3.685 0.000229 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

exp(coef(r4))
# (Intercept)          Sex1       Amount1 mask_wearing1 
#   4.525197      1.139287      1.079214      2.783507 

em1 <-  emmeans(r4, "mask_wearing") %>% pairs(adjust = "holm")
# mask_wearing emmean    SE  df asymp.LCL asymp.UCL
# Yes           2.599 0.531 Inf    1.5577     3.641
# No            0.474 0.191 Inf    0.0987     0.849

# Results are averaged over the levels of: Sex, Amount 
# Results are given on the logit (not the response) scale. 
# Confidence level used: 0.95 

# contrast estimate    SE  df z.ratio p.value
# Yes - No     2.13 0.565 Inf 3.760   0.0002 

# Results are averaged over the levels of: Sex, Amount 
# Results are given on the log odds ratio (not the response) scale. 

## Adding the rating difference as a predictor to the binary choice
## Creating a new column for the rating difference
cleandata_third_M <- clean_data_third %>% filter(grepl("M", Pic.Label.1)) %>% 
  mutate(trust_diff = trustworthy_p1 - trustworthy_p2)
cleandata_third_U <- clean_data_third %>% filter(grepl("U", Pic.Label.1)) %>% 
  mutate(trust_diff = trustworthy_p2 - trustworthy_p1)

clean_data_third <- rbind(cleandata_third_M,cleandata_third_U)

set_sum_contrasts()
r5 <-  glm(mask_got_20 ~ Sex + Amount + mask_wearing + trust_diff , family = "binomial", 
           data = clean_data_third)
summary(r5)
tab_model(r5, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)

exp(coef(r5))


#New 3 level dummy variable
clean_data_third <- clean_data_third %>% 
  mutate(protection = ifelse(mask_perception == "Only themselves" | 
                               mask_perception == "Mostly themselves", "themselves",
                      ifelse(mask_perception == "Equally themselves and others", 
                                     "Equally themselves and others", "others")))

clean_data_third$protection <- as.factor(clean_data_third$protection)

## mannually contrast coding

clean_data_third <- clean_data_third %>% 
  mutate(Sex_contrast = ifelse(Sex == "Male", 1, -1),
         Amount_contrast = ifelse(Amount == "£10", 1, -1),
         mask_wearing_contrast = ifelse(mask_wearing == "Yes", 1, -1))

set_default_contrasts()
r6 <-  glm(mask_got_20 ~ Sex_contrast + Amount_contrast + mask_wearing_contrast + 
             protection, family = "binomial", 
           data = clean_data_third)
tab_model(r6, p.style = "numeric_stars", show.aic = TRUE, auto.label = TRUE, 
          show.stat = TRUE, show.se = TRUE)


## ANOVAs 

aov1 <- aov_ez("ID", "trustworthy_score", data = data_long_third_trust, between = "whether_mask_got_20",
               within = "mask_condition")
aov1

afex_plot(aov1, x = "mask_condition", trace = "whether_mask_got_20", error = "none",
          mapping = c("linetype", "shape", "fill"),
          data_geom = ggplot2::geom_violin, 
          data_arg = list(width = 0.5))

# Follow-up test 
em1 <- emmeans(aov1, c("whether_mask_got_20","mask_condition"), model = "multivariate")

con1 <-  
  list(Yes_M_v_U = c(0,1,0,-1),
       No_M_v_U = c(1,0,-1,0),
       Mask_Y_v_N = c(1,-1,0,0),
       Unmask_Y_v_N = c(0,0,-1,1))

c1 <- contrast(em1, con1, adjust = "holm") 

confint(c1)

## Section 2 
## first both attractiveness and trustworthiness ratings need to become a long table -- using pivot_long
test02 <- clean_data_third %>% 
  mutate_all(as.character) %>% 
  pivot_longer(c(18,20), names_to = "Picture_label", values_to = "Picture_name",
               values_ptypes = list(att_score=character()))

## 01 For attractiveness rating 
test01 <- clean_data_third %>% 
  mutate_all(as.character) %>% 
  pivot_longer(c(8:9), names_to = "attractive", values_to = "attractive_score",
                     values_ptypes = list(att_score=character()))

data_long_third <- cbind(test02, test01[45:46])

## rearrange variable's type
data_long_third$X <- as.integer(data_long_third$X)
data_long_third$attractive_score <- as.numeric(data_long_third$attractive_score)
data_long_third <- data_long_third %>% mutate(mask_condition = case_when(
  endsWith(Picture_name, "U") ~ "unmask",
  endsWith(Picture_name, "M") ~ "mask"
  ))
data_long_third$Picture_name <- as.factor(data_long_third$Picture_name)
data_long_third$mask_condition <- as.factor(data_long_third$mask_condition)
data_long_third$Sex <- as.factor(data_long_third$Sex)

distribution_plot <- function(data, x, y, ...){
  ggplot(data = data, aes_string({{x}}, fill = {{y}}, ...)) +
    geom_density(alpha = (0.4)) +
    labs(x = x, y = "Density", fill = y) +
    scale_x_continuous(breaks=seq(0,10,by=1)) 
}

## Plot
distribution_plot(data_long_third, "attractive_score", "mask_condition") +
  scale_x_continuous(breaks=seq(0,100,by=20)) + 
  xlab("Attractiveness Rating") + 
  facet_wrap(~Sex, labeller = label_both) 


## two-way mixed ANOVA for attractiveness 
attr <- aov_ez("X", "attractive_score", data = data_long_third, between = "Sex",
       within = "mask_condition")
attr
# Anova Table (Type 3 tests)

# Response: attractive_score
#               Effect     df    MSE    F  ges p.value
# 1                Sex 1, 169 464.90 0.84 .004    .361
# 2     mask_condition 1, 169 151.62 0.99 .001    .321
# 3 Sex:mask_condition 1, 169 151.62 0.72 .001    .399
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

afex_plot(attr, x = "mask_condition", trace = "Sex", error = "within",
          mapping = c("linetype", "shape", "fill"),
          data_geom = ggplot2::geom_violin, 
          data_arg = list(width = 0.5)) 

## 02 For trustworthiness rating 
trust <- clean_data_third %>% 
  mutate_all(as.character) %>% 
  pivot_longer(c(10:11), names_to = "trustworthy", values_to = "trustworthy_score",
               values_ptypes = list(att_score=character()))

data_long_third_trust <- cbind(test02, trust[45:46])

## rearrange variable's type
data_long_third_trust <- data_long_third_trust %>% rename(ID = X)
data_long_third_trust$ID <- as.integer(data_long_third_trust$ID)
data_long_third_trust$trustworthy_score <- as.numeric(data_long_third_trust$trustworthy_score)
data_long_third_trust <- data_long_third_trust %>% mutate(mask_condition = case_when(
  endsWith(Picture_name, "U") ~ "unmask",
  endsWith(Picture_name, "M") ~ "mask"
))
data_long_third_trust$Picture_name <- as.factor(data_long_third_trust$Picture_name)
data_long_third_trust$mask_condition <- as.factor(data_long_third_trust$mask_condition)
data_long_third_trust$Sex <- as.factor(data_long_third_trust$Sex)



## Plot
distribution_plot(data_long_third_trust, "trustworthy_score", "mask_condition") +
  scale_x_continuous(breaks=seq(0,100,by=20)) + 
  xlab("Trustworthiness Rating") + 
  facet_wrap(~Sex, labeller = label_both)

## two-way mixed ANOVA for attractiveness 
set_sum_contrasts()
trust <- aov_ez("ID", "trustworthy_score", data = data_long_third_trust, between = "Sex",
       within = "mask_condition")
trust
# Anova Table (Type 3 tests)

# Response: trustworthy_score
#               Effect     df    MSE         F  ges p.value
# 1                Sex 1, 169 577.46      0.25 .001    .616
# 2     mask_condition 1, 169 176.90 22.85 *** .031   <.001
# 3 Sex:mask_condition 1, 169 176.90      2.23 .003    .137
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

trust_em <- emmeans(trust, "mask_condition")
trust_con <- list(
  mask_vs_unmask = c(-1,1)
)
x1 <- contrast(trust_em, trust_con, adjust = "holm")

# contrast       estimate   SE  df t.ratio p.value
# mask_vs_unmask    -6.88 1.44 169 -4.780  <.0001 

# Results are averaged over the levels of: Sex 

afex_plot(trust, x = "mask_condition", trace = "Sex", error = "within",
          mapping = c("linetype", "shape", "fill"),
          data_geom = ggplot2::geom_violin, 
          data_arg = list(width = 0.5)) 

## Doing K-S test and ECDF plots for trustworthiness and attractiveness 
unmask_T <- data_long_third_trust %>% filter(mask_condition == "unmask") %>% select(trustworthy_score)
unmask_T <- as.data.frame(mask_T)
mask_T <- data_long_third_trust %>% filter(mask_condition == "mask") %>% select(trustworthy_score)
mask_T <- as.data.frame(mask_T)

ks.test(mask_T[,1], unmask_T[,1])
# Two-sample Kolmogorov-Smirnov test

# data:  mask_T[, 1] and unmask_T[, 1]
# D = 0.18129, p-value = 0.00725
# alternative hypothesis: two-sided

unmask_A <- data_long_third %>% filter(mask_condition == "unmask") %>% select(attractive_score)
unmask_A <- as.data.frame(mask_A)
mask_A <- data_long_third %>% filter(mask_condition == "mask") %>% select(attractive_score)
mask_A <- as.data.frame(mask_A)

ks.test(mask_A[,1], unmask_A[,1])
# Two-sample Kolmogorov-Smirnov test

# data:  mask_A[, 1] and unmask_A[, 1]
# D = 0.087719, p-value = 0.5262
# alternative hypothesis: two-sided

# ECDF plots
ECDF_plot <- function(data, x, y, ...){
  ggplot(data = data, aes_string({{x}}, color = {{y}}, ...)) +
    stat_ecdf(geom = "step") + 
    labs(x=x, y="ECDF")
}

ECDF_plot(data_long_third_trust, "trustworthy_score", "mask_condition") 
## About 50% of participant rated the unmasked person less than 50, 
# While only about 38% of participant rated the masked person less than 50.

ECDF_plot(data_long_third, "attractive_score", "mask_condition")
## There is small difference between masked and unmasked participants on the attractiveness rating
 
# Final plots
d2 <- clean_data_third %>% 
  group_by(mask_wearing, Sex, whether_mask_got_20) %>% 
  summarise(n=n()) %>% 
  mutate(Percentage=n/sum(n)) %>% 
  mutate(`Still wearing mask` = ifelse(mask_wearing == "Yes", "Yes", "No")) %>%
  mutate(Mask = ifelse(whether_mask_got_20 == "Yes", "£20 to masked", "£20 to unmasked"))

d2$Mask <- 
  factor(d2$Mask,levels = c("£20 to unmasked","£20 to masked"))


d2$`Still wearing mask` <- 
  factor(d2$`Still wearing mask`,levels = c("Yes","No"))

d2_new <- d2[-c(2, 4, 6, 8), ] 

d2_new <- d2_new %>% mutate(Sex = ifelse(Sex == "Male", "Male participant", "Female participant"))

ggplot(data = d2_new, aes(x = `Still wearing mask`, y = Percentage,
           label = paste0(round(Percentage*100), "%"), fill = `Still wearing mask`)) +
  geom_col(position = position_stack(), color = "black") +
  geom_text(position = position_stack(vjust = 1.02), size = 5) +
  scale_y_continuous(labels = scales::percent_format())  +
  facet_wrap(~Sex) + 
  ylab("£20 to masked (%)") + 
  xlab("Whether participants still wearing masks")  +
  labs(
       fill = 
" ") +
  scale_fill_manual(values = c("gray30", "gray70")) +
  theme(axis.text.x = element_text(size=10), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, 
                                  hjust = 0.5)) +
  theme_classic() + theme(plot.title = element_text(size = 16,
                                  face = "bold",
                                  margin = margin(b = 35), hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.title.y.right = element_blank(),                # hide right axis title
        axis.text.y.right = element_blank(),                 # hide right axis labels
        axis.ticks.y = element_blank(),                      # hide left/right axis ticks
        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(3, "mm"),                       # remove spacing between facets
        strip.background = element_rect(size = 0),         # match default line size of theme_classic
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12),            # make facet label bold
        axis.text.x=element_text(colour="black"),            # make x axis text bold
        axis.text.y.left=element_text(colour="black"), 
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(size = 14),
        axis.title=element_text(size=14,face="bold"),
        legend.position = "none")


clean_data_third$whether_mask_got_20 <- 
  factor(clean_data_third$whether_mask_got_20,levels = c("No","Yes"))

d4 <- clean_data_third %>% 
  group_by(mask_perception, whether_mask_got_20) %>% 
  summarise(n=n()) %>% 
  mutate(Percentage=n/sum(n))

#mask_perception               whether_mask_got_20     n Percentage
#<fct>                         <fct>               <int>      <dbl>
#1 Only themselves               Yes                     1     0.143 
#2 Only themselves               No                      6     0.857 
#3 Mostly themselves             Yes                    37     0.607 
#4 Mostly themselves             No                     24     0.393 
#5 Equally themselves and others Yes                    70     0.795 
#6 Equally themselves and others No                     18     0.205 
#7 Mostly others                 Yes                    13     0.929 
#8 Mostly others                 No                      1     0.0714
#9 Only others                   Yes                     1     1 


dat <- data.frame(
  "£20_yes" = c(13, 37),
  "£20_no" = c(1, 24),
  row.names = c("Mostly other", "Mostly themselves"),
  stringsAsFactors = FALSE
)
colnames(dat) <- c("£20_yes", "£20_no")

fisher.test(dat)

ggplot(data = d4, aes(x = mask_perception, y = Percentage, fill = whether_mask_got_20,
                          label = paste0(round(Percentage*100), "%"))) +
  geom_col(position = position_stack(), color = "black") +
  geom_text(position = position_stack(vjust = .5)) +
  scale_y_continuous(labels = scales::percent_format()) 


d1 <- clean_data_third %>% 
  group_by(Sex, Amount,whether_mask_got_20) %>% 
  summarise(n=n()) %>% 
  mutate(Percentage=n/sum(n)) %>% 
  mutate(Mask = ifelse(whether_mask_got_20 == "Yes", "£20 to masked", "£20 to unmasked"))

d1$Mask <- 
  factor(d1$Mask,levels = c("£20 to unmasked","£20 to masked"))

d1_new <- d1[-c(2, 4, 6, 8), ] 

d1_new <- d1_new %>% mutate(Sex = ifelse(Sex == "Male", "Male participants", "Female participants"))

data.table::setnames(d1_new,'Amount','Allocating')

ggplot(data = d1_new, aes(x = Allocating, y = Percentage, fill = Allocating,
           label = paste0(round(Percentage*100), "%"))) +
  geom_col(position = position_stack(), color = "black") +
  geom_text(position = position_stack(vjust = 1.015), size = 5) +
  scale_y_continuous(labels = scales::percent_format()) + 
  geom_hline(yintercept=0.5, linetype="dashed", 
           color = "red", size=1) +
  facet_wrap(~Sex) +
  scale_fill_manual(values=c("grey30", 'grey80')) +
  ylab("£20 to masked (%)") + 
  xlab("Allocation frame") +
  labs(
    fill = 
      " ") +
  theme(axis.text.x = element_text(size=10), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, 
                                  hjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(size = 14,
                                  face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title.y.right = element_blank(),                # hide right axis title
        axis.text.y.right = element_blank(),                 # hide right axis labels
        axis.ticks.y = element_blank(),# hide left/right axis ticks
        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(3, "mm"),                       # remove spacing between facets
        strip.background = element_rect(size = 0),         # match default line size of theme_classic
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12),            # make facet label bold,            # make x axis text bold
        axis.text.y.left=element_text(colour="black"),
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(size = 14),
        axis.title=element_text(size=14,face="bold"))

clean_data_third %>% 
  group_by(Pic.Label.1) %>% 
  summarise(mean=mean(trustworthy_p1)) 

clean_data_third %>% 
  group_by(Pic.Label.2) %>% 
  summarise(mean=mean(trustworthy_p2)) 


d3 <- data_long_third_trust %>% group_by(mask_condition, Sex) %>% 
  summarise(m = mean(trustworthy_score),
            se = sd(trustworthy_score) / sqrt(length(trustworthy_score))) %>% 
  mutate(Mask = ifelse(mask_condition == "unmask", "Unmasked", "Masked"), 
         Gender = ifelse(Sex == "Female", "Female participants", "Male participants")) 

data.table::setnames(d3,'Whether counterpart got £20','Whether counterpart masked')

ggplot(data = d3, aes(x = Mask, y = m, label = round(m), fill = Mask)) +
  geom_col(position = position_stack(), color = "black") +
  geom_errorbar(aes(ymin = m - se, ymax = m + se), width = .1) +
  coord_cartesian(ylim = c(20, 70)) +
  facet_wrap(~Gender) +
  scale_fill_manual(values = c("gray30", "gray70")) +
  ylab("Average trustworthiness rating") + 
  xlab("Mask manipulation") +
  labs(
    caption = "Error bars indicate standard errors of the mean") +
  theme(axis.text.x = element_text(size=10), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, 
                                  hjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(size = 14,
                                  face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title.y.right = element_blank(),                # hide right axis title
        axis.text.y.right = element_blank(),                 # hide right axis labels
        axis.ticks.y = element_blank(),# hide left/right axis ticks
        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(3, "mm"),                       # remove spacing between facets
        strip.background = element_rect(size = 0),         # match default line size of theme_classic
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12),            # make facet label bold,            # make x axis text bold
        axis.text.y.left=element_text(colour="black"),
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(size = 14),
        axis.title=element_text(size=14,face="bold"),
        legend.position = "none")
