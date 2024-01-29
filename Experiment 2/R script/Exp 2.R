## Study 2 
setwd("/Users/pengjunning/Desktop")
library(readr)
library(effsize)
library(afex)
library(emmeans)
library(sjPlot)
library(lme4)
library(lmerTest)
library(nlme)
library(performance)

### Loading data 
study2 <- read.csv("Experiment_2.csv", check.names = F)

## Adding mask condition
study2 <- study2 %>%
  mutate(condition = case_when(
    endsWith(Picturelabel, "U") ~ "unmask",
    endsWith(Picturelabel, "M") ~ "mask"
  ))
### Model specific 
study2 <- study2 %>%
  mutate(modelcondition = case_when(
    startsWith(Picturelabel, "F1") ~ "F1",
    startsWith(Picturelabel, "F2") ~ "F2",
    startsWith(Picturelabel, "F3") ~ "F3",
    startsWith(Picturelabel, "F4") ~ "F4"
  ))

### Picture match
study2[,36] <- gsub("https://wbs.eu.qualtrics.com/ControlPanel/Graphic.php?IM=", "", study2[,36], fixed = TRUE)

study2 <- study2 %>%
  mutate(pic.match = ifelse(study2$paired_picture==study2$Picture,'Correct','Incorrect'))
### Change char to factor/numeric
# study2$`vaccine (Yes/No)` <-  as.factor(study2$`vaccine (Yes/No)`)
study2$condition <-  as.factor(study2$condition)
study2$Sex <-  as.factor(study2$Sex)
study2$Picture <-  as.factor(study2$Picture)
study2$paired_picture <-  as.factor(study2$paired_picture)
study2$Picturelabel <-  as.factor(study2$Picturelabel)
study2$protection <-  as.factor(study2$protection)
study2$modelcondition <-  as.factor(study2$modelcondition)
study2$Order <-  as.factor(study2$Order)
study2$behaviour <-  factor(study2$behaviour,levels = c("As frequently","Much less frequently","Less frequently", 
                                                        "Much more frequently", "More frequently"))
study2$party <- factor(study2$party, levels = c("Conservative Party", "Labour Party", "Liberal Democrats", 
                                                "Scottish National Party", "Other Party (Please specify)", "Did not vote", 
                                                "Prefer not to say"))

# Get rid of participants who got wrong with the comprehension question (the first two)
cleandata <- study2 %>% filter(Comprehension1 == "£9" & Comprehension2 == "£2" & pic.match == "Correct") # 361 observation
#41 invalid observation 

# Get rid of participants who got wrong with the comprehension question (all four questions)
cleandata_4Comp <- study2 %>% filter(Comprehension1 == "£9" & Comprehension2 == "£2" & pic.match == "Correct", 
                                      Comp_Sender_Total == "£9  (Initial £10 minus £3 sent plus £2 received from Responder)" & 
                                      Comp_Responder_Total == "£17  (Initial £10 plus £9 received from Sender minus £2 returned)")


# ks test                     
LT<- cleandata %>% filter(trustworthylevel == "low") %>% select(mean_return)
LT <- as.data.frame(LT)
MT<- cleandata %>% filter(trustworthylevel == "medium") %>% select(mean_return)
MT <- as.data.frame(MT)
HT<- cleandata %>% filter(trustworthylevel == "high") %>% select(mean_return)
HT <- as.data.frame(HT)
ks.test(LT[,1],MT[,1])
ks.test(LT[,1],HT[,1])
cohen.d(LT[,1],HT[,1])

# Correlation plots 
corr_plots <- function(data, x, y, ...){
  ggplot(data = data, aes_string({{x}}, {{y}}, ...)) +
    geom_jitter() +
    geom_smooth(aes_string({{x}}, {{y}}), method = "lm") +
    labs(x = x, y = y) +
    stat_cor(method="pearson")
}

a1 <- corr_plots(cleandata, "attractiveness", "trustworthy") + 
  facet_wrap(~condition)
a2 <- corr_plots(cleandata, "Sender", "trustworthy")
a3 <- corr_plots(cleandata, "mean_return", "trustworthy")
cowplot::plot_grid(a1,a2,a3, ncol=2,labels = c("A","B","C"))

### Vaccine percentage
ggplot(cleandata, aes(x = `vaccine`)) +
  geom_bar(alpha = .8) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) + 
  ggtitle("Vaccination proportion") +
  labs(x="Vaccine (Yes/No)")

### Picture selection 
ggplot(cleandata, aes(x = pic.match)) +
  geom_bar(alpha = .8) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  labs(x = "Picture Selection") + 
  ggtitle("Picture Match") +
  labs(x="Whether participants selected the correct model")

### Comprehension question
## Mask wearing behaviour 
ggplot(cleandata, aes(x = behaviour, fill = protection)) +
  geom_bar(alpha = .4) +
  labs(x = "Mask wearing behaviour after lockdown", fill="Protection Attitude") +
  ggtitle("Mask wearing behaviour and participant's attitude") 

### Voting party
ggplot(cleandata, aes(x = party, fill = protection)) +
  geom_bar(alpha = .4) +
  labs(x = "Voting party", fill="Protection Attitude") +
  ggtitle("Voting from last election") 


### Responder -- transfer from money value to proportion value
cleandata$Responder3 <- cleandata$Responder3/3
cleandata$Responder6 <- cleandata$Responder6/6
cleandata$Responder9 <- cleandata$Responder9/9
cleandata$Responder12 <- cleandata$Responder12/12
cleandata$Responder15 <- cleandata$Responder15/15
cleandata$Responder18 <- cleandata$Responder18/18
cleandata$Responder21 <- cleandata$Responder21/21
cleandata$Responder24 <- cleandata$Responder24/24
cleandata$Responder27 <- cleandata$Responder27/27
cleandata$Responder30 <- cleandata$Responder30/30

cleandata_4Comp$Responder3 <- cleandata_4Comp$Responder3/3
cleandata_4Comp$Responder6 <- cleandata_4Comp$Responder6/6
cleandata_4Comp$Responder9 <- cleandata_4Comp$Responder9/9
cleandata_4Comp$Responder12 <- cleandata_4Comp$Responder12/12
cleandata_4Comp$Responder15 <- cleandata_4Comp$Responder15/15
cleandata_4Comp$Responder18 <- cleandata_4Comp$Responder18/18
cleandata_4Comp$Responder21 <- cleandata_4Comp$Responder21/21
cleandata_4Comp$Responder24 <- cleandata_4Comp$Responder24/24
cleandata_4Comp$Responder27 <- cleandata_4Comp$Responder27/27
cleandata_4Comp$Responder30 <- cleandata_4Comp$Responder30/30


### converting data from wide to long 
Data_long <- cleandata_4Comp %>% pivot_longer(cols = Responder3:Responder30, names_to = "Responder_value",
                                        values_to = "Pound_returned")
Data_long$Pound_returned <- as.numeric(Data_long$Pound_returned)

Data_long$Responder_value <- factor(Data_long$Responder_value, levels = c("Responder3","Responder6","Responder9","Responder12",
                                                                          "Responder15","Responder18","Responder21","Responder24",
                                                                          "Responder27","Responder30"))

fre <- Data_long %>%
  group_by(Sender, Responder_value, condition) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

### calculate mean proportion returned 
cleandata <- cleandata %>% mutate(mean_return = as.vector(rowMeans(cleandata[, 9:18])))

cleandata <- cleandata %>%
  mutate(amountreturned = ifelse(cleandata$mean_return <= 0.33,"low",
                                   ifelse(cleandata$mean_return >= 0.34 & cleandata$mean_return < 0.66, "medium", "high")))

cleandata$amountreturned <- factor(cleandata$amountreturned, levels = c("low", "medium", "high"))

cor(cleandata$trustworthy, cleandata$mean_return)
cor(cleandata$trustworthy, cleandata$Sender)
cor(cleandata$mean_return, cleandata$Sender)

### Linear Regression 
library(ggfortify)
qqnorm(cleandata$Sender)
shapiro.test(cleandata$Sender)
skewness(cleandata$Sender)

qqnorm(cleandata$attractiveness)
shapiro.test(cleandata$attractiveness)
skewness(cleandata$attractiveness)

qqnorm(cleandata$trustworthy)
shapiro.test(cleandata$trustworthy)
skewness(cleandata$trustworthy)

### Sender 
set_default_contrasts()
lm1 <- lm(Sender ~ condition + Sex + Order + Sex * condition , cleandata_4Comp)
tab_model(lm1, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)
summary(lm1)

#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               3.6672     0.3732   9.825  < 2e-16 ***
# SexMale                   0.8087     0.4787   1.689    0.092 .  
# conditionunmask          -0.0441     0.4757  -0.093    0.926    
# OrderSenderFirst          1.5375     0.3366   4.568 6.78e-06 ***
# SexMale:conditionunmask  -0.1863     0.6736  -0.277    0.782    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 3.191 on 356 degrees of freedom
# Multiple R-squared:  0.06535,	Adjusted R-squared:  0.05485 
# F-statistic: 6.222 on 4 and 356 DF,  p-value: 7.538e-05
set_sum_contrasts()
lm2 <- lm(Sender ~ condition + Sex + Order + Sex * condition + cont_behave*condition , cleandata)
tab_model(lm2, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)


lm3 <- lm(Sender ~ Order + behaviour + trustworthy, cleandata)
lm4 <- lm(Sender ~ condition  + Sex + Order + behaviour + trustworthy, cleandata)

# Exploratory model 
lm5 <- lm(Sender ~ Sex + condition + Order  + behaviour + protection + trustworthy +
            condition*Sex + condition*Order + Sex*Order + 
            condition*trustworthy, cleandata)
tab_model(lm5, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)

Reg_table <- mtable("Model 1"=lm1, "Model 5"=lm5,
                    summary.stats=c("sigma","R-squared","F","p","N"))

Mask <- cleandata%>%filter(condition=="unmask")
Unmask <- cleandata%>%filter(condition=="mask")
cohen.d(Mask$Sender,Unmask$Sender)
### Calculating effect size
SF <- cleandata%>%filter(Order=="SenderFirst")
RF <- cleandata%>%filter(Order=="ResponderFirst")
cohen.d(SF$Sender,RF$Sender)

H <- cleandata%>%filter(trustworthylevel=="high")
L <- cleandata%>%filter(trustworthylevel == "low")
cohen.d(H$Sender,L$Sender)

BehLess <- cleandata%>%filter(behaviour=="Much less frequently")
BehAs <- cleandata%>%filter(behaviour == "As frequently")
cohen.d(BehAs$Sender,BehLess$Sender)

BehLess <- cleandata%>%filter(behaviour=="Much less frequently")
BehMore <- cleandata%>%filter(behaviour == "More frequently")
cohen.d(BehMore$Sender,BehLess$Sender)


### Trust 
set_default_contrasts()
lm1 <- lm(trustworthy ~ Sex + condition + Order + Sex * condition, cleandata)
lm2 <- lm(trustworthy ~ Sex + sentamount + behaviour + amountreturned, cleandata)
lm3 <- lm(trustworthy ~ Order+ sentamount+ behaviour + amountreturned, cleandata)
lm4 <- lm(trustworthy ~ condition  + Sex + Order + sentamount+ behaviour+ amountreturned, cleandata)
# Exploratory Model
lm5 <- lm(trustworthy ~ Sex + condition + Order  + behaviour + protection +
            condition*Sex + condition*Order + Sex*Order, cleandata)
tab_model(lm5, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)

Reg_table2 <- mtable("Model 1"=lm1,"Model 5"=lm5,
                    summary.stats=c("sigma","R-squared","F","p","N"))
Reg_table2


### Calculating effect size
M <- cleandata%>%filter(condition=="mask")
U <- cleandata%>%filter(condition=="unmask")
cohen.d(M$trustworthy,U$trustworthy)

M <- cleandata%>%filter(sentamount=="high")
L <- cleandata%>%filter(sentamount == "low")
cohen.d(M$trustworthy,L$trustworthy)

H <- cleandata%>%filter(amountreturned=="medium")
L <- cleandata%>%filter(amountreturned == "low")
cohen.d(H$trustworthy,L$trustworthy)

mean(cleandata[cleandata$condition == "mask", 25])
mean(cleandata[cleandata$condition == "unmask", 25])
autoplot(lm5)
# Residual Histogram
ols_plot_resid_hist(lm5)

# Follow-up test 
em4 <- emmeans(lm5, c("condition","Order"), model = "multivariate")

con4 <-  
  list(ResF_M_v_U = c(1,-1,0,0),
       SeF_M_v_U = c(0,0,1,-1),
       Mask_ResF_v_SeF = c(1,0,-1,0),
       Umask_ResF_v_SeF = c(0,1,0,-1))

c4 <- contrast(em4, con4, adjust = "holm") %>% as_tibble()
# contrast         estimate    SE    df t.ratio p.value
# <chr>               <dbl> <dbl> <dbl>   <dbl>   <dbl>
# 1 ResF_M_v_U           9.74  2.80   347   3.48  0.00227
# 2 SeF_M_v_U            1.83  2.83   347   0.647 0.549  
# 3 Mask_ResF_v_SeF      4.78  2.89   347   1.65  0.300  
# 4 Umask_ResF_v_SeF    -3.14  2.86   347  -1.09  0.549  

eff4 <- as.data.frame(effect(term="condition*Order", mod=lm5))
ggplot(data=eff4, aes(x=Order, y=fit, group=condition, fill = condition)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.1) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15) + 
  xlab("Order") + 
  ylab("Trustworthiness") 

ResFM <- cleandata%>%filter(Order=="ResponderFirst"&condition=="mask")
ResFU <- cleandata%>%filter(Order=="ResponderFirst"&condition=="unmask")
cohen.d(ResFM$trustworthy,ResFU$trustworthy)

### Attractiveness
lm1 <- lm(attractiveness ~ Sex + condition + Sex * condition, cleandata)
lm2 <- lm(attractiveness ~ Sex +  behaviour, cleandata)
lm3 <- lm(attractiveness ~ Order +  behaviour, cleandata)
lm4 <- lm(attractiveness ~ condition  + Sex + Order + behaviour, cleandata)
lm5 <- lm(attractiveness ~ Sex + condition + Order  + trustworthy + 
            condition*Sex + condition*Order + Sex*Order, cleandata)

Reg_table3 <- mtable("Model 1"=lm1,"Model 5"=lm5,
                     summary.stats=c("sigma","R-squared","F","p","N"))
Reg_table3
summary(lm1)
tab_model(lm1, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)

## Effect size
M <- cleandata%>%filter(sentamount=="high")
L <- cleandata%>%filter(sentamount == "low")
cohen.d(M$attractiveness,L$attractiveness)

H <- cleandata%>%filter(amountreturned=="high")
L <- cleandata%>%filter(amountreturned == "low")
cohen.d(H$attractiveness,L$attractiveness)

# Follow-up test 
em5 <- emmeans(lm5, c("condition","Order"), model = "multivariate")
con5 <-  
  list(ResF_M_v_U = c(1,-1,0,0),
       SeF_M_v_U = c(0,0,1,-1),
       Mask_ResF_v_SeF = c(1,0,-1,0),
       Umask_ResF_v_SeF = c(0,1,0,-1))
c5 <- contrast(em5, con5, adjust = "holm") %>% as_tibble()

# A tibble: 4 x 6
# contrast         estimate    SE    df t.ratio p.value
# <chr>               <dbl> <dbl> <dbl>   <dbl>   <dbl>
# 1 ResF_M_v_U           3.42  2.96   351    1.16   0.461
# 2 SeF_M_v_U           -5.03  3.00   351   -1.68   0.376
# 3 Mask_ResF_v_SeF      4.85  3.03   351    1.60   0.376
# 4 Umask_ResF_v_SeF    -3.61  3.01   351   -1.20   0.461

eff5 <- as.data.frame(effect(term="condition*Order", mod=lm5))
ggplot(data=eff5, aes(x=Order, y=fit, group=condition, fill = condition)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.1) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15) + 
  xlab("Order") + 
  ylab("Attractiveness") 

ResFM <- cleandata%>%filter(Order=="ResponderFirst"&condition=="mask")
ResFU <- cleandata%>%filter(Order=="ResponderFirst"&condition=="unmask")
cohen.d(ResFM$attractiveness,ResFU$attractiveness)


### Multilevel Regression 
# Null model
null <- lme(fixed = Pound_returned~1, random = ~1|ID, data = Data_long)

summary(null)
VarCorr(null)
# Random Intercept Only Model
model2 <- lme(fixed = Pound_returned~ condition + behaviour + trustworthylevel, random = ~1|ID, data = Data_long)
summary(model2)
VarCorr(model2)

model3 <- lme(fixed = Pound_returned~condition + Sex + Order + Sender + trustworthylevel, random = ~1|ID, data = Data_long)
summary(model3)
VarCorr(model3)

# Random Intercept and random slope Model
model4 <- lme(fixed = Pound_returned~condition + Sex + Order + Sender, random = ~Sender|ID, data = Data_long)
summary(model4)
VarCorr(model4)

# Most saturated model
model5 <- lme(fixed = Pound_returned~condition + Sex + Order + Sender + condition*Sex, 
              random = ~Sender|ID, data = Data_long)

summary(model5)
tab_model(model5, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)
SF <- Data_long%>%filter(condition=="unmask")
RF <- Data_long%>%filter(condition=="mask")
cohen.d(SF$Pound_returned,RF$Pound_returned)

model6 <- lme(fixed = Pound_returned~condition + Sex + Order + Sender + trustworthy + 
                protection + behaviour + condition*Sex + behaviour*condition, 
              random = ~Sender|ID, data = Data_long)
summary(model6)
tab_model(model6, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)

Csender <- Data_long$Sender - mean(Data_long$Sender) 

Data_long <-Data_long %>% mutate(Csender = Data_long$Sender - mean(Data_long$Sender))

LT<- Data_long %>% filter(trustworthylevel == "low") %>% select(Pound_returned)
MT<- Data_long %>% filter(trustworthylevel == "medium") %>% select(Pound_returned)
HT<- Data_long %>% filter(trustworthylevel == "high") %>% select(Pound_returned)

cohen.d(MT$Pound_returned,LT$Pound_returned)
cohen.d(HT$Pound_returned,LT$Pound_returned)

anova(null,model2,model3,model4,model5)

#        Model df       AIC       BIC   logLik   Test   L.Ratio p-value
# null       1  3 -4561.784 -4543.210 2283.892                         
# model2     2  4 -4553.702 -4528.938 2280.851 1 vs 2  6.082357  0.0137
# model3     3  7 -4540.928 -4497.598 2277.464 2 vs 3  6.773353  0.0795
# model4     4  9 -4561.201 -4505.490 2289.600 3 vs 4 24.272366  <.0001
# model5     5 10 -4554.505 -4492.607 2287.252 4 vs 5  4.695840  0.0302


## Final plots 
CI_sender_2 <- cleandata %>% group_by(Sex, condition) %>% 
  summarise(m = mean(Sender),
            se = sd(Sender) / sqrt(length(Sender)))  %>% 
  mutate(Mask = ifelse(condition == "unmask", "Unmasked", "Masked"))%>%
  mutate(Sex = ifelse(Sex == "Female", "Female participant", "Male participant")) 

plot4 <- CI_sender_2 %>% 
  ggplot(aes(Mask, m)) +
  facet_wrap(~Sex, strip.position = 'top', scales='free_y') +
  geom_col(aes(fill = Mask), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = m - se,
                    ymax = m + se),
                color = "#22292F",
                width = .1) +
  scale_y_continuous(limits = c(0,6), breaks=seq(0,10,by=2)) +
  theme_classic() +
  labs(
    x = "Facemask Manipulation",
    y = "Amount sent by trustor",
    subtitle = "Exp 2 (Female Counterparts Only)",
    fill = "Mask",
    caption = "      Error bars indicate standard errors"
  ) + 
  scale_fill_grey(start = 0.3) +
  theme_classic() +
  theme(plot.title = element_text(size = 16,
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
        legend.position = "none", 
        plot.caption = element_text(hjust = -0.1),
        axis.text = element_text(size = 14),
        axis.title=element_text(size=14,face="bold"))

CI_trust_2 <- cleandata %>% group_by(Sex, condition) %>% 
  summarise(m = mean(trustworthy),
            se = sd(trustworthy) / sqrt(length(trustworthy))) %>% 
  mutate(Mask = ifelse(condition == "unmask", "Unmasked", "Masked")) %>%
  mutate(Sex = ifelse(Sex == "Female", "Female participant", "Male participant")) 

plot5 <- CI_trust_2 %>% 
  ggplot(aes(Mask, m)) +
  facet_wrap(~Sex, strip.position = 'top') +
  geom_col(aes(fill = Mask), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = m - se,
                    ymax = m + se),
                color = "#22292F",
                width = .1) +
  scale_y_continuous(limits = c(0, 70),breaks=seq(0,70,by=15)) +
  theme_classic() +
  labs(
    x = "Facemask Manipulation",
    y = "Trustworthiness Ratings",
    fill = "Mask",
    subtitle = "Exp 2 (Female Counterparts Only)",
    caption = "Error bars indicate standard error"
  ) + 
  scale_fill_grey(start = 0.3) +
  theme(plot.title = element_text(size = 16,
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
        legend.position = "none", 
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(size = 14),
        axis.title=element_text(size=14,face="bold"))

cowplot::plot_grid(plot2, plot5,ncol=1,nrow = 2, rel_heights = c(1.6,1))


Data_long <- Data_long %>%
  mutate(Amount_sent = case_when(
    Responder_value =="Responder3" ~ "1",
    Responder_value =="Responder6" ~ "2",
    Responder_value =="Responder9" ~ "3",
    Responder_value =="Responder12" ~ "4",
    Responder_value =="Responder15" ~ "5",
    Responder_value =="Responder18" ~ "6",
    Responder_value =="Responder21" ~ "7",
    Responder_value =="Responder24" ~ "8",
    Responder_value =="Responder27" ~ "9",
    Responder_value =="Responder30" ~ "10"
  ))

Data_long$Amount_sent <- factor(Data_long$Amount_sent, levels = c("1", "2", "3",
                                                                  "4","5",'6',
                                                                  "7","8","9",
                                                                  "10"))

CI_return2 <- Data_long %>% group_by(Sex, condition, Amount_sent ) %>% 
  summarise(m = mean(Pound_returned),
            se = sd(Pound_returned) / sqrt(length(Pound_returned))) %>%
  mutate(`Mask manipulation` = ifelse(condition == "unmask", "Unmasked", "Masked"))  %>%
  mutate(Sex = ifelse(Sex == "Female", "Female participant", "Male participant")) 

plot6 <- ggplot(CI_return2) +
  aes(x = Amount_sent, y = m, colour = `Mask manipulation`) +
  geom_pointrange(aes(ymin = m - se,
                      ymax = m + se), 
                  position=position_dodge(width=0.5)) + 
  geom_line(aes(group = `Mask manipulation`),
            position=position_dodge(width=0.5)) +
  scale_color_hue(direction = 1)  + 
  labs(
    x = "Amount sent",
    y = "Proportion returned",
    subtitle = "Exp 2 (Female Counterparts Only)",
    colour = "Mask manipulation",
    caption = "Error bars indicate standard error"
  ) +
  scale_colour_manual(values=c("Black", "gray60")) +
  facet_wrap(~Sex) +
  theme_classic() +
  theme(plot.title = element_text(size = 16,
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
        plot.caption = element_text(hjust = -0.1),
        axis.text = element_text(size = 14),
        axis.title=element_text(size=14,face="bold"),
        legend.position = "none")


legend <- get_legend(
  plot6 +
    guides() + 
    theme(legend.position = "bottom"))

# Combine combined plot and legend using plot_grid()
plot6 <- cowplot::plot_grid(plot6,legend, ncol=1,nrow = 2, rel_heights = c(1, 0.07))

cowplot::plot_grid(plot3,plot6,ncol=1,nrow = 2, 
                   rel_heights = c(1.5, 1))