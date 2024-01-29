# Setting up 
setwd("/Users/pengjunning/Desktop")
library(readr)
library(effectsize)
library(afex)
library(emmeans)
library(sjPlot)
library(lme4)
library(lmerTest)
library(nlme)
library(tidyverse)

# Loading data 
Data <- read.csv("Experiment_1.csv", check.names = F)

# Transfer chr into factor
Data$Comp_Sender_3 <-  as.factor(Data$Comp_Sender_3)
Data$Comp_Responder_2 <- as.factor(Data$Comp_Responder_2)
Data$COVID_19_vaccine <- as.factor(Data$COVID_19_vaccine)
Data$COVID_19_vaccine_attitude <- as.factor(Data$COVID_19_vaccine_attitude)
Data$Protect_themselves_or_others <- as.factor(Data$Protect_themselves_or_others) 
Data$understanding_difficulty <- as.factor(Data$understanding_difficulty)
Data$Picture_label <- as.factor(Data$Picture_label)
Data$Sex <- as.factor(Data$Sex)
Data$Order <- as.factor(Data$Order)
Data$Ethnicity <- as.factor(Data$Ethnicity)
Data$Pic.Mask <- factor(Data$Pic.Mask, levels = (c('U', 'M')))
Data$Pic.Gender <- as.factor(Data$Pic.Gender)
Data$Sender_Response <- as.numeric(Data$Sender_Response)
Data$Picture_label <- as.factor(Data$Picture_label)

Data <- Data %>% 
  rename(
    prevent_catching = `Mask_attitude`,
    prevent_spreading = `mask_prevents_Covid.19`
  )

# Get rid of participants who got wrong with the comprehension question (the first two)
cleandata <- Data %>% filter(Comp_Sender_3 == "£9" & Comp_Responder_2 == "£2") # 718 left
cleandata$Pic.Mask <- factor(cleandata$Pic.Mask, levels = (c('M', 'U')))

# Get rid of participants who got wrong with the comprehension question (all four questions)
cleandata_4Comp <- Data %>% filter(Comp_Sender_3 == "£9" & Comp_Responder_2 == "£2", 
                                     Comp_Sender_Total == "£9  (Initial £10 minus £3 sent plus £2 received from Responder)" & 
                                       Comp_Responder_Total == "£17  (Initial £10 plus £9 received from Sender minus £2 returned)")

# Calculating proportional returned 
cleandata$Responder_3 <- cleandata$Responder_3/3
cleandata$Responder_6 <- cleandata$Responder_6/6
cleandata$Responder_9 <- cleandata$Responder_9/9
cleandata$Responder_12 <- cleandata$Responder_12/12
cleandata$Responder_15 <- cleandata$Responder_15/15
cleandata$Responder_18 <- cleandata$Responder_18/18
cleandata$Responder_21 <- cleandata$Responder_21/21
cleandata$Responder_24 <- cleandata$Responder_24/24
cleandata$Responder_27 <- cleandata$Responder_27/27
cleandata$Responder_30 <- cleandata$Responder_30/30

cleandata_4Comp$Responder_3 <- cleandata_4Comp$Responder_3/3
cleandata_4Comp$Responder_6 <- cleandata_4Comp$Responder_6/6
cleandata_4Comp$Responder_9 <- cleandata_4Comp$Responder_9/9
cleandata_4Comp$Responder_12 <- cleandata_4Comp$Responder_12/12
cleandata_4Comp$Responder_15 <- cleandata_4Comp$Responder_15/15
cleandata_4Comp$Responder_18 <- cleandata_4Comp$Responder_18/18
cleandata_4Comp$Responder_21 <- cleandata_4Comp$Responder_21/21
cleandata_4Comp$Responder_24 <- cleandata_4Comp$Responder_24/24
cleandata_4Comp$Responder_27 <- cleandata_4Comp$Responder_27/27
cleandata_4Comp$Responder_30 <- cleandata_4Comp$Responder_30/30

# ----------------- Linear Regression ---------------------- 
# ----------------- Dependent Variable as AMOUNT SENT ---------------------- 
library(afex)
set_default_contrasts()
lm1 <- lm(Sender_Response ~ Sex + Pic.Gender + Pic.Mask + Order + Pic.Mask*Pic.Gender + 
            Pic.Mask*Sex + Pic.Gender*Sex, cleandata_4Comp)
summary(lm1)
tab_model(lm1, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)

lm2 <- lm(Sender_Response ~ Pic.Gender + Trustworthy+ Protect_themselves_or_others, cleandata)
lm3 <- lm(Sender_Response ~ Sex + Trustworthy+ Protect_themselves_or_others, cleandata)
lm4 <- lm(Sender_Response ~ Order + Trustworthy+ Protect_themselves_or_others, cleandata)
lm5 <- lm(Sender_Response ~ Pic.Mask + Pic.Gender + Sex + Order + Trustworthy+ Protect_themselves_or_others, cleandata)

# Exploratory model
lm6 <- lm(Sender_Response ~  Pic.Mask + Sex + Pic.Gender + Order + Trustworthy + Protect_themselves_or_others +
            Pic.Mask*Pic.Gender + Pic.Mask*Sex + Pic.Gender*Sex + Pic.Mask*Order + Pic.Gender*Order + 
              Sex*Order + Pic.Mask*Trustworthy, cleandata)
summary(lm6)
tab_model(lm6, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)

### Effect size
Mask <- cleandata%>%filter(Pic.Mask=="M")
Unmask <- cleandata%>%filter(Pic.Mask=="U")
cohen.d(Mask$Sender_Response,Unmask$Sender_Response)

SF <- cleandata%>%filter(Order=="SenderFirst")
RF <- cleandata%>%filter(Order=="ResponderFirst")
cohen.d(SF$Sender,RF$Sender)

CL <- cleandata%>%filter(catching=="low")
CH <- cleandata%>%filter(catching=="high")
cohen.d(CL$Sender,SH$Sender)

Ma <- cleandata%>%filter(Sex=="Male")
Fe <- cleandata%>%filter(Sex=="Female")
cohen.d(Ma$Sender,Fe$Sender)

# ----------------- Dependent Variable as Trustworthiness ratings ---------------------- 
lm1 <- lm(Trustworthy ~ Sex + Pic.Gender + Pic.Mask + Order + Pic.Mask*Pic.Gender + 
            Pic.Mask*Sex + Pic.Gender*Sex, cleandata)
lm2 <- lm(Trustworthy ~ Pic.Gender, cleandata)
lm3 <- lm(Trustworthy ~ Sex, cleandata)
lm4 <- lm(Trustworthy ~ Order, cleandata)
lm5 <- lm(Trustworthy ~ Pic.Mask + Pic.Gender + Sex + Order , cleandata)

# Exploratory model
lm6 <- lm(Trustworthy ~ Sex +  Pic.Gender + Pic.Mask + Order + Protect_themselves_or_others + 
            Pic.Mask*Pic.Gender + Pic.Mask*Sex + Pic.Mask*Order +
            Pic.Gender*Sex + Pic.Gender*Order + 
            Sex*Order, cleandata)
Reg_table_T <- mtable("Model 1"=lm1,"Model 2"=lm2,"Model 3"=lm3, 
                    "Model 4"=lm4, "Model 5"=lm5,"Model 6"=lm6,
                    summary.stats=c("sigma","R-squared","F","p","N"))
Reg_table_T
summary(lm6)
tab_model(lm6, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)

### Effect size
M <- cleandata%>%filter(Pic.Mask=="M")
U <- cleandata%>%filter(Pic.Mask=="U")
cohen.d(M$Trustworthy,U$Trustworthy)

Fe <- cleandata%>%filter(Pic.Gender=="F")
Ma <- cleandata%>%filter(Pic.Gender=="M")
cohen.d(Fe$Trustworthy, Ma$Trustworthy)

# Follow-up test 
em1 <- emmeans(lm6, c("Pic.Mask","Pic.Gender"), model = "multivariate")

con1 <-  
  list(Model_Female_M_v_U = c(1,-1,0,0),
       Model_Male_M_v_U = c(0,0,1,-1),
       Unmask_Fe_v_Ma = c(0,1,0,-1),
       Mask_Fe_v_Ma = c(1,0,-1,0))

c1 <- contrast(em1, con1, adjust = "holm") %>% as_tibble()

#contrast           estimate    SE    df t.ratio p.value
#<chr>                 <dbl> <dbl> <dbl>   <dbl>   <dbl>
#1 Model_Female_M_v_U    6.84   1.90   703   3.59  0.00140
#2 Model_Male_M_v_U      1.67   1.92   703   0.867 0.772  
#3 Unmask_Fe_v_Ma        0.685  1.88   703   0.364 0.772  
#4 Mask_Fe_v_Ma          5.86   1.94   703   3.02  0.00778

# Effect size
FM <- cleandata%>%filter(Pic.Mask=="M"&Pic.Gender=="F")
FU <- cleandata%>%filter(Pic.Mask=="U"&Pic.Gender=="F")
cohen.d(FM$Trustworthy,FU$Trustworthy)

MF <- cleandata%>%filter(Pic.Mask=="M"&Pic.Gender=="F")
MM <- cleandata%>%filter(Pic.Mask=="M"&Pic.Gender=="M")
cohen.d(MF$Trustworthy,MM$Trustworthy)

eff1 <- as.data.frame(effect(term="Pic.Mask*Pic.Gender", mod=lm6))
ggplot(data=eff1, aes(x=Pic.Mask, y=fit, group=Pic.Gender, fill = Pic.Gender)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.1) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15) + 
  xlab("Mask Condition") + 
  ylab("Trustworthiness") 

# Follow-up test 
em2 <- emmeans(lm6, c("Pic.Gender", "Sex"), model = "multivariate")

con2 <-  
  list(Model_Female_Ma_v_Fe = c(1,0,-1,0),
       Model_Male_Ma_v_Fe = c(0,1,0,-1),
       P.Male_Fe_v_Ma = c(0,0,1,-1),
       P.Female_Fe_v_Ma = c(1,-1,0,0))

c2 <- contrast(em2, con2, adjust = "holm") %>% as_tibble()

#contrast             estimate    SE    df t.ratio p.value
#<chr>                   <dbl> <dbl> <dbl>   <dbl>   <dbl>
#1 Model_Female_Ma_v_Fe    1.38   1.91   703   0.724 0.939  
#2 Model_Male_Ma_v_Fe     -3.81   1.92   703  -1.98  0.144  
#3 P.Male_Fe_v_Ma          0.678  1.91   703   0.355 0.939  
#4 P.Female_Fe_v_Ma        5.87   1.91   703   3.07  0.00885

eff2 <- as.data.frame(effect(term="Pic.Gender*Sex", mod=lm6))
ggplot(data=eff2, aes(x=Sex, y=fit, group=Pic.Gender, fill = Pic.Gender)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.1) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15) + 
  xlab("Participant Gender") + 
  ylab("Trustworthiness") 

# Effect size
SF <- cleandata%>%filter(Sex=="Female"&Pic.Gender=="F")
SM <- cleandata%>%filter(Sex=="Female"&Pic.Gender=="M")
cohen.d(SF$Trustworthy,SM$Trustworthy)

# ----------------- Dependent Variable as Attractiveness ratings ---------------------- 
lm1 <- lm(Attractiveness ~ Sex + Pic.Gender + Pic.Mask + Pic.Mask*Pic.Gender + Pic.Mask*Sex + Pic.Gender*Sex, cleandata)
tab_model(lm1, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)
summary(lm1)
lm2 <- lm(Attractiveness ~ Pic.Gender, cleandata)
lm3 <- lm(Attractiveness ~ Sex, cleandata)
lm4 <- lm(Attractiveness ~ Order, cleandata)
lm5 <- lm(Attractiveness ~ Pic.Mask + Pic.Gender + Sex + Order, cleandata)
# Exploratory model
lm6 <- lm(Attractiveness ~ Sex +  Pic.Gender + Pic.Mask + Order  +
            Pic.Mask*Pic.Gender + Pic.Mask*Sex + Pic.Mask*Order +
            Pic.Gender*Sex + Pic.Gender*Order + 
            Sex*Order, cleandata)
Reg_table_A <- mtable("Model 1"=lm1,"Model 6"=lm6,
                      summary.stats=c("sigma","R-squared","F","p","N"))
Reg_table_A


## Effect size
M <- cleandata%>%filter(Pic.Mask=="M")
U <- cleandata%>%filter(Pic.Mask=="U")
cohen.d(M$Attractiveness,U$Attractiveness)

Fe <- cleandata%>%filter(Pic.Gender=="F")
Ma <- cleandata%>%filter(Pic.Gender=="M")
cohen.d(Fe$Attractiveness, Ma$Attractiveness)

# Follow-up test 
FM <- cleandata%>%filter(Pic.Mask=="M"&Pic.Gender=="F")
FU <- cleandata%>%filter(Pic.Mask=="U"&Pic.Gender=="F")
cohen.d(FM$Attractiveness,FU$Attractiveness)

MF <- cleandata%>%filter(Pic.Mask=="M"&Pic.Gender=="F")
MM <- cleandata%>%filter(Pic.Mask=="M"&Pic.Gender=="M")
cohen.d(MF$Attractiveness,MM$Attractiveness)

UF <- cleandata%>%filter(Pic.Mask=="U"&Pic.Gender=="F")
UM <- cleandata%>%filter(Pic.Mask=="U"&Pic.Gender=="M")
cohen.d(UF$Attractiveness,UM$Attractiveness)

em3 <- emmeans(lm6, c("Pic.Mask","Pic.Gender"), model = "multivariate")

con3 <-  
  list(Model_Female_M_v_U = c(1,-1,0,0),
       Model_Male_M_v_U = c(0,0,1,-1),
       Unmask_Fe_v_Ma = c(0,1,0,-1),
       Mask_Fe_v_Ma = c(1,0,-1,0))

c3 <- contrast(em3, con3, adjust = "holm") %>% as_tibble()
# A tibble: 4 x 6
#contrast           estimate    SE    df t.ratio     p.value
#<chr>                 <dbl> <dbl> <dbl>   <dbl>       <dbl>
#1 Model_Female_M_v_U    5.97   2.21   707   2.70  0.0143     
#2 Model_Male_M_v_U     -0.317  2.23   707  -0.142 0.887      
#3 Unmask_Fe_v_Ma        6.25   2.19   707   2.85  0.0133     
#4 Mask_Fe_v_Ma         12.5    2.26   707   5.56  0.000000152

eff3 <- as.data.frame(effect(term="Pic.Mask*Pic.Gender", mod=lm6))
ggplot(data=eff3, aes(x=Pic.Mask, y=fit, group=Pic.Gender, fill = Pic.Gender)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.1) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15) + 
  xlab("Mask Condition") + 
  ylab("Attractiveness") 

# Follow-up test 
em4 <- emmeans(lm6, c("Pic.Gender", "Sex"), model = "multivariate")

con4 <-  
  list(Model_Female_Ma_v_Fe = c(1,0,-1,0),
       Model_Male_Ma_v_Fe = c(0,1,0,-1),
       P.Male_Fe_v_Ma = c(0,0,1,-1),
       P.Female_Fe_v_Ma = c(1,-1,0,0))

c4 <- contrast(em4, con4, adjust = "holm") %>% as_tibble()

# contrast             estimate    SE    df t.ratio      p.value
#<chr>                   <dbl> <dbl> <dbl>   <dbl>        <dbl>
#1 Model_Female_Ma_v_Fe     2.36  2.24   704    1.05 0.292       
#2 Model_Male_Ma_v_Fe      -3.75  2.24   704   -1.68 0.188       
#3 P.Male_Fe_v_Ma           6.46  2.21   704    2.92 0.0109      
#4 P.Female_Fe_v_Ma        12.6   2.21   704    5.69 0.0000000750

eff4 <- as.data.frame(effect(term="Pic.Gender*Sex", mod=lm6))
ggplot(data=eff4, aes(x=Sex, y=fit, group=Pic.Gender, fill = Pic.Gender)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.1) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15) + 
  xlab("Participant Gender") + 
  ylab("Attractiveness") 

# Effect size
SF <- cleandata%>%filter(Pic.Mask=="M"&Pic.Gender=="F")
SM <- cleandata%>%filter(Pic.Mask=="M"&Pic.Gender=="M")
cohen.d(SF$Attractiveness,SM$Attractiveness)

MF <- cleandata%>%filter(Sex=="Male"&Pic.Gender=="F")
MM <- cleandata%>%filter(Sex=="Male"&Pic.Gender=="M")
cohen.d(MF$Attractiveness,MM$Attractiveness)


## ------------------- Amount Returned in the second stage ----------------
Data_long <- cleandata_4Comp %>% pivot_longer(cols = Responder_3:Responder_30, names_to = "Responder_value",
                                     values_to = "Pound_returned")
Data_long$Responder_value <- as.character(Data_long$Responder_value)
Data_long$Responder_value <- gsub('Responder_', '', Data_long$Responder_value)
Data_long$Responder_value <- as.numeric(Data_long$Responder_value)
Data_long$Pound_returned <- as.numeric(Data_long$Pound_returned)

RF <- Data_long %>% filter(Order == "ResponderFirst") %>% select(Pound_returned)
RF <- as.data.frame(RF)
SF <- Data_long %>% filter(Order == "SenderFirst") %>% select(Pound_returned)
SF <- as.data.frame(SF)
ks.test(RF[,1],SF[,1])

# Two-sample Kolmogorov-Smirnov test
# data:  RF[, 1] and SF[, 1]
# D = 0.034775, p-value = 0.01548
# alternative hypothesis: two-sided

ggplot(Data_long, aes(x=Pound_returned)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", fill = "Black") +
  labs(x = "Trustor Response", y = "Percent", fill="Sender Amount") +
  theme(legend.position = "none")+
  scale_x_continuous(breaks=seq(0,30,by=1)) 

### Multilevel Regression 
# Null model
model1 <- lme(fixed = Pound_returned~1, random = ~1|ID, data = Data_long)
summary(model1)
VarCorr(model1)
# Random Intercept Only Model
model2 <- lme(fixed = Pound_returned~Pic.Mask + Pic.Gender + Sex + Order + Sender_Response + Trustworthy, 
              random = ~1|ID, data = Data_long)
summary(model2)
VarCorr(model2)

# Random Intercept and random slope Model
model4 <- lme(fixed = Pound_returned~Pic.Mask + Pic.Gender + Sex + Order + Sender_Response + Trustworthy + Protect_themselves_or_others,
              random = ~Sender_Response|ID, data = Data_long)
summary(model4)
VarCorr(model4)

# Most staurated model
model5 <- lme(fixed = Pound_returned~Pic.Mask + Sex + Pic.Gender +  
                Order + Sender_Response + Sex*Pic.Gender + Sex*Pic.Mask + Pic.Gender*Pic.Mask, 
              random = ~Sender_Response|ID, data = Data_long)

summary(model5)
tab_model(model5, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)

# Exploratory model
model6 <- lme(fixed = Pound_returned~Sex + Pic.Gender + Pic.Mask + Order + Sender_Response + Trustworthy + 
                Protect_themselves_or_others+ Sex*Pic.Gender + Sex*Pic.Mask + Pic.Gender*Pic.Mask , 
              random = ~Sender_Response|ID, data = Data_long)

summary(model6)
tab_model(model6, p.style = "numeric_stars", show.aic = T, auto.label = TRUE)



SF <- Data_long%>%filter(Pic.Mask=="U")
RF <- Data_long%>%filter(Pic.Mask=="M")
cohen.d(SF$Pound_returned,RF$Pound_returned)

VarCorr(model5)
anova(model1, model2, model4, model5)
# Model df       AIC       BIC   logLik   Test  L.Ratio p-value
# model1     1  3 -10523.66 -10503.03 5264.832                        
# model2     2 10 -10553.60 -10484.82 5286.799 1 vs 2 43.93398  <.0001
# model4     3 12 -10576.24 -10493.71 5300.121 2 vs 3 26.64396  <.0001
# model5     4 15 -10554.23 -10451.07 5292.115 3 vs 4 16.01062  0.0011

library(sjPlot)

plot_model(model5,type = "eff", term = c("Sender_Response","trustworthylevel"),
           axis.title = ("Proportion returned")) +
  scale_x_continuous(breaks=seq(0,10,by=1)) +
  theme_classic() 

plot_model(model5,type = "eff", term = c("Sender_Response","Pic.Mask"),
           axis.title = ("Proportion returned")) +
  scale_x_continuous(breaks=seq(0,10,by=1)) +
  theme_classic()

plot_model(model5,type = "eff", term = c("Sender_Response","Order"),
           axis.title = ("Proportion returned")) +
  scale_x_continuous(breaks=seq(0,10,by=1)) +
  theme_classic() 


LT<- Data_long %>% filter(Order == "SenderFirst") %>% select(Pound_returned)
MT<- Data_long %>% filter(Order == "ResponderFirst") %>% select(Pound_returned)
HT<- Data_long %>% filter(trustworthylevel == "high") %>% select(Pound_returned)

cohen.d(MT$Pound_returned,LT$Pound_returned)
cohen.d(HT$Pound_returned,LT$Pound_returned)


## Plots for manuscript  
CI_sender <- cleandata %>% group_by(Sex, Pic.Mask, Pic.Gender) %>% 
  summarise(m = mean(Sender_Response),
  se = sd(Sender_Response) / sqrt(length(Sender_Response))) %>% 
  mutate(Mask = ifelse(Pic.Mask == "U", "Unmasked", "Masked")) %>%
  mutate(Sex = ifelse(Sex == "Female", "Female participant", "Male participant")) %>%
  mutate(Pic.Gender = ifelse(Pic.Gender == "F", "Female counterpart", "Male counterpart"))
  
names(CI_sender)[names(CI_sender) == 'Pic.Gender'] <- 'Picture gender'

plot1 <- CI_sender %>% 
  ggplot(aes(Mask, m)) +
  facet_grid(`Picture gender`~Sex, scales='free_y') +
  geom_col(aes(fill = Mask), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = m - se,
                    ymax = m + se),
                color = "#22292F",
                width = .1) +
  scale_y_continuous(breaks=seq(0,10,by=2)) +
  theme_classic() +
  labs(
    y = "Amount sent by trustor",
    subtitle = "Exp 1"
  ) + 
  scale_fill_grey(start = 0.3) +
   theme(plot.title = element_text(size = 16,
                                   face = "bold", hjust = 0.5),
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
         axis.title=element_text(size=14,face="bold"),
         axis.title.x=element_blank())


CI_trust <- cleandata %>% group_by(Sex, Pic.Mask, Pic.Gender) %>% 
  summarise(m = mean(Trustworthy),
            se = sd(Trustworthy) / sqrt(length(Trustworthy))) %>% 
  mutate(Mask = ifelse(Pic.Mask == "U", "Unmasked", "Masked")) %>%
  mutate(Sex = ifelse(Sex == "Female", "Female participant", "Male participant")) %>%
  mutate(Pic.Gender = ifelse(Pic.Gender == "F", "Female counterpart", "Male counterpart"))

names(CI_trust)[names(CI_trust) == 'Pic.Gender'] <- 'Picture gender'


plot2 <- CI_trust %>% 
  ggplot(aes(Mask, m)) +
  facet_grid(`Picture gender`~Sex) +
  geom_col(aes(fill = Mask), color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = m - se,
                    ymax = m + se),
                color = "#22292F",
                width = .1) +
  scale_y_continuous(limits = c(0, 70),breaks=seq(0,70,by=15)) +
  theme_classic() +
  labs(
    y = "Trustworthiness Ratings",
    subtitle = "Exp 1"
  ) + 
  scale_fill_grey(start = 0.3) +
  theme(plot.title = element_text(size = 16,
                                  face = "bold", hjust = 0.5),
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
        axis.title=element_text(size=14,face="bold"),
        axis.title.x=element_blank())

Data_long <- Data_long %>%
  mutate(Amount_sent = case_when(
    Responder_value =="3" ~ "1",
    Responder_value =="6" ~ "2",
    Responder_value =="9" ~ "3",
    Responder_value =="12" ~ "4",
    Responder_value =="15" ~ "5",
    Responder_value =="18" ~ "6",
    Responder_value =="21" ~ "7",
    Responder_value =="24" ~ "8",
    Responder_value =="27" ~ "9",
    Responder_value =="30" ~ "10"
  ))

Data_long$Amount_sent <- 
  factor(Data_long$Amount_sent, levels = c("1", "2", "3",
                                           "4","5",'6', 
                                           "7","8","9","10"))


CI_return <- Data_long %>% group_by(Sex, Pic.Mask, Pic.Gender, Amount_sent ) %>% 
  summarise(m = mean(Pound_returned),
            se = sd(Pound_returned) / sqrt(length(Pound_returned)))%>% 
  mutate(`Mask manipulation` = ifelse(Pic.Mask == "U", "Unmasked", "Masked"))%>% 
  mutate(Pic.Gender = ifelse(Pic.Gender == "F", "Female counterpart", "Male counterpart"))%>% 
  mutate(Sex = ifelse(Sex == "Female", "Female participant", "Male participant"))

names(CI_return)[names(CI_return) == 'Pic.Gender'] <- 'Picture gender'
 
plot3 <- ggplot(CI_return) +
   aes(x = Amount_sent, y = m, colour = `Mask manipulation`) +
   geom_pointrange(aes(ymin = m - se,
                       ymax = m + se), 
                   position=position_dodge(width=0.5)) + 
   scale_color_hue(direction = 1)  + 
   geom_line(aes(group = `Mask manipulation`),
             position=position_dodge(width=0.5)) +
   labs(
     y = "Proportion returned",
     subtitle = "Exp 1") +
   scale_colour_manual(values=c("Black", "gray60")) +
   facet_grid(`Picture gender`~Sex) +
   theme_classic() +
    theme(plot.title = element_text(size = 16,
                                  face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.title.y.right = element_blank(),                # hide right axis title
        axis.text.y.right = element_blank(),                 # hide right axis labels
        axis.ticks.y = element_blank(),                      # hide left/right axis ticks
        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(3, "mm"),                      # remove spacing between facets
        strip.background = element_rect(size = 0),         # match default line size of theme_classic
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12),            # make facet label bold
        axis.text.x = element_text(size = 12),            # make x axis text bold
        axis.text.y.left=element_text(colour="black"), 
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"),
        legend.position = "none",
        axis.title.x=element_blank())


cowplot::plot_grid(plot1,plot2,plot3, ncol=2, labels = c('A', 'B', 'C'))