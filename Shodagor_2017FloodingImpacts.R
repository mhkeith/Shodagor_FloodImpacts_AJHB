#### Code for Shodagor 2017 flooding analysis- economic and nutritional impacts ####

## load packages
library(tidyverse); library(zscorer); library(brms); library(cowplot); library(lubridate); library(ggalluvial)

## navigate to working directory with data
## load datasets
popfish.monthly <- read.csv(file="PopFish.monthly.csv") ## monthly population 3-day fishing totals
fish.dat <- read.csv(file="FishData.long.csv") ## long-form fish catch and income data 
fish.median <- read.csv(file="FishMedian.csv") ## fish catch and income weekly medians
expend.dat <-read.csv(file="ExpendData.csv") ## household food expenditure data
anthro.cdat <- read.csv(file="AnthroC.csv") ## child anthropometric data
anthro.adat <- read.csv(file="AnthroA.csv") ## adult anthropometric data
nutr.cdat <- read.csv(file="Shodagor.ModC.csv") ## child nutrition model data
nutr.adat <- read.csv(file="Shodagor.ModA.csv") ## adult nutrition model data

###-------------------------Fish Catch and Income----------------------------###
## format dates to plot 2017 versus 2018
popfish.monthly$Date <- as.Date(popfish.monthly$Date, format="%m/%d/%Y")
popfish2 <- popfish.monthly %>% mutate (Year = lubridate::year(Date), DayMonth = format(Date, "%d-%m"))
popfish2$Year <- as.character(popfish2$Year)
popfish2$DayMonth <- as.Date(popfish2$DayMonth, format="%d-%m")

## plot monthly 3-day fish catch and income totals for the 20 households with complete data July-October 2017 and 2018
popfish.f2 <- popfish2 %>% ggplot(aes(x=DayMonth, y=Catch.Total, stratum=Year, alluvium=Year)) +
  geom_alluvium(aes(fill=Year), decreasing=FALSE) + theme_minimal() +
  scale_fill_manual(values=c("darkmagenta", "darkorange2"), name="") + xlab("") +
  ylab("Monthly 3-day fish catch total (kg)") + scale_x_date(date_labels = "%B") +
  theme(legend.position = c(0.85,0.85), legend.background = element_rect(fill="white", linetype = "solid", color="White"))

popincome.f2 <- popfish2 %>% ggplot(aes(x=DayMonth, y=Income.Total, stratum=Year, alluvium=Year)) +
  geom_alluvium(aes(fill=Year), decreasing=FALSE) + theme_minimal() +
  scale_fill_manual(values=c("darkmagenta", "darkorange2"), name="") + xlab("") +
  ylab("Monthly 3-day fishing income total (taka)") + scale_x_date(date_labels = "%B") +
  theme(legend.position="none")  

plot_grid(popfish.f2, popincome.f2, ncol=1) ## Figure 2
#ggsave("Figure2.tiff", plot=last_plot(), dpi=300, height=10, width=9, units="in")

## format daily data to plot 2017 versus 2018
fish.dat$Fish.Date <- as.Date(fish.dat$Fish.Date, format="%m/%d/%Y") 
fish.dat2 <- fish.dat %>% mutate(Year = lubridate::year(Fish.Date), DayMonth = format(Fish.Date, "%d-%m"))
fish.dat2$Year <- as.character(fish.dat2$Year)
fish.dat2$DayMonth <- as.Date(fish.dat2$DayMonth, format="%d-%m")
fish.dat3 <- fish.dat2[fish.dat2$DayMonth >= "2022-07-01" & fish.dat2$DayMonth <= "2022-10-31", ]
fish.median$Date <- as.Date(fish.median$Date, format="%m/%d/%Y")
fish.median2 <- fish.median %>% mutate(Year = lubridate::year(Date), DayMonth = format(Date, "%d-%m"))
fish.median2$Year <- as.character(fish.median2$Year)
fish.median2$DayMonth <- as.Date(fish.median2$DayMonth, format="%d-%m")

## plot daily household fish catches and incomes with weekly median trend lines overlaid (Figure S1) 
fish.plot.s1 <- fish.dat3 %>% ggplot(aes(x=DayMonth, y=KG.Catch, color=Year)) + geom_jitter(alpha=0.3, width=0.5, height=0.5) +
  theme_minimal() + scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  xlab("") + ylab("Daily household fish catch (kg)") + 
  scale_color_manual(values=c("darkmagenta", "darkorange2"), labels=c('2017 weekly median', '2018 weekly median')) +
  theme(legend.title=element_blank(), legend.position = c(0.90,0.90),
        legend.background = element_rect(fill="white", linetype = "solid", color="White")) +
  geom_line(aes(x=DayMonth, y=KG.Catch_median, color=Year), data = fish.median2, size = 1.5)

income.plot.s1 <- fish.dat3 %>% ggplot(aes(x=DayMonth, y=Total.Profit, color=Year)) + geom_jitter(alpha=0.3, width=0.5, height=0.6) +
  theme_minimal() + scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  xlab("") + ylab("Daily household fishing income (taka)") + 
  scale_color_manual(values=c("darkmagenta", "darkorange2"), labels=c('2017 weekly median', '2018 weekly median')) +
  theme(legend.position="none") +
  geom_line(aes(x=DayMonth, y=Total.Profit_median, color=Year), data = fish.median2, size = 1.5)

plot_grid(fish.plot.s1, income.plot.s1, ncol=1) ## Figure S1
#ggsave("FigureS1.tiff", plot=last_plot(), dpi=300, height=10, width=9, units="in")

###----------------------Household Food Expenditures-------------------------###
expend.dat$Date <- as.Date(expend.dat$Date, format="%m/%d/%Y")
## calculate population-level aggregates across each month for rice, fish, meat, veg, and other
exp.pop.totals <- expend.dat %>% select(-1) %>% group_by(Date) %>% summarise_all(sum) ## totals from the same 27 families every month
## alluvial plot
exp.allu.dat <- exp.pop.totals %>% rename_with(~str_remove(., '.Total')) %>% 
  pivot_longer(!Date, names_to = "Category", values_to = "Amount")
## Figure 3
exp.allu.dat %>% ggplot(aes(x=Date, y=Amount, stratum=Category, alluvium=Category)) +
  geom_alluvium(aes(fill=Category), decreasing=FALSE) + theme_minimal() + xlab("") +
  ylab("Monthly 3-day expenditure totals (taka)") + scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y") +
  scale_fill_manual(values= c("darkorange2", "darkmagenta","turquoise2", "chartreuse", "deeppink"), name="") +
  theme(legend.position = c(0.85,0.85), legend.background = element_rect(fill="white", linetype = "solid", color="White"))
#ggsave("Figure3.tiff", plot=last_plot(), dpi=300, height=6, width=11, units="in")

###------------------------Supplemental Plots 2-4---------------------------###
## violin plots of seasonal child growth rates (change in hfaz, wfaz, bfaz)
anthro.cdat2 <- anthro.cdat %>% filter(Specific.Season>1 & Specific.Season!=6)
anthro.cdat2$Specific.Season <- as.factor(anthro.cdat2$Specific.Season)
hfa.rate.vio <- anthro.cdat2 %>% # seasonal height-for-age growth rates
  select(Specific.Season, hfa.rate, G2)  %>% 
  ggplot(aes(x=Specific.Season, y=hfa.rate)) +
  geom_violin() +geom_jitter(aes(colour=G2), width = 0.1, height = 0, size=1.2, alpha=0.7) + 
  scale_colour_manual(values=c("darkorange2", "darkmagenta")) + 
  theme_minimal() + geom_hline(yintercept = 0, linetype="dashed") + ggtitle("Seasonal change in child anthropometrics") +
  labs(x="", y="Change in height-for-age (z)") + scale_x_discrete(labels=c("2" = "Rainy 2017", "3" = "Dry 2018", 
                                                                           "4" = "Rainy 2018", "5" = "Dry 2019")) + 
  theme(legend.title=element_blank(), legend.position = c(0.92, 0.91)) + ylim(-5.0,5.0)

wfa.rate.vio <- anthro.cdat2 %>% # seasonal weight-for-age growth rates
  select(Specific.Season, wfa.rate, G2) %>% 
  ggplot(aes(x=Specific.Season, y=wfa.rate)) +
  geom_violin() + geom_jitter(aes(colour=G2), width = 0.1, height = 0, size=1.2, alpha=0.7) +
  scale_colour_manual(values=c("darkorange2", "darkmagenta"))+ theme_minimal() + geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="", y="Change in weight-for-age (z)") + scale_x_discrete(labels=c("2" = "Rainy 2017", "3" = "Dry 2018", 
                                                                           "4" = "Rainy 2018", "5" = "Dry 2019")) + 
  theme(legend.position = "none") + ylim(-5.0,5.0)

bfa.rate.vio <- anthro.cdat2 %>% # seasonal BMI-for-age growth rates
  select(Specific.Season, bfa.rate, G2) %>% 
  ggplot(aes(x=Specific.Season, y=bfa.rate)) +
  geom_violin() + geom_jitter(aes(colour=G2), width = 0.1, height = 0, size=1.2, alpha=0.7) + 
  scale_colour_manual(values=c("darkorange2", "darkmagenta"))+
  theme_minimal() + geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Season", y="Change in BMI-for-age (z)") + scale_x_discrete(labels=c("2" = "Rainy 2017", "3" = "Dry 2018", 
                                                                              "4" = "Rainy 2018", "5" = "Dry 2019")) + 
  theme(legend.position = "none") + ylim(-5.0,5.0)

plot_grid(hfa.rate.vio, wfa.rate.vio, bfa.rate.vio, ncol=1) ## Figure S2
#ggsave("FigureS2.tiff", plot=last_plot(), dpi=300, height=10, width=9, units="in")

## plot seasonal change in child BMI-for-age by sex across age
anthro.cdat %>% ggplot(aes(x=age, y=bfa.rate, group=G2, color=G2)) + geom_point() + geom_smooth(aes(fill=G2)) +
  scale_color_manual(values=c("darkorange2", "darkmagenta")) + theme_minimal() + 
  scale_fill_manual(values=c("darkorange2", "darkmagenta")) +
  xlab("Age (years)") + ylab("Seasonal change in BMI-for-age (z)") + ylim(-5.0, 5.0) + 
  geom_hline(yintercept = 0, size = 1) +
  theme(legend.title = element_blank(), legend.position = c(0.84,0.17))
#ggsave("FigureS4.tiff", plot=last_plot(), dpi=300, height=6, width=11, units="in") 

## violin plots of seasonal adult BMI change
anthro.adat2 <- anthro.adat %>% filter(Specific.Season>1 & Specific.Season!=6)
anthro.adat2$Specific.Season <- as.factor(anthro.adat2$Specific.Season)
bmi.rate.vio <- anthro.adat2 %>%  
  select(Specific.Season, bmi.rate, G2) %>% 
  ggplot(aes(x=Specific.Season, y=bmi.rate)) + geom_hline(yintercept = 0, linetype="dashed") +
  geom_violin() + geom_jitter(aes(colour=G2), width = 0.1, height = 0, size=1.2, alpha=0.7) + 
  ggtitle("Seasonal change in adult BMI") +
  scale_colour_manual(values=c("darkorange2", "darkmagenta")) + theme_minimal() +
  labs(x="Season", y="Change in BMI (kg/m^2)") + scale_x_discrete(labels=c("2" = "Rainy 2017", "3" = "Dry 2018", 
                                                                           "4" = "Rainy 2018", "5" = "Dry 2019")) +
  theme(legend.title = element_blank(), legend.position = c(0.92, 0.91))
bmi.rate.vio ## Figure S3
#ggsave("FigureS3.tiff", plot=last_plot(), dpi=300, height=6, width=11, units="in")

###------------------------Seasonal BMI Models-------------------------------###
## seasonal effects on child BMI-for-age 
anthro.cdat3 <- anthro.cdat2 %>% filter(is.na(bfa.rate)==FALSE & Specific.Season!='6')
cbmi.season.mod <- brm(bfa.rate ~ Specific.Season + (1|IndivID), data = anthro.cdat3)
summary(cbmi.season.mod)
conditional_effects(cbmi.season.mod)
# plot
cbmi.season.plot <- conditional_effects(cbmi.season.mod, "Specific.Season")
cbfa.plot <- plot(cbmi.season.plot, plot = FALSE)[[1]] +
  theme_minimal() + labs(x="", y="Seasonal change in child BMI-for-age (z)") + 
  scale_x_discrete(labels=c("2" = "Rainy 2017\n(flood)", "3" = "Dry 2018", "4" = "Rainy 2018", "5" = "Dry 2019")) +
  ylim(-1.0,1.0) + geom_hline(yintercept = 0, linetype="dashed")

## seasonal effects on adult BMI
anthro.adat3 <- anthro.adat2 %>% filter(is.na(bmi.rate)==FALSE & Specific.Season!='6')
Abmi.season.mod <- brm(bmi.rate ~ Specific.Season + (1|IndivID), data = anthro.adat3)
summary(Abmi.season.mod)
conditional_effects(Abmi.season.mod)
# plot
abmi.season.plot <- conditional_effects(Abmi.season.mod, "Specific.Season")
abmi.plot <- plot(abmi.season.plot, plot = FALSE)[[1]] +
  theme_minimal() + labs(x="Season", y="Seasonal change in adult BMI (kg/m^2)") + 
  scale_x_discrete(labels=c("2" = "Rainy 2017\n(flood)", "3" = "Dry 2018", "4" = "Rainy 2018", "5" = "Dry 2019")) +
  ylim(-1.0,1.0) + geom_hline(yintercept = 0, linetype="dashed")

plot_grid(cbfa.plot, abmi.plot, ncol=1) ## Figure 4
#ggsave("Figure4.tiff", plot=last_plot(), dpi=300, height=9, width=9, units="in")

###--------------------------Nutritional Models------------------------------###
## change in child BMI-for-age and adult BMI during the 2017 flood season
## impacts of September 2017 expenditures, fish catch and income, control for proximity to the Meghna and household size
## random-level effect for individual household variance

prior <- c(set_prior("normal(0,0.5)", class="Intercept"),
           set_prior("normal(0,1)", class="b"),
           set_prior("cauchy(0,1)", class="sd"))
## child BMI-for-age model
mod.nutrC <- brm(bfa.rate ~ Meghna.close + hhold.size.r17 + log.rice + log.fish + log.veg + (1|HholdID),
                 data=nutr.cdat, chains=4, cores=2, iter=2000, warmup=1000, 
                 prior=prior, control=list(adapt_delta = 0.99, max_treedepth = 13))
summary(mod.nutrC) 
conditional_effects(mod.nutrC, ask=TRUE)
## plot the conditional effects
# proximity to the Meghna
cbfa.Meghna.plot <- conditional_effects(mod.nutrC, "Meghna.close")
cbfa.Meghna.plot <- as.data.frame(cbfa.Meghna.plot$Meghna.close)
cMeghna.plot <- ggplot(cbfa.Meghna.plot, aes(x=Meghna.close, y=estimate__, color=cond__)) + 
  theme_minimal() + ylim(-2.0, 2.0) +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = cond__), alpha=0.25) + geom_line(size=1, aes(color=cond__)) +
  scale_x_continuous(breaks = c(0,1), labels=c("No", "Yes")) +
  scale_fill_manual(values=c("1" = "turquoise2")) +
  scale_colour_manual(values=c("1" = "turquoise2")) + theme(legend.position="none") +
  labs(x="Close to the Meghna River", y="") + 
  geom_hline(yintercept = 0, linetype="dashed")
# household size
cbfa.hhold.plot <- conditional_effects(mod.nutrC, "hhold.size.r17")
cbfa.hhold.plot <- as.data.frame(cbfa.hhold.plot$hhold.size.r17)
chhold.plot <- ggplot(cbfa.hhold.plot, aes(x=hhold.size.r17, y=estimate__, color=cond__)) + theme_minimal() + ylim(-2.0, 2.0) +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = cond__), alpha=0.25) + geom_line(size=1, aes(color=cond__)) +
  scale_fill_manual(values=c("1" = "darkmagenta")) +
  scale_colour_manual(values=c("1" = "darkmagenta")) + theme(legend.position="none") +
  labs(x="Household size", y="") + 
  geom_hline(yintercept = 0, linetype="dashed")
# rice expenditures 
cbfa.rice.plot <- conditional_effects(mod.nutrC, "log.rice")
cbfa.rice.plot <- as.data.frame(cbfa.rice.plot$log.rice)
crice.plot <- ggplot(cbfa.rice.plot, aes(x=log.rice, y=estimate__, color=cond__)) + theme_minimal() + ylim(-2.0, 2.0) +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = cond__), alpha=0.2) + geom_line(size=1, aes(color=cond__)) +
  scale_fill_manual(values=c("1" = "chartreuse")) +
  scale_colour_manual(values=c("1" = "chartreuse")) + theme(legend.position="none") +
  labs(x="Median rice expenditure (log)", y="Rainy 2017 change in child BMI-for-age (z)") + 
  geom_hline(yintercept = 0, linetype="dashed")
# fish expenditures
cbfa.fish.plot <- conditional_effects(mod.nutrC, "log.fish")
cbfa.fish.plot <- as.data.frame(cbfa.fish.plot$log.fish)
cfish.plot <- ggplot(cbfa.fish.plot, aes(x=log.fish, y=estimate__, color=cond__)) + theme_minimal() + ylim(-2.0, 2.0) +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = cond__), alpha=0.2) + geom_line(size=1, aes(color=cond__)) +
  scale_fill_manual(values=c("1" = "darkorange2")) +
  scale_colour_manual(values=c("1" = "darkorange2")) + theme(legend.position="none") +
  labs(x="Median fish expenditure (log)", y="") + 
  geom_hline(yintercept = 0, linetype="dashed")
# vegetable expenditures
cbfa.veg.plot <- conditional_effects(mod.nutrC, "log.veg")
cbfa.veg.plot <- as.data.frame(cbfa.veg.plot$log.veg)
cveg.plot <- ggplot(cbfa.veg.plot, aes(x=log.veg, y=estimate__, color=cond__)) + theme_minimal() + ylim(-2.0, 2.0) +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = cond__), alpha=0.2) + geom_line(size=1, aes(color=cond__)) +
  scale_fill_manual(values=c("1" = "deeppink")) +
  scale_colour_manual(values=c("1" = "deeppink")) + theme(legend.position="none") +
  labs(x="Median vegetable expenditure (log)", y="") + 
  geom_hline(yintercept = 0, linetype="dashed")
# facet together
cmod.plot <- plot_grid(crice.plot, cfish.plot, cveg.plot, chhold.plot, cMeghna.plot, nrow=1)

## adult BMI model
mod.nutrA <- brm(bmi.rate ~ Meghna.close + hhold.size.r17 + log.rice + log.fish + log.veg + (1|HholdID),
                     data=nutr.adat, chains=4, cores=2, iter=2000, warmup=1000, 
                     prior=prior, control=list(adapt_delta = 0.99, max_treedepth = 13))
summary(mod.nutrA)
conditional_effects(mod.nutrA, ask=TRUE)
## plot the conditional effects
# proximity to the Meghna
abmi.Meghna.plot <- conditional_effects(mod.nutrA, "Meghna.close")
abmi.Meghna.plot <- as.data.frame(abmi.Meghna.plot$Meghna.close)
aMeghna.plot <- ggplot(abmi.Meghna.plot, aes(x=Meghna.close, y=estimate__, color=cond__)) + 
  theme_minimal() + ylim(-2.5, 2.5) +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = cond__), alpha=0.25) + geom_line(size=1, aes(color=cond__)) +
  scale_x_continuous(breaks = c(0,1), labels=c("No", "Yes")) +
  scale_fill_manual(values=c("1" = "turquoise2")) +
  scale_colour_manual(values=c("1" = "turquoise2")) + theme(legend.position="none") +
  labs(x="Close to the Meghna River", y="") + 
  geom_hline(yintercept = 0, linetype="dashed")
# household size
abmi.hhold.plot <- conditional_effects(mod.nutrA, "hhold.size.r17")
abmi.hhold.plot <- as.data.frame(abmi.hhold.plot$hhold.size.r17)
ahhold.plot <- ggplot(abmi.hhold.plot, aes(x=hhold.size.r17, y=estimate__, color=cond__)) +
  theme_minimal() + ylim(-2.5, 2.5) +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = cond__), alpha=0.25) + geom_line(size=1, aes(color=cond__)) +
  scale_fill_manual(values=c("1" = "darkmagenta")) +
  scale_colour_manual(values=c("1" = "darkmagenta")) + theme(legend.position="none") +
  labs(x="Household size", y="") + 
  geom_hline(yintercept = 0, linetype="dashed")
# rice expenditures 
abmi.rice.plot <- conditional_effects(mod.nutrA, "log.rice")
abmi.rice.plot <- as.data.frame(abmi.rice.plot$log.rice)
arice.plot <- ggplot(abmi.rice.plot, aes(x=log.rice, y=estimate__, color=cond__)) + theme_minimal() + ylim(-2.5, 2.5) +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = cond__), alpha=0.2) + geom_line(size=1, aes(color=cond__)) +
  scale_fill_manual(values=c("1" = "chartreuse")) +
  scale_colour_manual(values=c("1" = "chartreuse")) + theme(legend.position="none") +
  labs(x="Median rice expenditure (log)", y="Rainy 2017 change in adult BMI (kg/m^2)") + 
  geom_hline(yintercept = 0, linetype="dashed")
# fish expenditures
abmi.fish.plot <- conditional_effects(mod.nutrA, "log.fish")
abmi.fish.plot <- as.data.frame(abmi.fish.plot$log.fish)
afish.plot <- ggplot(abmi.fish.plot, aes(x=log.fish, y=estimate__, color=cond__)) + theme_minimal() + ylim(-2.5, 2.5) +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = cond__), alpha=0.2) + geom_line(size=1, aes(color=cond__)) +
  scale_fill_manual(values=c("1" = "darkorange2")) +
  scale_colour_manual(values=c("1" = "darkorange2")) + theme(legend.position="none") +
  labs(x="Median fish expenditure (log)", y="") + 
  geom_hline(yintercept = 0, linetype="dashed")
# vegetable expenditures
abmi.veg.plot <- conditional_effects(mod.nutrA, "log.veg")
abmi.veg.plot <- as.data.frame(abmi.veg.plot$log.veg)
aveg.plot <- ggplot(abmi.veg.plot, aes(x=log.veg, y=estimate__, color=cond__)) + theme_minimal() + ylim(-2.5, 2.5) +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = cond__), alpha=0.2) + geom_line(size=1, aes(color=cond__)) +
  scale_fill_manual(values=c("1" = "deeppink")) +
  scale_colour_manual(values=c("1" = "deeppink")) + theme(legend.position="none") +
  labs(x="Median vegetable expenditure (log)", y="") + 
  geom_hline(yintercept = 0, linetype="dashed")
# facet together
amod.plot <- plot_grid(arice.plot, afish.plot, aveg.plot, ahhold.plot, aMeghna.plot, nrow=1)
plot_grid(cmod.plot, amod.plot, ncol=1) ## Figure 5
#ggsave("Figure5.tiff", plot=last_plot(), dpi=300, height=7, width=14, units="in")