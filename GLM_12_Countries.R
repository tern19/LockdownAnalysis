### GLM using the oxford data

## GLM for specific countries
## NA: US, Mexico, Canada
## CA: Trinidad and Tobago, Barbados, Haiti
## SA: Brazil, Paraguay, Venezuela
## EU: UK, Germany, Sweden


## Oxford data methodology for indexes
## https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/index_methodology.md
## https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md


##Loads in the data and any required packages
remotes::install_github("nset-ornl/wbstats", force = TRUE)
remotes::install_github("joachim-gassen/tidycovid19", force = TRUE) #Enter 1 to install all updates
library(tidycovid19)
library(tidyverse)
merged <- download_merged_data(silent = TRUE, cached = TRUE)

library(countrycode)
merged$continent <- countrycode(sourcevar = merged[["country"]],
                                origin = "country.name",
                                destination = "region23")
##Changes 'confirmed' to cases per day rather than cumulative.
##New variable is called 'daily_confirmed'
firstdiff <- function(x) {
  shifted <- c(0,x[1:(length(x)-1)])
  result = x-shifted
  which_negative = which(result<0)
  result[which_negative] = NA
  return(result)
}
merged <- merged %>%
  mutate(daily_confirmed = firstdiff(confirmed))


## Tidying the OxCRGT_latest dataset with proper date formatting as well as adding the continent column. 
OxCGRT_latest <- read.csv("Oxford data 22.11/data/OxCGRT_latest.csv")
OxCGRT_latest$continent = countrycode(sourcevar = OxCGRT_latest[["CountryName"]],
                                      origin = "country.name",
                                      destination = "region23")
library(lubridate)
OxCGRT_latest = OxCGRT_latest %>% mutate(Date = as.Date(parse_date_time(OxCGRT_latest$Date,orders=c("y","ym","ymd"))))

## Selecting our chosen countries after 6th of February since its the reference point for Google mobility tracker.
## (actually 15th since the countries miss google data from before then), and before 9th of November since then countries
## start to have missing oxford or google data due to being so recent. 

GLMDataMerged= {merged %>% filter( ( str_detect(country, "United States") | 
                                       str_detect(country, "Mexico") | 
                                       str_detect(country, "Canada") | 
                                       str_detect(country, "Trinidad & Tobago")  | 
                                       str_detect(country, "Barbados")  | 
                                       str_detect(country, "Haiti")  | 
                                       str_detect(country, "Brazil")  |
                                       str_detect(country, "Paraguay")  |
                                       str_detect(country, "Venezuela")  |
                                       str_detect(country, "United Kingdom")  |
                                       str_detect(country, "Germany")  |
                                       str_detect(country, "Sweden") ) & ((date > as.Date("2020-02-15")) & (date < as.Date('2020-11-09'))) )}

## Changes Trinidad & Tobago to Trinidad and Tobago to match Oxford data
GLMDataMerged = GLMDataMerged %>% mutate_at(vars(country), list(~ ifelse(. == 'Trinidad & Tobago', 'Trinidad and Tobago', .)))

GLMDataOxford= {OxCGRT_latest %>% filter( ( str_detect(CountryName, "United States") | 
                                        str_detect(CountryName, "Mexico") | 
                                        str_detect(CountryName, "Canada") |
                                          str_detect(CountryName, "Trinidad and Tobago") | 
                                          str_detect(CountryName, "Barbados") | 
                                          str_detect(CountryName, "Haiti") | 
                                          str_detect(CountryName, "Brazil") | 
                                          str_detect(CountryName, "Paraguay") | 
                                          str_detect(CountryName, "Venezuela") | 
                                          str_detect(CountryName, "United Kingdom") | 
                                          str_detect(CountryName, "Germany") | 
                                       str_detect(CountryName, "Sweden") ) & ((Date > as.Date("2020-02-15")) & (Date < as.Date('2020-11-09')))  )}

## Removes state level data from US and country level data from UK, leaving only US as a whole and UK as a whole. 
GLMDataOxford = GLMDataOxford %>% filter(!(RegionCode %in% c('UK_ENG', 'UK_NIR', 'UK_SCO', 'UK_WAL',
                                                                'US_AK', 'US_AL', 'US_AR', 'US_AZ', 'US_CA',
                                                                'US_CO', 'US_CT', 'US_DC', 'US_DE', 'US_FL',
                                                                'US_GA', 'US_HI', 'US_IA', 'US_ID', 'US_IL',
                                                                'US_IN', 'US_KS', 'US_KY', 'US_LA', 'US_MA',
                                                                'US_MD', 'US_ME', 'US_MI', 'US_MN', 'US_MO',
                                                                'US_MT', 'US_NC', 'US_ND', 'US_NE', 'US_NH',
                                                                'US_NJ', 'US_NM', 'US_NV', 'US_NY', 'US_OH',
                                                                'US_OK', 'US_OR', 'US_PA', 'US_RI', 'US_SC',
                                                                'US_SD', 'US_TN', 'US_TX', 'US_UT', 'US_VA',
                                                                'US_VI', 'US_VT', 'US_WA', 'US_WI', 'US_WV',
                                                                'US_WY', 'US_MS')))
## Removes United States Virgin Islands from dataset
GLMDataOxford = GLMDataOxford %>% filter(!(CountryName %in% c('United States Virgin Islands')))


## From index_methodology from documentation we can see that only indexes of use to us to respond to the question about lockdown 
## and other government innervations will be: C1-8, E1-2, H1-3 and H6. Where the C indexes will be more closely tied to lockdown
## in here for glm we ' ll be using C5, C6, C7, C8, daily_confirmed and gcmr_residential as predictor variables and gcmr_retail_recreation as outcome



GLMDataMerged = GLMDataMerged %>% select(country, date, gcmr_retail_recreation, gcmr_residential, daily_confirmed, population)
GLMDataOxford = GLMDataOxford %>% select(CountryName, Date, C5_Close.public.transport,
                                                                              C6_Stay.at.home.requirements,
                                                                              C7_Restrictions.on.internal.movement,
                                                                              C8_International.travel.controls)

GLMDataOxford = GLMDataOxford %>% transmute(country = CountryName, date = Date, C5_Close.public.transport = C5_Close.public.transport,
                                  C6_Stay.at.home.requirements = C6_Stay.at.home.requirements,
                                  C7_Restrictions.on.internal.movement = C7_Restrictions.on.internal.movement,
                                  C8_International.travel.controls = C8_International.travel.controls)

GLMDataTotal = merge(GLMDataMerged, GLMDataOxford, by = c('country', 'date'), all = TRUE)
summary(GLMDataTotal)

## the 28 NA's from retail_recreation come nearly all from Haiti not having data for most of period August - September and few dates
## from Barbados. 4 NA's from residential come from April from Barbados.

GLMDataTotal = na.omit(GLMDataTotal)

## now onto fitting the glm
glm1 = glm(gcmr_retail_recreation ~ country + gcmr_residential + daily_confirmed + factor(C5_Close.public.transport) 
           + factor(C6_Stay.at.home.requirements) + factor(C7_Restrictions.on.internal.movement)
           + factor(C8_International.travel.controls), data = GLMDataTotal, family = gaussian)
summary(glm1)

glm2perCapita = glm(gcmr_retail_recreation ~ country + I(daily_confirmed/population) + factor(C5_Close.public.transport) 
           + factor(C6_Stay.at.home.requirements) + factor(C7_Restrictions.on.internal.movement)
           + factor(C8_International.travel.controls), data = GLMDataTotal, family = gaussian)
summary(glm2perCapita)

glm2perCapitaRES = glm(gcmr_retail_recreation ~ country + gcmr_residential + I(daily_confirmed/population) + factor(C5_Close.public.transport) 
                    + factor(C6_Stay.at.home.requirements) + factor(C7_Restrictions.on.internal.movement)
                    + factor(C8_International.travel.controls), data = GLMDataTotal, family = gaussian)
summary(glm2perCapitaRES)

lm2perCapita = lm(gcmr_retail_recreation ~ country + I(daily_confirmed/population) + factor(C5_Close.public.transport) 
                    + factor(C6_Stay.at.home.requirements) + factor(C7_Restrictions.on.internal.movement)
                    + factor(C8_International.travel.controls), data = GLMDataTotal)
summary(lm2perCapita)


## see how auto would find best model
m.min = glm(gcmr_retail_recreation ~ 1, data = GLMDataTotal, family = gaussian)
m.max = glm(gcmr_retail_recreation ~ country + gcmr_residential + I(daily_confirmed/population) + factor(C5_Close.public.transport) 
            + factor(C6_Stay.at.home.requirements) + factor(C7_Restrictions.on.internal.movement)
            + factor(C8_International.travel.controls), data = GLMDataTotal, family = gaussian)

auto.both = step(m.min, direction = 'both', scope = list('lower' = m.min, 'upper' = m.max))
summary(auto.both)

## so from step analysis we can see that best model would be the max model with cases per capita so glm2perCapita
## Now lets graph the trend line out of the model

GLMDataTotal$prediction = predict(glm2perCapita)

TotalPlotWithoutPred = GLMDataTotal %>% ggplot(aes(x=date, y=gcmr_retail_recreation)) + geom_point(aes(colour = country)) +
  labs(title = 'Retail and Recreation in 12 chosen countries', x='Date', y='Mean Retail and Recreation change (% to baseline)') +
  scale_x_date(date_breaks = 'months', date_labels = '%b-%y') + ylim(-100,50)
  
print(TotalPlotWithoutPred)


GLMDataTotal1 = GLMDataTotal[GLMDataTotal$country %in% c("Barbados","Brazil","Canada", "Germany", "Haiti", "Mexico"),]
GLMDataTotal2 = GLMDataTotal[GLMDataTotal$country %in% c("Paraguay","Sweden","Trinidad and Tobago", "United Kingdom", "United States", "Venezuela"),]

annot1st = data.frame(country = c('Barbados', 'Haiti', 'Mexico', 'Paraguay',
                                      'Trinidad and Tobago', 'United Kingdom', 'Venezuela'), 
                          xmin = c(as.Date('2020-04-03'), as.Date('2020-03-19'), as.Date('2020-03-30'), as.Date('2020-03-17'),
                                   as.Date('2020-04-27'), as.Date('2020-03-23'), as.Date('2020-03-13')), 
                          xmax = c(as.Date('2020-07-01'), as.Date('2020-07-19'), as.Date('2020-05-31'), as.Date('2020-05-24'),
                                   as.Date('2020-06-07'), as.Date('2020-05-12'), as.Date('2020-06-01')),
                          ymin = c(-100,-100,-100,-100,-100,-100,-100),
                          ymax = c(50,50,50,50,50,50,50))
annot2nd = data.frame(country = c('Paraguay', 'Venezuela'), 
                      xmin = c(as.Date('2020-08-25'), as.Date('2020-07-22')),
                      xmax = c(as.Date('2020-10-04'), as.Date('2020-11-08')),
                      ymin = c(-100,-100),
                      ymax = c(50,50))


TotalPlot= GLMDataTotal %>% ggplot(aes(x=date, y=gcmr_retail_recreation)) + geom_point(aes(colour = country)) + 
  geom_line(aes(y=prediction), color = 'black') +
  facet_wrap(~ country, labeller = label_wrap_gen(width=5))+
  labs(title = 'Retail and Recreation in 12 chosen countries with trend lines from model', x='Date', y='Retail and Recreation change (% to baseline)') +
  ylim(-100,50) + scale_x_date(date_breaks = 'months', date_labels = '%b') +
  geom_rect(data = annot1st, aes(x=NULL, y= NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, alpha = 0.05, fill = 'red')) +
  geom_rect(data = annot2nd, aes(x=NULL, y=NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, alpha =0.05, fill ='blue')) +
  scale_fill_manual(values = c('lightblue2', 'skyblue2'), name = 'Countrywide \nLockdown', breaks = c('red', 'blue'), 
                    labels = c('1st Lockdown', '2nd Lockdown')) + guides(colour = FALSE, alpha =FALSE)
  


g1 = GLMDataTotal1 %>% ggplot(aes(x=date, y=gcmr_retail_recreation)) + geom_point(aes(colour = country)) + 
  geom_line(aes(y=prediction), color = 'black') +
  facet_wrap(~ country, labeller = label_wrap_gen(width=10))+
  labs(title = 'Retail and Recreation in 12 chosen countries with fitted model', x='Date', y='Mean Retail and Recreation change (% to baseline)') +
  ylim(-100,50)  + scale_x_date(date_breaks = 'months', date_labels = '%b')

g2 = GLMDataTotal2 %>% ggplot(aes(x=date, y=gcmr_retail_recreation)) + geom_point(aes(colour = country)) + 
  geom_line(aes(y=prediction), color = 'black') +
  facet_wrap(~ country, labeller = label_wrap_gen(width=))+
  labs(title = 'Retail and Recreation in 12 chosen countries with fitted model', x='Date', y='Mean Retail and Recreation change (% to baseline)') +
  ylim(-100,50) + scale_x_date(date_breaks = 'months', date_labels = '%b')


print(TotalPlot)
print(g1)
print(g2)

## DEF LOCKDOWN: C6 on level of at least 2 as well as C7 on level of 2 AND both flags equal 1 so its countrywide
## barbados 03.04 - 01.07 1st lockdown
## brazil no lockdown 
## canada no lockdown
## germany no lockdown
## haiti 19.03 - 19.07 1st lockdown
## mexico 30.03 - 31.05 1st lockdown
## paraguay 17.03 - 24.05 1st lockdown 25.08- 04.10 2nd lockdown
## sweden no lockdown
## trinidad and tobago 27.04 - 07.06 1st lockdown 
## uk 23.03 - 12.05 1st lockdown 
## usa no lockdown
## venezuela 13.03 - 01.06 1st lockdown 22.07 - 08.11 2nd lockdown





