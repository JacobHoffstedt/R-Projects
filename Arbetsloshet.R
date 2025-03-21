library(readxl)  # Ensure readxl is loaded for reading Excel files
library(dplyr)
library(ggplot2)
library(scales)
library(Synth)

# Load Data
dat <- read_excel("C:/Users/Jacob/OneDrive/Dokument/Examensarbete VT2025/Arbetslöshet.xlsx", sheet = "Slå ihop1")



dat <- dat%>% 
  mutate(Värde_pop = (Värde / Population) * 1000)


dat <- dat %>%
  mutate(Värde_pop = log1p(Värde_pop))  # Avoids log(0) issue


dat  <- dat %>%
  mutate(logpop = log1p(Population))  # Avoids log(0) issue

dat <- dat %>% mutate(PERIOD = as.Date(PERIOD))



# Define Treated municipalities
treated_municipalities <- c("Ljusdal", "Älvdalen", "Malung", "Örnsköldsvik", 
                            "Bräcke", "Jokkmok", "Ragunda", "Härjedalen")


donor_municipalities <- c(
  "Kil", "Eda", "Torsby", "Storfors", "Hammarö", "Munkfors", "Forshaga", "Grums", "Årjäng", "Sunne",
  "Kristinehamn", "Filipstad", "Hagfors", "Arvika", "Säffle", "Vansbro", "Gagnef", "Leksand", "Rättvik", "Orsa",
  "Smedjebacken", "Mora",
  "Säter", "Hedemora", "Avesta", "Ludvika", "Ockelbo", "Hofors", "Ovanåker", "Nordanstig",
  "Sandviken", "Söderhamn", "Bollnäs", "Hudiksvall", "Ånge", "Timrå", "Härnösand",
  "Kramfors", "Sollefteå",
  "Krokom", "Strömsund", "Åre", "Berg",
  "Nordmaling", "Bjurholm", "Vindeln", "Robertsfors", "Norsjö", "Malå", "Storuman", "Sorsele", "Dorotea",
  "Vännäs", "Vilhelmina", "Åsele", "Lycksele",
  "Arvidsjaur", "Arjeplog",
  "Överkalix", "Kalix", "Övertorneå", "Pajala", "Gällivare", "Älvsbyn",
  "Boden", "Haparanda", "Kiruna"
)




# Filter only treated municipalities
dat_treat <- dat %>%
  filter(KOMMUN %in% treated_municipalities,  # Keep only Treated municipalities
         PERIOD >= as.Date("2010-01-01") & PERIOD <= as.Date("2020-03-01"))

dat_donor <- dat %>%
  filter(KOMMUN %in% donor_municipalities,  # Keep only Treated municipalities
         PERIOD >= as.Date("2010-01-01") & PERIOD <= as.Date("2020-03-01"))



# Aggregate and calculate outcome

dat_treat_agg <- dat_treat %>%
  group_by(PERIOD) %>%
  summarise(Värde_pop = mean(Värde_pop, na.rm = TRUE), .groups = "drop")

dat_donor_agg <- dat_donor %>%
  group_by(PERIOD) %>%
  summarise(Värde_pop = mean(Värde_pop, na.rm = TRUE), .groups = "drop")


#Aggregate and calculate population
dat_pop_agg <- dat_treat %>%
  group_by(PERIOD) %>%
  summarise(Population = mean(Population, na.rm = TRUE), .groups = "drop")

#Aggregate and calculated logpop
dat_logpop_agg <- dat_treat %>%
  group_by(PERIOD) %>%
  summarise(logpop = mean(logpop, na.rm = TRUE), .groups = "drop")


# Create a data frame for the aggregated treated units and their outcomes by period
kommun_agg <- data.frame(
  PERIOD = dat_treat_agg$PERIOD,  # Assuming PERIOD matches
  LÄN = "Aggregated",  # Placeholder, since this is a combined unit
  KOMMUN = "aa",
  ARBETSLÖSA = NA,  # If not applicable, use NA,
  Värde = NA,  # If needed
  Population = dat_pop_agg$Population,  # If needed
  Värde_pop = dat_treat_agg$Värde_pop,  # Assuming these are your aggregated values
  logpop = dat_logpop_agg$logpop
)


#Add the dataframe to dat
dat <- bind_rows(dat, kommun_agg)



#KOMMUN_ID
dat <- dat %>%
  mutate(KOMMUN_ID = as.numeric(factor(KOMMUN)))

donor_ids <- dat %>%
  filter(KOMMUN %in% donor_municipalities) %>%
  distinct(KOMMUN_ID) %>%
  pull(KOMMUN_ID)  # Extracts as a vector

#### SCM ####

#Time variable
dat <- dat %>%
  mutate(time = as.numeric(format(PERIOD, "%Y")) + (as.numeric(format(PERIOD, "%m")) - 1) / 12)
#Treatment period

pre_treatment <- sort(unique(dat$time[dat$time < 2018.5 & dat$time %in% unique(dat$time)]))
post_treatment <- sort(unique(dat$time[dat$time >= 2018.5 & dat$time %in% unique(dat$time)]))


#Rounding time to 3 decimals in order for the below variables to match time varible.
dat <- dat %>%
  mutate(time = round(time, 3))  # Round to 3 decimals to match pre_treatment
pre_treatment <- round(pre_treatment, 3)
post_treatment <- round(post_treatment, 3)

#Ensuring everything is as dataframe for scm to run

dat <- as.data.frame(dat)
dat_donor <- as.data.frame(dat_donor)
dat_donor_agg <- as.data.frame(dat_donor_agg)
dat_treat <- as.data.frame(dat_treat)
dat_treat_agg <- as.data.frame(dat_treat_agg)

pre_treatment <- as.numeric(pre_treatment)

dataprep.out <- dataprep(
  foo = dat,
  predictors = c("Värde_pop", "Population"),  
  predictors.op = "mean",
  dependent = "Värde_pop",
  unit.variable = "KOMMUN_ID",  # Use numeric ID instead of KOMMUN name
  time.variable = "time",  
  treatment.identifier = 128, # Get numeric ID of treated unit 1 agg ljusdal 128
  controls.identifier = donor_ids,  # Get numeric IDs of donor units
  time.predictors.prior = pre_treatment,
  time.optimize.ssr = pre_treatment,
  time.plot = c(pre_treatment, post_treatment),
  unit.names.variable = ("KOMMUN")
)
synth.out <- synth(dataprep.out)




#See weights of donor municipalities 
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.tables)

#gaps between outcome and synthetic unit
gaps<- dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
) ; gaps
gaps

#MSPE

rownames(gaps) <- as.character(round(as.numeric(rownames(gaps)), 3))
pre_treatment <- as.character(round(pre_treatment, 3))
print(rownames(gaps))
print(pre_treatment)
all(pre_treatment %in% rownames(gaps))


matched_rows_pre <- match(pre_treatment, rownames(gaps))  # Find row positions
matched_rows_post <- match(post_treatment, rownames(gaps))
MSPE_pre <- mean((gaps[matched_rows_pre, ])^2, na.rm = TRUE)
MSPE_post <- mean((gaps[matched_rows_post, ])^2, na.rm = TRUE)

MSPE_ratio <- MSPE_post / MSPE_pre
MSPE_ratio
MSPE_post
MSPE_pre



#Plotting difference between Y1 and Y0
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)
#Plotting Y1 and Y0
path.plot(
  synth.res = synth.out, 
  dataprep.res = dataprep.out,
  Ylab = "Number of unemployed per 1000 inhabitants (logged)",
  Xlab = "Time",
  Legend = c("Treated", "Synthetic"),
  Legend.position = "bottomright"
)







na_ratio <- sum(is.na(dat_treat$Värde_log)) / nrow(dat_treat)
na_ratio

na_ratio <- sum(is.na(dat_donor$Värde_log)) / nrow(dat_donor)
na_ratio


zero_ratio <- sum(dat_treat$Värde_log == 0, na.rm = TRUE) / nrow(dat_treat)
zero_ratio

zero_ratio <- sum(dat_donor$Värde_log == 0, na.rm = TRUE) / nrow(dat_donor)
zero_ratio

mean_treat <- mean(dat_treat_agg$Avg_Värde, na.rm = TRUE)
mean_donor <- mean(dat_donor_agg$Avg_Värde, na.rm = TRUE)
mean_treat
expm1(mean_treat)
mean_donor 
expm1(mean_donor)




  