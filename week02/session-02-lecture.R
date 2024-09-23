# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EEEB 3005 - Introduction to Statistics
# Session 02: Lecture Code
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Data Management ####
### ~~~~~~~~~~~~~~~~~~~~


# get the penguin data
library(palmerpenguins)
pd <- data.frame(penguins_raw)
pd_simple <- data.frame(penguins)
summary(pd)

# frequency distribution by date
table(pd$Date.Egg)

# check data type for species variable
class(pd$Species)

# open tidyverse for data management
library(package = "tidyverse")

# change variable from character to factor
pd_cleaned <- pd %>%
  mutate(Species = as.factor(Species))

# check data type again
class(pd_cleaned$Species)

# to see how R stores factors as integer values internally: 
as.numeric(pd_cleaned$Species)

# Recode species to simplify
pd_cleaned <- pd_cleaned %>%
  mutate(Species = recode_factor(Species,
                                 `Adelie Penguin (Pygoscelis adeliae)` = 'Adelie',
                                 `Chinstrap penguin (Pygoscelis antarctica)` = 'Chinstrap',
                                 `Gentoo penguin (Pygoscelis papua)` = 'Gentoo'))



### Frequency Tables ####
### ~~~~~~~~~~~~~~~~~~~~~

# simple table of frequencies by species
table(pd_cleaned$Species)


# use freq from the descr package to make a table of frequencies and percents
# suppress the bar plot that automatically prints
# also calculates valid percent in each group, excluding NAs
descr::freq(pd_cleaned$Sex, plot = FALSE)


# You can do this with tidyverse, with much more code! 
pd_cleaned %>%
  group_by(Sex) %>%
  summarize(Frequency = n()) %>%
  mutate(Percent = 100 * (Frequency / sum(Frequency)),
         `Valid Percent` = ifelse(
           !is.na(Sex),
           100 * (Frequency / (sum(Frequency[!is.na(Sex)]))),
           NA))





### Central Tendency ####
### ~~~~~~~~~~~~~~~~~~~~~

# table with frequencies from the bill length variable
# too many categories! 
table(pd_cleaned$Culmen.Length..mm.)


### Mean

# see how mean is influenced by outliers
# create salaries vector and find its mean
salaries <- c(25000, 62000, 41000, 96000, 41000)
mean(salaries)

# add Bill Gates
salaries.gates <- c(salaries, 11500000000)


### Median is more robust! 

# find the mean of the vector with gates
mean(salaries.gates)

# median salary without Bill Gates
median(salaries)

# median salary with Bill Gates
median(salaries.gates)




### Penguin Data ####

# check the bill length data
# examine PHYSHLTH to check data management
summary(pd_cleaned$Culmen.Length..mm.)


#### Make a histogram
# simple version
hist(pd$Flipper.Length..mm.)

# ggplot version (we will talk more about this next time)
pd_cleaned %>%
  ggplot(aes(x = Flipper.Length..mm.)) +
  geom_histogram(fill = "lightblue", 
                 color = "black") + 
  theme_minimal() + 
  labs(title = "Histogram of flipper length", 
       x = "Flipper length (mm)", 
       y = "Count") + 
  theme(axis.text = element_text(size = 13))


# get mean
# note the na.rm = TRUE, or else we get NA for mean and median
mean(x = pd_cleaned$Flipper.Length..mm., na.rm = TRUE)

# get median
median(x = pd_cleaned$Flipper.Length..mm., na.rm = TRUE)

# get mode
# see slide for explanation on why this works and how
which.max(tabulate(pd_cleaned$Flipper.Length..mm.))


# repeat calculations by species using dplyr
pd_cleaned %>% 
  group_by(Species) %>% 
  summarize(mean = mean(Flipper.Length..mm., na.rm = TRUE),
            median = median(Flipper.Length..mm., na.rm = TRUE),
            mode = which.max(tabulate(Flipper.Length..mm.)))


# we can make two different histograms to see the difference
# between males and females
pd_cleaned %>%
  ggplot(aes(x = Flipper.Length..mm.)) +
  geom_histogram(color = "black", fill = "lightblue") + 
  theme_minimal() + 
  labs(title = "Histogram of flipper length", 
       x = "Flipper length (mm)", 
       y = "Count") + 
  theme(axis.text = element_text(size = 13)) + 
  facet_wrap(~Species, nrow = 3)


#### Make a summary table by Sex and Species
pd_cleaned %>%
  group_by(Species, Sex) %>% 
  drop_na(Flipper.Length..mm.) %>%
  summarize(mean.days = mean(Flipper.Length..mm.),
            sd.days = sd(Flipper.Length..mm.),
            var.days = var(Flipper.Length..mm.),
            med.days = median(Flipper.Length..mm.),
            iqr.days = IQR(Flipper.Length..mm.),
            mode.days = which.max(tabulate(Flipper.Length..mm.)))




### Make Table 1 ####
### ~~~~~~~~~~~~~~~~~

library(table1)
table1(data = pd_simple, 
       ~ sex + 
         island + 
         flipper_length_mm + 
         body_mass_g | species,
       topclass="Rtable1-zebra",
       rowlabelhead = "Variables")

