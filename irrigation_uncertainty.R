## ----setup, include=FALSE----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----preliminary steps, results="hide", message=FALSE, warning=FALSE---------------------

# PRELIMINARY STEPS ------------------------------------------------------

# Before starting the analysis, we define a function to load all R 
# packages required in one go, and load them. We then create a function 
# to read in all the spreadsheets of the excel file with the data 
# needed for the analysis, and read in the data. Finally, we cast a 
# function to define the theme of the plots that will be created in 
# this work. 

# Define function to read in all required libraries in one go:
loadPackages <- function(x) {
  for(i in x) {
    if(!require(i, character.only = TRUE)) {
      install.packages(i, dependencies = TRUE)
      library(i, character.only = TRUE)
    }
  }
}

# Load all required libraries:
loadPackages(c("data.table", "fitdistrplus", "fGarch", "readxl", "countrycode",
               "scales", "tidyverse", "cowplot", "mvoutlier", "complmrob",
               "randtoolbox","robustbase", "parallel", "smatr", "boot", 
               "doParallel", "sensitivity", "wesanderson", 
               "grid", "gridExtra", "NbClust", "benchmarkme"))

# install and load sensobol 0.2.1
PackageURL <- "https://cran.r-project.org/src/contrib/Archive/sensobol/sensobol_0.2.1.tar.gz"
install.packages(PackageURL, repos=NULL, type="source")
library(sensobol)

# Set checkpoint

dir.create(".checkpoint")
library("checkpoint")

checkpoint("2020-03-11", 
           R.version ="3.6.1", 
           checkpointLocation = getwd())

# Define function to read in all excel spreadsheets in one go:
readAll <- function(name, tibble = FALSE) {
  sheets <- excel_sheets(name)
  df <- lapply(sheets, function(y) read_excel(name, 
                                             sheet = y))
  if(!tibble) df <- lapply(df, as.data.frame)
  names(df) <- sheets
  df
}

# Read in all excel spreadsheets:
df <- readAll("full.dataset2.xlsx") %>%
  lapply(., function(x) mutate_if(x, is.character, as.factor))

# Redefine column names for population.estimate spreadsheet
colnames(df$population.estimate) <- c("Estimate", "Continent", "Codes", 
                                      paste0("Year.", 2015:2100))

# Create function for custom plot themes
theme_AP <- function() {
  theme_bw() +
    theme(aspect.ratio = 1, 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.background = element_rect(fill = "transparent", 
                                           color = NA), 
          legend.key = element_rect(fill = "transparent", 
                                    color = NA))
} 


## ----datasets, warning=FALSE, cache=TRUE-------------------------------------------------

# PREPARE DATASETS -------------------------------------------------------

# Prepare population dataset
pop <- df$pop %>%
  # Select population between 1999-2012 to
  # get the mean and concur with (mean) values
  # for irrigated areas attested by Meier et al.
  # between 1999-2012
  select(Continent, Country, Year.1999:Year.2012) %>%
  gather(Year, Population, Year.1999:Year.2012) %>%
  # Multiply to get actual population
  mutate_at(vars(Population), funs(.*1000)) %>%
  separate(., Year, into = c("dummy", "Year")) %>%
  mutate_at(vars(Year), funs(as.numeric)) %>%
  select(-dummy)

# Obtain number codes for each country to ease merging
# with irrigated areas at the country level
pop$Codes <- countrycode(pop$Country, origin = "country.name", 
                         destination = "un")


# Create temporal list of data frame splitted by Dataset
temp <- df$meier %>%
  gather(Dataset, Area.irrigated, Meier.et.al.2018:Thenkabail.et.al.2009) %>%
  split(., .$Dataset)

# MERGE EACH DATASET WITH CORRESPONDING POPULATION VALUES ----------------

df.meier <- list()

for(i in names(temp)) {
  if(i == "Thenkabail.et.al.2009") {
    # Merge with population from 1999
    df.meier[[i]] <- pop %>%
      filter(Year == 1999) %>%
      inner_join(., temp[[i]],
                 by = c("Continent", "Country", "Codes"))
  } 
  if(i == "Salmon.et.al.2015") {
    # Merge with population from 2005
    df.meier[[i]] <- pop %>%
      filter(Year == 2005) %>%
      inner_join(., temp[[i]], 
                 by = c("Continent", "Country", "Codes"))
  } 
  if(i == "Siebert.et.al.2013") {
    df.meier[[i]] <- pop %>%
      # Merge with mean population values 2000-2008
      spread(., Year, Population) %>%
      select(Continent, Country, `2000`:`2008`) %>%
      gather(Year, Population, `2000`:`2008`) %>%
      group_by(Country, Continent) %>%
      summarise(Population = mean(Population)) %>%
      mutate(Year = "2000.2008") %>%
      inner_join(., temp[[i]], 
                 by = c("Continent", "Country"))
  } else {
    # For Meier et al.2018, Aquastat and FAOSTAT
    df.meier[[i]] <- pop %>%
      # Merge with mean population values 1999-2012
      group_by(Country, Continent) %>%
      summarise(Population = mean(Population)) %>%
      # Add dummy year column
      mutate(Year = "1999.2012") %>%
      inner_join(., temp[[i]], 
                 by = c("Continent", "Country"))
  }
}

# Create dataset

cols <- c("Population", "Area.irrigated")

df.meier <- df.meier %>%
  rbindlist() %>%
  .[, .(Country, Continent, Codes, 
    Area.irrigated, Population, Dataset)] %>%
  # Drop countries with no irrigated area
  .[!Area.irrigated == 0] %>%
  # Drop Oceania due to small sample size
  .[!Continent == "Oceania"] %>%
  .[, (cols):= .SD / 10^6, .SDcols = (cols)]


## ----density_population, cache=TRUE, dependson="datasets", dev="tikz", fig.height=8.7, fig.width=4, fig.cap="Scatter plots of measures of population density against irrigated areas."----

# PLOT IRRIGATED AREAS AGAINST POPULATION DENSITY MEASURES ---------------------

# Read in dataset
density.population <- fread("density_population.csv")

# Rename variables
density.population <- density.population[, Variable:= ifelse(Variable %in% 
                                                               "Total area of the country (excl. coastal waters)", "Area.country", "Area.cultivated")]

# Reduce dataset
density.population <- density.population[, c(2, 4, 5):= NULL]

# Spread
dens <- spread(density.population, Variable, Value)

cols <- c("Population", "Area.irrigated")

# Plot
inner_join(df.meier, dens, by = "Country") %>%
  data.table() %>%
  .[, `Area cultivable`:= Population / ((Area.cultivated * 1000) / 10^6)] %>%
  .[, `Area country`:= Population / ((Area.country * 1000) / 10^6)] %>%
  gather(parameter, value, `Area cultivable`:`Area country`) %>%
  ggplot(., aes(value, Area.irrigated, 
                color = Continent)) +
  geom_point() +
  facet_grid(Dataset~parameter) +
  labs(x = "Population density (M/Mha)", 
       y = "Irrigated area (Mha)") +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(legend.position = "top", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent", 
                                         color = NA), 
        legend.key = element_rect(fill = "transparent", 
                                  color = NA))


## ----population_vs_irrigation, dependson="datasets", fig.align="center", fig.height=4.7, dev="tikz", figh.width=5.5, cache=TRUE, fig.cap="Scatter plots of irrigated areas against population. The strip label indicates the data set used to plot the values for irrigated areas [@FAO2016b; @FAO2017a; @Meier2018; @Salmon2015; @Siebert2013; @Thenkabail2009]. All those data sets have been compiled and studied by @Meier2018. The population data was retrieved from the @UnitedNations2017c."----

# PLOT SCATTER PLOT OF IRRIGATED AREA AND POPULATION ---------------------

df.meier %>%
  ggplot(., aes(Population, Area.irrigated,
                color = Continent)) +
  geom_point() +
  facet_wrap(~Dataset) +
  labs(x = "Population (M)", 
       y = "Irrigated area (Mha)") +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  theme_AP() +
  theme(legend.position = "top")


## ----read_aquastat_dataset, cache=TRUE---------------------------------------------------

# READ IN AQUASTAT DATA SET ---------------------------------------------------

aquastat <- fread("aquastat.csv", 
                  nrows = 3562)

# ARRANGE AQUASTAT DATA SET ---------------------------------------------------

aquastat <- setnames(aquastat, c("Area", "Variable Name"), 
                     c("Country", "Variable"))

cols <- c("Country", "Variable")

aquastat <- aquastat[, (cols):= lapply(.SD, factor), .SDcols = cols] %>%
  .[, Variable:= fct_recode(Variable, "Water.withdrawal" = "Irrigation water withdrawal", 
                            "Water.requirement" = "Irrigation water requirement")] %>%
  .[Variable == "Water.withdrawal" |Variable == "Water.requirement", 
    .(Country, Variable, Year, Value)] %>%
  .[!c(Year <= 1999 | Year >= 2012)] # Retain only years 1999-2012


## ----function_code, cache=TRUE-----------------------------------------------------------

# FUNCTIONS TO CODE -----------------------------------------------------------

getCodes <- function(x) countrycode(x, origin = "country.name", destination = "un")
getContinent <- function(x) countrycode(x, origin = "country.name", destination = "continent")
getCountry <- function(x) countrycode(x, origin = "un", destination = "country.name")

addAll <- function(dt, dataset) {
  if(is.data.table(dt) == FALSE) {
    setDT(dt)
  }
  dt[, Codes:= lapply(.SD, getCodes), .SDcols = "Country"] %>%
    .[, Continent:= lapply(.SD, getContinent), .SDcols = "Country"] %>%
    .[, Country:= lapply(.SD, getCountry), .SDcols = "Codes"] %>%
    .[, Dataset:= dataset]
}


## ----compute_water_requirements, cache=TRUE, dependson=c("read_aquastat_dataset", "function_code")----

# CODE COUNTRY AND CONTINENT --------------------------------------------------

aquastat <- addAll(aquastat, "Aquastat")

aquastat.dt <- spread(aquastat, Variable, Value)[
  !c(Year <= 1999 | Year >= 2012) # Retain only years 1999-2012
  ]

# READ IN TABLE 4 DATA SET ----------------------------------------------------

table4 <- fread("table_4.csv",
                skip = 3,
                nrows = 167) %>%
  .[, c(2, 5, 7):= NULL]

# CODE COUNTRY AND CONTINENT --------------------------------------------------

table4.dt <- addAll(table4, "Table.4") %>%
  .[!Continent == "Oceania"] %>%
  .[, .(Country, Year, Codes, Continent, Dataset, Water.requirement, Water.withdrawal)] %>%
  .[!c(Year <= 1999 | Year >= 2012)] # Retain only years 1999-2012

# MERGE AQUASTAT AND TABLE 4 DATASETS -----------------------------------------

water.dt <- rbind(aquastat.dt, table4.dt) %>%
  melt(., measure.vars = c("Water.requirement", "Water.withdrawal")) %>%
  .[, .(Max = max(value, na.rm = TRUE), 
        Min = min(value, na.rm = TRUE)), 
    by = .(Continent, variable, Country)]

# Transform Inf values in NA
is.na(water.dt) <- do.call(cbind,lapply(water.dt,
                                        is.infinite))

# READ IN MEIER ET AL. DATASET ------------------------------------------------

meier.dt <- df$meier %>%
  data.table() %>%
  .[!Continent == "Oceania"] %>%
  .[, (4:9):= lapply(.SD, function(x) x / 10^6), .SDcols = (4:9)]

# CODE COUNTRY AND CONTINENT --------------------------------------------------

meier.dt <- addAll(meier.dt, "Meier")[, Dataset:= NULL]

# CREATE FINAL DATASET --------------------------------------------------------

dt_water <- melt(water.dt, measure.vars = c("Max", "Min"), 
                 variable.name = "Stat") %>%
  .[, .(Value = mean(value, na.rm = TRUE)), 
    by = .(Continent, variable, Country)] %>%
  .[meier.dt, on = c("Country", "Continent")] %>%
  .[!variable %in% NA] %>% # Remove rows in variable with NA
  melt(., measure.vars = c(6:11), 
       variable.name = "Dataset", 
       value.name = "Area.irrigated") %>%
  .[, Dataset:= factor(Dataset, levels = c("Aquastat", "FAOSTAT", "Meier.et.al.2018", 
                                           "Salmon.et.al.2015", "Siebert.et.al.2013",
                                           "Thenkabail.et.al.2009"))]

dt.full <- dt_water[variable == "Water.requirement"] %>%
  .[df.meier, on = c("Continent", "Country" ,"Dataset", "Codes", "Area.irrigated")] %>%
  setnames(., "Value", "Water") %>%
  .[, .(Continent, Country, Codes, Dataset, Area.irrigated, Population, Water)]


## ----plot_water_variables, cache=TRUE, dependson="compute_water_requirements", dev="tikz", fig.height=8.7, fig.width=4----

# PLOT WATER VARIABLES AGAINST IRRIGATED AREAS ---------------------------

ggplot(dt_water, aes(Area.irrigated, Value,
               color = Continent)) +
  geom_point() +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Area irrigated (Mha)",
       y = expression(Km^3/yr)) +
  facet_grid(Dataset ~ variable) +
  theme_bw() +
  theme(legend.position = "top", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent",
                                         color = NA),
        legend.key = element_rect(fill = "transparent",
                                  color = NA))


## ----barplots, dev="tikz", echo=TRUE, fig.cap="The extension of irrigation documented by different authors and institutions. @FAO2016b and FAOSTAT [@FAO2017a] provide official values based on national surveys, census and statistics. @Siebert2013 merges FAOSTAT and Aquastat values with independent maps and remote sensing imagery. @Salmon2015 integrates national and subnational surveys with remote sensing and gridded climate data sets. @Thenkabail2009 relies on remote sensing, Google Earth, and ground control points. @Meier2018 downscales the map by @Siebert2013 and uses multi-temporal normalized difference vegetation indexes with agricultural suitability data. The data was retrieved from @Meier2018.", cache=TRUE, fig.width=5.5, fig.height=3.5----

# PLOT DIFFERENCES IN THE MEASUREMENT OF IRRIGATED AREAS
# AS A FUNCTION OF DATASET (CONTINENTAL) (Figure 2) ----------------------

# Create data frame with total irrigated areas per dataset
total <- df$meier %>%
  filter(!Continent == "Oceania") %>%
  gather(Dataset, Value, Meier.et.al.2018:Thenkabail.et.al.2009) %>%
  group_by(Dataset) %>%
  summarise(Total = sum(Value, na.rm = T) / 10^6) %>%
  data.frame()

# Bar plot with continental and total irrigated areas per dataset
df$meier %>%
  gather(Dataset, Value, Meier.et.al.2018:Thenkabail.et.al.2009) %>%
  filter(!Continent == "Oceania") %>%
  group_by(Continent, Dataset) %>%
  summarise(Total = sum(Value, na.rm = T) / 10^6) %>%
  ggplot(., aes(Continent, Total)) +
  geom_bar(stat = "identity") +
  geom_text(data = total, aes(label = paste("Total: ", 
                                            round(Total, digits = 2), 
                                            " Mha",
                                            sep = ""), 
                              group = Dataset), 
            x = 4, 
            y = 160, 
            inherit.aes = FALSE, 
            size = 3) +
  scale_y_continuous(breaks = pretty_breaks(n = 2)) +
  coord_flip() +
  labs(x = "Continent", 
       y = "Irrigated area (Mha)") +
  facet_wrap(~Dataset) +
  theme_bw() +
  theme(aspect.ratio = 1,
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


## ----plot_country_level, dev="tikz", cache=TRUE, message=FALSE, warning=FALSE------------

# DIFFERENCES IN THE MEASUREMENT OF IRRIGATED AREAS
# AS A FUNCTION OF DATASET (COUNTRY) ---------------------------------------------

temp <- df$meier %>%
  gather(Dataset, Value, 4:ncol(.)) %>%
  filter(!c(Value == 0 |
              Continent == "Oceania")) %>%
  mutate(Country = fct_recode(Country, 
                              "Congo" = "Democratic Republic of the Congo", 
                              "Tanzania" = "United Republic of Tanzania", 
                              "Iran" = "Iran (Islamic Republic of)", 
                              "Korea (DPR)" = "Korea, Democratic People's Republic of", 
                              "Lao" = "Lao People's Democratic Republic", 
                              "Macedonia" = "The former Yugoslav Republic of Macedonia")) %>%
  mutate_at(vars(Value), funs(. / 10^6)) %>%
  droplevels() %>%
  split(., .$Continent) 

gg <- list()
for(i in seq(temp)) {
  gg[[i]] <- ggplot(temp[[i]], aes(reorder(Country, Value), Value)) +
    geom_point(stat = "identity", aes(color = Dataset)) +
    scale_y_log10(  breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
    coord_flip() +
    scale_color_manual(name = "Dataset", 
                       labels = c("Aquastat", "FAOSTAT", "Thenkabail et al. 2009", 
                                  "Siebert et al. 2013", "Salmon et al. 2015", 
                                  "Meier et al. 2018"), 
                       values = c("yellowgreen", "seagreen4", "magenta3", 
                                  "sienna3", "turquoise2", "khaki3")) +
    labs(y = "Irrigated area (Mha)", 
         x = "") +
    facet_wrap(~Continent, 
               scales = "free_y") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.position = "none", 
          plot.margin = margin(t = -4.5,
                               unit = "cm")) 
}

# Extract legend
legend <- get_legend(gg[[1]] + theme(legend.position = "top"))


## ----plot_country_level1, dev="tikz", cache=TRUE, fig.height=9, fig.width=6, message=FALSE, warning=FALSE, fig.cap="Extension of irrigation at the country level. The data was retrieved from @Meier2018."----

# PLOT FOR AFRICA AND THE AMERICAS --------------------------------------

first <- plot_grid(gg[[1]], gg[[2]], ncol = 2)
plot_grid(legend, first, ncol = 1)


## ----plot_country_level2, dev="tikz", cache=TRUE, fig.height=9, fig.width=6, message=FALSE, warning=FALSE, fig.cap="Extension of irrigation at the country level. The data was retrieved from @Meier2018."----

# PLOT FOR ASIA AND EUROPE ----------------------------------------------

second <- plot_grid(gg[[3]], gg[[4]], ncol = 2)
plot_grid(legend, second, ncol = 1)


## ----out, fig.keep="none", cache=TRUE, dependson="datasets"------------------------------

# CHECK WHETHER THERE ARE OUTLIERS IN AREA.IRRIGATED VS. POPULATION 
# AND AREA.IRRIGATED VS WATER REQUIRED FOR IRRIGATION -------------------------

# Create datasets
temp <- dt_water[variable == "Water.requirement"] %>%
  .[df.meier, on = c("Continent", "Country" ,"Dataset", "Codes", "Area.irrigated")] %>%
  mutate_at(vars(Area.irrigated, Population, Value), funs(log10)) %>%
  data.table()

# Calculate Mahalabobis distances (robust and classic) for each
# continent and dataset

cols <- c("Population", "Area.irrigated")

temp1 <- temp[, dd.plot(.SD), .SDcols = cols, by = .(Continent, Dataset)] %>%
  .[, Class:= "Population"]

temp2 <- temp %>%
  na.omit() %>%
  .[, dd.plot(.SD), .SDcols = c("Value", "Area.irrigated"), 
                  by = .(Continent, Dataset)] %>%
  .[, Class:= "Water"]

out <- rbind(temp1, temp2)

# Extract number of outliers per continent and dataset
out.n <- out[, .(Outliers = sum(outliers == TRUE)), 
             by = .(Continent, Dataset, Class)] %>%
  .[order(Continent)]

# Extract maximum values for Mahalanobis distances
# (robust and classic)
out.md <- out[, .(max.Mahalanobis.classic = max(md.cla), 
                  max.Mahalanobis.robust = max(md.rob)), 
              by = .(Dataset, Continent, Class)]

# Merge both datasets
out.df <- out.n[out.md, on = c("Dataset", "Continent", "Class")] %>%
  .[order(Continent)]


## ----outlier_export, dependson="out", cache=TRUE-----------------------------------------

# EXPORT OUTLIERS DATASET ------------------------------------------------

fwrite(out.df, "out.df.csv")


## ----out_plot, dev="tikz", dependson="out", fig.width=10, fig.height=9, cache=TRUE, fig.cap="Scatter plot of Mahalanobis vs Robust distances."----

# ARRANGE TO PLOT RESULTS -----------------------------------------------------

temp <- out %>%
  split(., .$Class)

gg <- list()
for(i in names(temp)) {
  gg[[i]] <-   ggplot(temp[[i]], aes(md.cla, md.rob, 
                                     color = outliers)) +
    geom_point() +
    scale_colour_manual(name = "Outlier", 
                        values = setNames(c("black", "red"), 
                                          c(FALSE, TRUE))) +
    facet_grid(Dataset~Continent) +
    labs(x = "Mahalanobis distance", 
         y = "Robust distance") +
    theme_AP() +
    theme(legend.position = "top")
}


## ----plot_out_population, cache=TRUE, dev="tikz", dependson="out_plot", fig.width=10, fig.height=9, cache=TRUE, fig.cap="Scatter plot of Mahalanobis vs Robust distances."----

# PLOT MAHALANOBIS DISTANCES FOR AREA IRRIGATED VERSUS POPULATION -------------

plot(gg[["Population"]])


## ----plot_out_water, cache=TRUE, dev="tikz", dependson="out_plot", fig.width=10, fig.height=9, cache=TRUE, fig.cap="Scatter plot of Mahalanobis vs Robust distances."----

# PLOT MAHALANOBIS DISTANCES FOR AREA IRRIGATED VERSUS 
# IRRIGATION WATER REQUIREMENT ------------------------------------------------

plot(gg[["Water"]])


## ----irrigated_area_baseline, dependson="datasets", cache=TRUE---------------------------

# CALCULATE FOR EACH CONTINENT THE MAXIMUM AND MINIMUM
# EXTENSION OF IRRIGATED AREAS -------------------------------------------

total.area.irrigated <- df$meier %>%
  data.table() %>%
  melt(., measure.vars = c(4:9), 
       variable.name = "Dataset", 
       value.name = "Value") %>%
  .[, Value:= Value / 10 ^6] %>%
  .[, .(Total = sum(Value, na.rm = T)), .(Dataset, Continent)] %>%
  .[, .(min = min(Total), max = max(Total)), Continent] %>%
  .[!Continent == "Oceania"] %>%
  split(., .$Continent, drop = TRUE)

df$meier  %>%
  data.table() %>%
  melt(., measure.vars = c(4:9), 
       variable.name = "Dataset", 
       value.name = "Value") %>%
  .[, Value:= Value / 10 ^6] %>%
  .[, .(Total = sum(Value, na.rm = T)), .(Dataset, Continent)] %>%
  .[, .(min = min(Total), max = max(Total)), Continent]

## ----define_bootstrap_replicas, cache=TRUE-----------------------------------------------

# DEFINE NUMBER OF BOOTSTRAP REPLICAS -------------------------------------------

R <- 10^4


## ----ols_regressions, dependson=c("datasets", "compute_water_requirements", "define_bootstrap_replicas"), cache=TRUE----

# OLS REGRESSIONS: ALPHA, BETA AND DELTA ----------------------------------------

# Create bootstrap function for non-robust OLS
boot.ols <- function(formula, x, i) {
  d <- x[i, ]
  # Bootstrap slope
  fit <-lm(formula, data = d)
  out <-coef(fit)
  return(out)
}
# t1: alpha ols nonrob
# t2: beta ols nonrob

# Bootstrap alpha and beta
dt.regressions <- dt.full %>%
  .[, -2] %>%
  mutate_at(vars(Population, Area.irrigated, Water), funs(log10)) %>%
  data.table()

# Regular/Robust regressions
olsB <- dt.regressions[, # Regular OLS regressions
                      .(pop = list(boot(.SD, 
                                        statistic = boot.ols, 
                                        R = R,
                                        formula = Area.irrigated ~ Population, 
                                        parallel = "multicore", 
                                        ncpus = floor(detectCores() * 0.75))), 
                        water = list(boot(.SD, 
                                          statistic = boot.ols, 
                                          R = R,
                                          formula = Water ~ Area.irrigated, 
                                          parallel = "multicore", 
                                          ncpus = floor(detectCores() * 0.75))), 
                        # Robust OLS regressions
                        popR = list(lmrob(Area.irrigated ~ Population)), 
                        waterR = list(lmrob(Water ~ Area.irrigated))),
                      by = .(Continent, Dataset)]

# Extract alpha and beta for OLS non robust (population)
ols.nonrobust.pop <- olsB[, "All":= list(lapply(.SD, function(x) 
  map(x, "t"))), .SDcols = "pop", .(Continent, Dataset)] %>%
  .[, .("Alpha" = lapply(All, function(x) lapply(x, function(y) y[, 1])), 
       "Beta" = lapply(All, function(x) lapply(x, function(y) y[, 2]))), 
   .(Continent, Dataset)] %>%
  .[, lapply(.SD, unlist), .SDcols = c("Alpha", "Beta"), .(Continent, Dataset)] %>%
  .[, Regression:= "OLS"] %>%
  .[, Robust:= "NO"]

# Extract phi and delta for OLS non robust (water)
ols.nonrobust.water <- olsB[, "All":= list(lapply(.SD, function(x) 
  map(x, "t"))), .SDcols = "water", .(Continent, Dataset)] %>%
  .[, .("Phi" = lapply(All, function(x) lapply(x, function(y) y[, 1])), 
        "Delta" = lapply(All, function(x) lapply(x, function(y) y[, 2]))), 
    .(Continent, Dataset)] %>%
  .[, lapply(.SD, unlist), .SDcols = c("Phi", "Delta"), .(Continent, Dataset)] %>%
  .[, Regression:= "OLS"] %>%
  .[, Robust:= "NO"]


## ----ols_regressions_robust, dependson=c("datasets", "compute_water_requirements", "define_bootstrap_replicas"), cache=TRUE----

# OLS REGRESSIONS ROBUST: ALPHA, BETA AND DELTA --------------------------------

# Create cluster of 4 CPUS to speed up the bootstrapping

# Bootstrap OLS robust
olsR <- olsB[, .(popRob = lapply(popR, function(x) bootcoefs(x, 
                                                             R = R, 
                                                             method = "frb", 
                                                             ncpus = floor(detectCores() * 0.75))), 
                 waterRob = lapply(waterR, function(x) bootcoefs(x, 
                                                                 R = R, 
                                                                 method = "frb", 
                                                                 ncpus = floor(detectCores() * 0.75)))), 
             .(Continent, Dataset)]

# Extract alpha and beta for OLS robust (population) 
ols.robust.pop <- olsR[, "Allpop":= list(lapply(.SD, function(x) 
  lapply(x, function(y) y[["bootres"]]))), 
           .SDcols = "popRob", .(Continent, Dataset)] %>%
  .[, "temp":= list(lapply(.SD, function(x) 
    lapply(x, function(y) lapply(y, function(z) z[["t"]])))), 
    .SDcols = "Allpop", .(Continent, Dataset)] %>%
  .[, .("Alpha" = lapply(temp, function(x) 
    lapply(x, function(y) lapply(y, function(z) z[, 1]))),
    "Beta" = lapply(temp, function(x) 
      lapply(x, function(y) lapply(y, function(z) z[, 2])))) , 
    .(Continent, Dataset)] %>%
  .[, lapply(.SD, unlist), .SDcols = c("Alpha", "Beta"), .(Continent, Dataset)] %>%
  .[, Regression:= "OLS"] %>%
  .[, Robust:= "YES"]

# Extract phi and delta for OLS robust (water) 
ols.robust.water <- olsR[, "Allwater":= list(lapply(.SD, function(x) 
  lapply(x, function(y) y[["bootres"]]))), 
  .SDcols = "waterRob", .(Continent, Dataset)] %>%
  .[, "temp":= list(lapply(.SD, function(x) 
    lapply(x, function(y) lapply(y, function(z) z[["t"]])))), 
    .SDcols = "Allwater", .(Continent, Dataset)] %>%
  .[, .("Phi" = lapply(temp, function(x) 
    lapply(x, function(y) lapply(y, function(z) z[, 1]))),
    "Delta" = lapply(temp, function(x) 
      lapply(x, function(y) lapply(y, function(z) z[, 2])))) , 
    .(Continent, Dataset)] %>%
  .[, lapply(.SD, unlist), .SDcols = c("Phi", "Delta"), .(Continent, Dataset)] %>%
  .[, Regression:= "OLS"] %>%
  .[, Robust:= "YES"]


## ----sma_regressions, dependson=c("datasets", "compute_water_requirements", "define_bootstrap_replicas"), cache=TRUE----

# SMA REGRESSIONS: ALPHA AND BETA -----------------------------------------------

# Create bootstrap function for robust and non-robust SMA
boot.sma <- function(formula, x, i) {
  d <- x[i, ]
  # Bootstrap coefficients (non-robust)
  fit1 <- sma(formula, data = d, method = "SMA")
  # Bootstrap coefficients (robust)
  fit2 <- sma(formula, data = d, method = "SMA", robust = TRUE)
  coef1 <- coef(fit1)
  coef2 <- coef(fit2)
  all <- c(coef1, coef2)
  return(all)
}
# t1: alpha sma nonrob
# t2: beta sma nonrob
# t3: alpha sma rob
# t4: beta sma rob

# Bootstrap alpha and beta
smaB <- dt.regressions[, list(list(boot(.SD, 
                                        statistic = boot.sma, 
                                        R = R, 
                                        formula = Area.irrigated ~ Population, 
                                        parallel = "multicore", 
                                        ncpus = floor(detectCores() * 0.75)))), 
                       .(Continent, Dataset)]


## ----extract_alpha_beta_robust, cache=TRUE, dependson="sma_regression"-------------------

# EXTRACT ALPHA AND BETA ROBUST SMA ---------------------------------------------

# Extract alpha and beta SMA (non-robust)
sma.nonrobust.pop <- smaB[, "All":= list(lapply(V1, function(x) x["t"]))] %>%
  .[, list("Alpha" = lapply(All, function(x) lapply(x, function(y) y[, 1])),
           "Beta" = lapply(All, function(x) lapply(x, function(y) y[, 2]))),
      .(Continent, Dataset)] %>%
  .[, lapply(.SD, unlist), .SDcols = c("Alpha", "Beta"), .(Continent, Dataset)] %>%
  .[, Regression:= "SMA"] %>%
  .[, Robust:= "NO"]

# Extract alpha and beta SMA (robust)
sma.robust.pop <- smaB[, "All":= list(lapply(V1, function(x) x["t"]))] %>%
  .[, list("Alpha" = lapply(All, function(x) lapply(x, function(y) y[, 3])),
           "Beta" = lapply(All, function(x) lapply(x, function(y) y[, 4]))),
      .(Continent, Dataset)] %>%
  .[, lapply(.SD, unlist), .SDcols = c("Alpha", "Beta"), .(Continent, Dataset)] %>%
  .[, Regression:= "SMA"] %>%
  .[, Robust:= "YES"]


## ----export_sma, cache=TRUE, dependson="extract_alpha_beta_robust"-----------------------

# EXPORT BOOTSTRAP REPLICAS ---------------------------------------------------

fwrite(sma.nonrobust.pop, "sma.nonrobust.pop.csv")
fwrite(sma.robust.pop, "sma.robust.pop.csv")


## ----dt_bootstrap_samples, cache=TRUE, dependson=c("sma_regressions", "ols_regressions_robust", "ols_regressions", "extract_alpha_beta_robust")----

# CREATE FINAL DATA SETS WITH ALL BOOTSTRAP SAMPLES ----------------------------

# For beta and alpha
boot.samples.pop <- rbind(ols.nonrobust.pop, 
                          ols.robust.pop, 
                          sma.nonrobust.pop, 
                          sma.robust.pop) %>%
  .[order(Continent, Dataset)] 

# For delta
boot.samples.water <- rbind(ols.nonrobust.water, 
                            ols.robust.water) %>%
  .[order(Continent, Dataset)]


## ----export_bootstrap_samples2, cache=TRUE, dependson="dt_bootstrap_samples"-------------

# EXPORT BOOTSTRAP SAMPLES ------------------------------------------------------

fwrite(boot.samples.pop, "boot.samples.pop.csv")
fwrite(boot.samples.water, "boot.samples.water.csv")

## ----lookup_tables, cache=TRUE, dependson="dt_bootstrap_samples"-------------------------

# CREATE THE LOOKUP TABLE ---------------------------------------------------

# Create vector to change columns
col_names <- c("Continent", "Dataset", "Regression", "Robust")
col_names2 <- col_names[!col_names %in% "Regression"]

# Create lookup table: population
lookup.pop <- boot.samples.pop[order(Beta), .SD, col_names] %>%
  .[, ID:= 1:.N, col_names] %>%
  .[, index:= paste(Continent, Dataset, Regression, Robust, ID, sep = "_")]

lookup.pop <- setkey(lookup.pop, index)

# Create lookup table: water
lookup.water <- boot.samples.water[order(Delta), .SD, col_names2] %>%
  .[, ID:= 1:.N, col_names2] %>%
  .[, index:= paste(Continent, Dataset, Robust, ID, sep = "_")]

lookup.water <- setkey(lookup.water, index)

## ----export_bootstrap_samples, cache=TRUE, dependson=c("dt_bootstrap_samples", "lookup_tables")----

# EXPORT BOOTSTRAP SAMPLES TO CSV ------------------------------------------

fwrite(lookup.pop, "lookup.pop.csv")
fwrite(lookup.water, "lookup.water.csv")


## ----population_baseline, cache=TRUE, dependson="datasets"-------------------------------

# CREATE DATA FRAME WITH POPULATION BASELINE VALUES ------------------------------

# Prepare population values between 1999-2015
temp <- df$population %>%
  # Filter out Oceania
  filter(!Continent == "Oceania") %>%
  select(Continent, Codes, Estimate, Year.1999:Year.2012) %>%
  gather(Year, Population, Year.1999:Year.2012) %>%
  mutate(Continent = fct_recode(Continent, 
                                "Americas" = "S.America", 
                                "Americas" = "N.America")) %>%
  group_by(Continent, Year, Estimate) %>%
  summarise(Population = sum(Population)) %>%
  # Multiply by 1000 to get original population values
  mutate_at(vars(Population), funs(.*10^3)) %>%
  separate(., Year, 
           into = c("dummy", "Year")) %>%
  # Drop dummy columns
  mutate_at(vars(Year), funs(as.numeric)) %>%
  mutate(t = 2050 - Year) %>%
  rename(N = Population) %>%
  select(Continent, t, N)

# Create population lookup dataset
population <- setDT(temp)[order(Continent)] %>%
  .[, index:= paste(Continent, t, sep = "_")] %>%
  # To get million population
  .[, N:= N / 10 ^6] %>%
  .[, .(N, index)]

population <- setkey(population, index)
fwrite(population, "population.csv")


## ----population_growth, cache=TRUE, dependson="datasets", dev="tikz", fig.height=6, fig.width=5----

# DEFINE POPULATION GROWTH RATES DISTRIBUTIONS ----------------------------------

# Prepare data frame with growth rates 2015-2050
df.rate1 <- df$growth.rate.estimate %>%
  select(Continent, Estimate, Year.2015.2020:Year.2045.2050) %>%
  gather(Period, Value, Year.2015.2020:Year.2045.2050) %>%
  # Unite population growth rates for N.America and S.America, 
  # and consider both regions as one (Americas)
  mutate(Continent = fct_recode(Continent, 
                                "Americas" = "S.America", 
                                "Americas" = "N.America")) %>%
  # Exclude Oceania
  filter(!Continent == "Oceania")

# Prepare data frame with growth rates 2000-2015
df.rate2 <- df$growth.rate %>%
  select(Continent, Year.2000.2005:Year.2010.2015) %>%
  gather(Period, Value, Year.2000.2005:Year.2010.2015) %>%
  mutate(Continent = fct_recode(Continent, 
                                "Americas" = "S.America", 
                                "Americas" = "N.America")) %>%
  # Exclude Oceania
  filter(!Continent == "Oceania") 

# Create population growth rates data frame
df.rate <- bind_rows(df.rate1, df.rate2) %>%
  # Add constant (+5) to growth rate values to 
  # allow fitting distributions later on
  mutate_at(vars(Value), funs(. + 5)) %>%
  split(., .$Continent, drop = TRUE)


# Describe growth rates of continents with a distribution

# Fit possible distributions according to histograms
distr.norm <- lapply(df.rate, function(x) fitdist(x$Value, "norm"))
distr.logis <- lapply(df.rate, function(x) fitdist(x$Value, "logis", 
                                                   method = "mme")) 
distr.weib <- lapply(df.rate, function(x) fitdist(x$Value, "weibull")) 

# Define function to plot
plotDistr <- function(x,...) {
  funs <- c(denscomp, qqcomp, cdfcomp, ppcomp)
  lapply(funs, function(f) f(list(x,...), 
                             legendtext = plot.legend))
}

# Plot distributions and fits (Figures 9-12)
par(mfrow = c(2, 2), 
    oma = c(0, 0, 2, 0))

plot.legend <- c("Normal", "Logistic", "Weibull")

for(i in names(distr.norm)) {
  gg <- plotDistr(distr.norm[[i]], 
                  distr.logis[[i]],
                  distr.weib[[i]])
  title(names(distr.norm[i]), outer = TRUE)
  print(gg)
}

# Create function to assess whether a normal, a logistic or a 
# weibull distribution better fits the data
bic.aic <- function(x, y) {
  df <- x %>%
    map(y) %>%
    data.frame() %>%
    t() %>%
    data.frame() %>%
    rownames_to_column(., var = "Continent") 
  return(df)
}

# Assess via BIC
bic.aic(x = distr.norm, 
        y = "bic") %>%
  rename(Normal = ".") %>%
  inner_join(., bic.aic(x = distr.logis, 
                        y = "bic"), 
             by = "Continent") %>%
  rename(Logistic = ".") %>%
  inner_join(., bic.aic(x = distr.weib, 
                        y = "bic"), 
             by = "Continent") %>%
  rename(Weibull = ".") %>%
  mutate(Model = "BIC")

# The distributions that better fit the data are the following:
# Africa: Weibull distribution
# Asia: Weibull distribution
# Europe: Weibull distribution
# Americas: Normal distribution

# Create data frame with original growth rate values
df.rate.nrm <- bind_rows(df.rate1, df.rate2) %>%
  # Divide growth rates per 100 because it is in percentage
  mutate_at(vars(Value), funs(. / 100)) %>%
  split(., .$Continent, drop = TRUE)


# Fit a normal distribution as we will need the fit for the Americas
distr.norm2 <- lapply(df.rate.nrm, function(x) fitdist(x$Value, "norm"))

# List with the parameters for the distribution of growth rates
growth.rate.distr <- list(distr.weib$Africa$estimate, 
                          distr.weib$Asia$estimate, 
                          distr.weib$Europe$estimate, 
                          distr.norm2$Americas$estimate)

# Name the slots of the list
names(growth.rate.distr) <- c("Africa", "Asia", "Europe", "Americas")   


## ----cropland_available, cache=TRUE, dependson="datasets"--------------------------------

# INTEGRATE CROPLAND AVAILABLE WITH MODEL OUTPUT UNCERTAINTY --------------------

# Prepare dataset by Zhang: create continental frame
df.zhang <- df$zhang.land.available %>%
  # Transform km2 to ha
  mutate_at(vars(Mkm2, Baseline), funs(.*100)) %>%
  group_by(Continent, Estimation, Baseline) %>%
  summarise(Min = min(Mkm2), 
            Max = max(Mkm2)) %>%
  # Conditional mutation: create column filled with
  # either the minimum value (if both projections decrease)
  # or the maximum value (if both projections increase)
  mutate(Value = ifelse(Max < Baseline, Min, Max)) %>%
  data.frame() %>%
  filter(!Continent == "World") %>%
  mutate(Continent = fct_recode(Continent, 
                                "Asia" = "China", 
                                "Asia" = "India", 
                                "Americas" = "US", 
                                "Americas" = "S.America", 
                                "Europe" = "Russia")) 

# Prepare dataset by Zhang: calculate min and max values
temp <- df.zhang %>%
  select(Continent, Estimation, Min, Max) %>%
  split(.,list(.$Continent, .$Estimation), 
        drop = TRUE) %>%
  lapply(., function(x)  {
    x$min <- sum(x$Min)
    x$max <- sum(x$Max)
    return(x[1, 5:6])
  })

# Create final data set by Zhang to plot
cropland.1 <- temp %>%
  map(data.frame) %>%
  rbindlist(., idcol = "Continent") %>%
  separate(., col = Continent, 
           into = c("Continent", "Estimation"))

cropland <- cropland.1 %>%
  gather(Parameter, Value, min:max) %>%
  group_by(Continent) %>%
  summarise(min = min(Value), 
            max = max(Value)) %>%
  split(., .$Continent) 


## ----water_available, cache=TRUE---------------------------------------------------------

# DEFINE DISTRIBUTIONS FOR THE TOTAL WATER AVAILABLE ----------------------------

# Read in dataset (# 10^9 m3/yr (It is already in km3)
water.availability <- fread("aquastat_water.csv", 
                            nrows = 182)[, .(Country, Value)]

# Get the codes and the continents
addAll(water.availability, "Aquastat")

# Compute 20% uncertainty
water.availability.dt <- water.availability[!Continent == "Oceania"] %>%
  .[, sum(Value, na.rm = TRUE), Continent] %>%
  .[, uncertainty:= round(V1 * 0.20, digits = 0)] %>%
  .[, lower:= round(V1 - uncertainty, digits = 0)] %>%
  .[, upper:= round(V1 + uncertainty, digits = 0)] %>%
  split(., .$Continent)


## ----settings_sample_matrix, cache=TRUE, results="hide", message=FALSE-------------------

# CREATE THE SAMPLE MATRIX --------------------------------------------------------

# Create a vector with the name of the columns
parameters <- c("X1", "X2", "X3", "X4", "W1", "W3", "W4", "r",
                "gamma", "Y0", "t", "K", "W_a", "eta")

# Obtain number of parameters
k <- length(parameters)

# Select sample size
n <- 2 ^ 10

# Create vector with the continents
Continents <- c("Africa", "Americas", "Asia", "Europe")

# Create an A, B and AB matrices for each continent
AB <- lapply(Continents, function(Continents) 
  sobol_matrices(n = n, k = k) %>%
               data.table()) 

# Name the slots, each is a continent
names(AB) <- Continents

# Name the columns
AB <- lapply(AB, setnames, parameters)


## ----sample_matrix_cluster, cache=TRUE, dependson="settings_sample_matrix", results="hide", message=FALSE----

# CREATE THE SAMPLE MATRICES FOR THE CLUSTERED PARAMETERS -----------------------

# Create vectors with the name of the parameters within each cluster
irrigation <- match(c("X1", "Y0", "W1", "W_a", "eta"), parameters)
population2 <- match(c("r", "gamma"), parameters)
model <- match(c("X2", "X3", "X4", "W3", "W4"), parameters)


# Create an A, B and AB matrices for the clustered parameters; retrieve
# only the AB

AB.cluster <- lapply(Continents, function(Continents) 
  sobol_matrices(n = n, 
                 k = k, 
                 cluster = list(irrigation, population2, model))) %>%
  lapply(., function(x) x[((2*n) + 1):nrow(x), ]) %>%
  lapply(., data.table)

# Name the slots, each is a continent
names(AB.cluster) <- Continents

# Name the columns
AB.cluster <- lapply(AB.cluster, setnames, parameters)

# Merge the sample matrix and the sample matrix of the
# clustered parameters

for(i in names(AB)) {
  AB[[i]] <- rbind(AB[[i]], AB.cluster[[i]])
}


## ----number_boot_replicas, cache=TRUE, dependson="dt_bootstrap_samples"------------------

# CHECK NUMBER OF BOOTSTRAP SAMPLES OF BETA, DELTA, ETC. -------------------------

N.boot <- boot.samples.pop[, .(N = .N), 
                           .(Continent, Dataset, Regression, Robust)] %>%
  .[, N] %>%
  .[1]

print(N.boot)


## ----transform_sample_matrix, cache=TRUE, dependson=c("sample_matrix_cluster", "water_available", "cropland_available", "population_growth", "population_baseline", "lookup_tables", "settings_sample_matrix")----

# TRANSFORM THE SAMPLE MATRIX ----------------------------------------------------

# Create function to transform the parameters that 
# have the same distribution in all continents
transform.sobol <- function(X) {
  X[, X1:= floor(X1 * (6-1+1)) + 1][, X1:= ifelse(X1 == 1, "Aquastat", 
                                                  ifelse(X1 == 2, "FAOSTAT", 
                                                         ifelse(X1 == 3, "Siebert.et.al.2013", 
                                                                ifelse(X1 == 4, "Meier.et.al.2018", 
                                                                       ifelse(X1 == 5, "Salmon.et.al.2015", 
                                                                              "Thenkabail.et.al.2009")))))]
  X[, X2:= floor(X2 * (2-1+1)) + 1][, X2:= ifelse(X2==1, "OLS", "SMA")]
  X[, X3:= floor(X3 * (2-1+1)) + 1][, X3:= ifelse(X3==1, "YES", "NO")]
  X[, X4:= floor(X4 * (N.boot - 1)) + 1]
  X[, W1:= floor(W1 * (6-1+1)) + 1][, W1:= ifelse(W1 == 1, "Aquastat", 
                                                  ifelse(W1 == 2, "FAOSTAT", 
                                                         ifelse(W1 == 3, "Siebert.et.al.2013", 
                                                                ifelse(W1 == 4, "Meier.et.al.2018", 
                                                                       ifelse(W1 == 5, "Salmon.et.al.2015", 
                                                                              "Thenkabail.et.al.2009")))))]
  X[, W3:= floor(W3 * (2-1+1)) + 1][, W3:= ifelse(W3==1, "YES", "NO")]
  X[, W4:= floor(W4 * (N.boot - 1)) + 1]
  X[, gamma:= 0.02 * qnorm(gamma) + 1]
  X[, t:= floor(t * (51-38 + 1)) + 38]
  X[, eta:= qunif(eta, min = 0.2, max = 0.5)]
  return(X)
}

AB <- lapply(AB, transform.sobol)

# Transform the parameters with their appropriate distributions
transform.sobol.continents <- function(AB) {
  for(i in names(AB)) {
    if(i == "Africa") {
      # Weibull distribution, substract the constant and divide by 100
      # because original values were in percentage
      AB[[i]][, r:= (growth.rate.distr$Africa[[2]] * 
                       (-log(1 - r)) ^ (1/growth.rate.distr$Africa[[1]]) 
                     -5) / 100]
      # Unifrom distribution
      AB[[i]][, K:= K * (cropland$Africa$max-cropland$Africa$min) +
                cropland$Africa$min]
      # Uniform distribution
      AB[[i]][, W_a:= qunif(W_a, min = water.availability.dt$Africa$lower, 
                            max = water.availability.dt$Africa$upper)]
      AB[[i]][, Y0:= Y0 *
                (total.area.irrigated$Africa$max-total.area.irrigated$Africa$min) +
                total.area.irrigated$Africa$min]
    }
    if(i == "Asia") {
      # Weibull distribution, substract the constant and divide by 100
      # because original values were in percentage
      AB[[i]][, r:= (growth.rate.distr$Asia[[2]] * 
                       (-log(1 - r)) ^ (1/growth.rate.distr$Asia[[1]])
                     -5) / 100]
      # Unifrom distribution
      AB[[i]][, K:= K * (cropland$Asia$max-cropland$Asia$min) +
                cropland$Asia$min]
      # Uniform distribution
      AB[[i]][, W_a:= qunif(W_a, min = water.availability.dt$Asia$lower, 
                            max = water.availability.dt$Asia$upper)]
      AB[[i]][, Y0:= Y0 *
                (total.area.irrigated$Asia$max-total.area.irrigated$Asia$min) +
                total.area.irrigated$Asia$min]
    }
    if(i == "Americas") {
      # Normal distribution
      AB[[i]][, r:= (growth.rate.distr$Americas[[2]] * 
                       qnorm(r) + growth.rate.distr$Americas[[1]])]
      # Unifrom distribution
      AB[[i]][, K:= K * (cropland$Americas$max-cropland$Americas$min) +
                cropland$Americas$min]
      # Uniform distribution
      AB[[i]][, W_a:= qunif(W_a, min = water.availability.dt$Americas$lower, 
                            max = water.availability.dt$Americas$upper)]
      AB[[i]][, Y0:= Y0 *
                (total.area.irrigated$Americas$max-total.area.irrigated$Americas$min) +
                total.area.irrigated$Americas$min]
    }
    if(i == "Europe") {
      # Weibull distribution, substract the constant and divide by 100
      # because original values were in percentage
      AB[[i]][, r:=(growth.rate.distr$Europe[[2]] * 
                      (-log(1 - r)) ^ (1/growth.rate.distr$Europe[[1]])
                    -5) / 100]
      # Unifrom distribution
      AB[[i]][, K:= K * (cropland$Europe$max-cropland$Europe$min) +
                cropland$Europe$min]
      # Uniform distribution
      AB[[i]][, W_a:= qunif(W_a, min = water.availability.dt$Europe$lower, 
                            max = water.availability.dt$Europe$upper)]
      AB[[i]][, Y0:= Y0 * (total.area.irrigated$Europe$max-total.area.irrigated$Europe$min) +
        total.area.irrigated$Europe$min]
    }
  }
  return(AB)
}

AB <- transform.sobol.continents(AB)


## ----final.dt, cache=TRUE, dependson=c("sample_matrix_cluster", "water_available", "cropland_available", "population_growth", "population_baseline", "lookup_tables", "settings_sample_matrix", "transform_sample_matrix")----

# WRITE FINAL DATA TABLE -------------------------------------------------------

final.dt <- rbindlist(AB, idcol = "Continent")


## ----export.final.dt, cache=TRUE, dependson=c("sample_matrix_cluster", "water_available", "cropland_available", "population_growth", "population_baseline", "transform_sample_matrix", "lookup_tables", "settings_sample_matrix", "transform_outliers")----

# EXPORT FINAL DATA TABLE --------------------------------------------------------

fwrite(final.dt, "final.dt.csv")
print(final.dt)

## ----model, cache=TRUE-------------------------------------------------------------------

# DEFINE THE MODEL ----------------------------------------------------------------

model <- function(X) {
  # Extract beta
  Beta <- lookup.pop[.(paste0(X[, 1:5], collapse = "_"))][, Beta]
  # Select population
  N <- population[.(paste0(X[, c("Continent", "t")], collapse = "_"))][, N]
  # Compute Alpha
  Alpha <- X[, Y0] / N ^ Beta
  # Extract phi and delta
  tmp <- lookup.water[.(paste0(X[, c(1, 6:8)], collapse = "_"))]
  Phi <- tmp[, Phi]
  Delta <- tmp[, Delta]
  # COMPUTE THE MODEL ---------------------------
  Y <- Alpha * 
  ((N ^ (1- X[, gamma]) + X[, r] * X[, t] * (1 - X[, gamma])) ^ 
     (Beta / (1 - X[, gamma])))
  # Compute how much water will we need to irrigate Y
  w <- (10 ^ Phi) * Y ^ Delta
  # Compute how much water we have 
  # available for irrigation --------------------
  w_i <- X[, W_a] * X[, eta]
  # Compute the total extension that can 
  # be irrigated with the water 
  # we have available for irrigation ------------
  Y.max <- (w_i / 10 ^ Phi) ^ (1 / Delta)
  # Constrain
  if(Y > X[, K]) {
    Y <- X[, K]
  }
  if(Y > Y.max) {
    Y <- Y.max
  }
  return(c(Beta, N, Phi, Delta, w, w_i, Y.max, Y))
}


## ----run_model, cache=TRUE, dependson=c("model", "export.final.dt")----------------------

# RUN MODEL USING PARALLEL COMPUTING --------------------------------------------

# Define parallel computing
cl <- makeCluster(floor(detectCores() * 0.75))
registerDoParallel(cl)

# Run model in parallel
Y <- foreach(i=1:nrow(final.dt), 
             .packages = c("dplyr", "data.table")) %dopar% 
             {
               model(final.dt[i])
             }

# Stop parallel cluster
stopCluster(cl)

## ----arrange_output, cache.lazy = FALSE, cache=TRUE, dependson=c("run_model", "final.dt", "transform_outliers", "export.final.dt")----

# ADD MODEL OUTPUT ---------------------------------------------------------------

model.output <- c("Beta", "N", "Phi", "Delta", "w", "w_i", "Y.max", "Y")

full.dt <- cbind(final.dt, data.table(do.call(rbind, Y))) %>%
  setnames(., paste("V", 1:length(model.output), sep = ""), model.output)

# Select the A and B matrix only (for uncertainty analysis)
AB.dt <- full.dt[, .SD[1:(n * 2)], Continent]


AB.dt <- fread("AB.dt.csv")

global.uncertainty[, .(mean = mean(Total), 
          sd = sd(Total), 
          min = min(Total), 
          max = max(Total))]

## ----export_full_model output, cache=TRUE, dependson="arrange_output"--------------------

# EXPORT MODEL OUTPUT ------------------------------------------------------------

fwrite(full.dt, "full.dt.csv")


## ----export_AB_matrix, cache=TRUE, dependson="arrange_output"----------------------------

# EXPORT AB MATRICES -------------------------------------------------------------

fwrite(AB.dt, "AB.dt.csv")

## ----quantiles, cache=TRUE, dependson="arrange_output"-----------------------------------

# COMPUTE QUANTILES --------------------------------------------------------------

# Check number and proportion of negative model output values
AB.dt[, .(negative.runs = sum(Y < 0), 
          proportion = sum(Y < 0) / .N), 
      Continent]

# Compute quantiles
quant <- AB.dt[Y > 0] %>%
  group_by(Continent) %>%
  do(data.frame(t(quantile(.$Y, 
                           probs = c(0.005, 0.01, 0.025, 0.975, 0.99, 0.995, 1), 
                           na.rm = TRUE)))) 

# Print the quantiles
print(quant)


## ----global_uncertainty, cache=TRUE, dependson="arrange_output"--------------------------

# COMPUTE QUANTILES AT THE GLOBAL LEVEL ------------------------------------------

projections <- df$projections %>%
  select(Study, `2050`, Group) %>%
  na.omit() %>%
  data.table()

projections <- rbind(projections, 
                     data.table(Study = "Rosegrant.et.al.2002", 
                                `2050` = 237, 
                                Group = 11)) 

# Assess uncertainty at the global level
global.uncertainty <- AB.dt %>%
  .[, .(Y, Continent)] %>%
  split(., .$Continent) %>%
  lapply(., function(x) x[, Y]) %>%
  do.call("cbind", .) %>%
  data.table() %>%
  .[, Total:= rowSums(.)]
x

# Calculate the 2.5 and the 97.5 quantiles
global.quantile <- quantile(global.uncertainty$Total,
                            probs = c(0.005, 0.01, 0.025, 0.975, 0.99, 0.995, 1),
                            na.rm = TRUE) %>%
  t() %>%
  data.frame()

# Print quantiles of the global uncertainty
print(global.quantile)


## ----read_projections--------------------------------------------------------------------

# READ PROJECTIONS OF IRRIGATED AREAS AT THE CONTINENTAL LEVEL --------------------

irrigated_area_2050 <- fread("irrigated_area_2050.csv")[, World:= NULL]
irrigated_area_2050_dt <- melt(irrigated_area_2050, 
                               measure.vars = c("Africa", "Americas", "Europe", "Asia"), 
                               variable.name = "Continent")

# Get minimum and maximum
prove <- irrigated_area_2050_dt[, .(min = min(value, na.rm = TRUE), 
                                    max = max(value, na.rm = TRUE)), 
                                Continent]

# Proportion of model runs covered by current projections (min and max)
sapply(Continents, function(x) AB.dt[Continent == x, 
                                     sum(Y >= prove[Continent == x, min] & 
                                           Y<= prove[Continent == x, max]) / .N])

# Proportion of model runs larger than the maximum value projected
sapply(Continents, function(x) AB.dt[Continent == x, 
                                     sum(Y >= prove[Continent == x, max]) / .N])


## ----plot_uncertainty_final, cache=TRUE, dependson=c("global_uncertainty", "quantiles", "arrange_output", "read_projections"), dev="tikz", fig.height=4.5, fig.width=6.5----

# PLOT UNCERTAINTY ---------------------------------------------------------------

# Continental level
a <- AB.dt %>%
  .[Y > 10^0] %>%
  ggplot(., aes(Y)) +
  geom_rect(data = quant,
            aes(xmin = X2.5.,
                xmax = X97.5.,
                ymin = -Inf,
                ymax = Inf,
                group = Continent),
            fill = "green",
            color = "white",
            alpha = 0.1,
            inherit.aes = FALSE) +
  geom_rect(data = cropland.1,
            aes(xmin = min,
                xmax = max,
                ymin = -Inf,
                ymax = Inf,
                group = Continent,
                fill = Estimation),
            color = "black",
            alpha = 0.7,
            inherit.aes = FALSE) +
  scale_fill_manual(guide = FALSE, 
                      values = c("chocolate4", "chocolate1")) +
  geom_histogram() +
  geom_vline(data = irrigated_area_2050_dt,
             aes(xintercept = value,
                 colour = Study),
             lty = 2,
             size = 1) +
  labs(x = "Area irrigated 2050 (Mha)",
       y = "Counts") +
  facet_wrap(~Continent,
             ncol = 1,
             scales = "free_y") +
  scale_x_log10() +
  scale_y_continuous(breaks = pretty_breaks(n = 2)) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent",
                                         color = NA),
        legend.key = element_rect(fill = "transparent",
                                  color = NA)) 

# Global level
# Global level
b <- global.uncertainty %>%
  ggplot(., aes(Total)) +
  geom_rect(data = global.quantile,
            aes(xmin = X2.5.,
                xmax = X97.5.,
                ymin = -Inf,
                ymax = Inf),
            fill = "green",
            color = "white",
            alpha = 0.1,
            inherit.aes = FALSE) +
  geom_rect(data = cropland.1[, .(maximum = sum(max), 
                                  minimum = sum(min)), Estimation],
            aes(xmin = minimum,
                xmax = maximum,
                ymin = -Inf,
                ymax = Inf,
                fill = Estimation),
            color = "black",
            alpha = 0.7,
            inherit.aes = FALSE) +
  scale_fill_manual(guide = FALSE, 
                      values = c("chocolate4", "chocolate1")) +
  geom_histogram() +
  geom_vline(data = projections[!Study == "Alcamo.et.al.2005"],
             aes(xintercept = `2050`,
                 colour = Study),
             lty = 2,
             size = 1) +
  scale_color_discrete(labels = c("Alcamo et al. 2005",
                                  "Alexandratos and \n Bruinsma 2012",
                                  "Fischer et al. 2007",
                                  "Molden 2007",
                                  "Rosegrant et al. 2002")) +
  labs(x = "Area irrigated 2050 (Mha)",
       y = "",
       color = "Projection") +
  scale_x_log10(# Limit the x axis for better visualization
                limits = c(200, 4300)) +
  theme_bw() +
  theme(legend.position = c(0.82, 0.58),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = alpha("white", 0.7)),
        legend.key = element_rect(fill = "transparent",
                                  color = NA))


## ----extra_plot, cache=TRUE, dependson=c("global_uncertainty", "quantiles", "arrange_output", "read_projections", "plot_uncertainty_final"), dev="tikz", fig.height=6.8, fig.width=4.7----

# PLOT ------------------------------------------------------------------------

plot_grid(a + labs(x = "", y = "Counts"), 
          b + labs(x = "Area irrigated 2050 (Mha)", y = "Counts"), 
          ncol = 1, 
          labels = "AUTO", 
          align = "hv", 
          rel_heights = c(1, 0.6))



## ----global_only, cache=TRUE, dependson=c("plot_uncertainty", "extra_plot"), dev="tikz", fig.height=2.5, fig.width=4.7----

# PLOT ------------------------------------------------------------------------

b + theme(plot.margin = margin(l = 0, unit = "cm"))


## ----export_global, cache=TRUE, dependson="global_uncertainty"---------------------------

# EXPORT GLOBAL UNCERTAINTY -----------------------------------------------------

fwrite(global.uncertainty, "global.uncertainty.csv")


## ----uncertainty_statistics, cache=TRUE, dependson=c("arrange_output", "global_uncertainty")----

# UNCERTAINTY STATISTICS --------------------------------------------------------

# How many model runs hit K?
AB.dt[, sum((Y == K) / .N) * 100, Continent]

# Compare the global projection to previous projections
global.uncertainty[, .(N = .N, 
                       larger.Alcamo.2007 = sum((Total > 262) / .N) * 100,
                       larger.FAO = sum((Total > 322) / .N) * 100,
                       larger.Molden = sum((Total > 450) / .N) * 100,
                       much.larger.Molden = sum((Total > 675) / .N) * 100,
                       twice.Molden = sum((Total > 900) / .N) * 100,
                       three.Molden = sum((Total > 450 * 3) / .N) * 100,
                       less.100 = sum((Total < 10^2) / .N) * 100)] 


## ----sensitivity_scatterplots, cache=TRUE, dependson="arrange_output"--------------------

# ARRANGE SCATTERPLOTS OF PARAMETERS VS MODEL OUTPUT ----------------------------

# Function to recode some parameters to allow plotting
code_columns <- function(x) {
  dt <- ifelse(x == "Aquastat", 1, 
               ifelse(x == "FAOSTAT", 2,
                      ifelse(x == "Siebert.et.al.2013", 3, 
                             ifelse(x == "Meier.et.al.2018", 4, 
                                    ifelse(x == "Salmon.et.al.2015", 5, 6)))))
  return(dt)
}

code_columns2 <- function(x) ifelse(x == "YES", 1, 2)

# Vector with renamed parameters for better plotting
parameters.renamed <- c("X1", "X2", "X3", "X4", "W1", "W3", "W4", "r", "$\\gamma$", 
                        "Y0", "t", "K", "Wa", "$\\eta$")

# Create temporary data table to plot
tmp <- cbind(AB.dt[, .(Continent)], AB.dt[, ..parameters], AB.dt[, .(Y)]) 

# Update columns and arrange
# Update columns to allow plotting of scatterplots
col_names <- c("X1", "W1")
col_names2 <- c("X3", "W3")
tmp <- tmp[, (col_names):= lapply(.SD, code_columns), .SDcols = col_names]
tmp <- tmp[, (col_names2):= lapply(.SD, code_columns2), .SDcols = col_names2]
tmp <- tmp[, X2:= ifelse(X2 == "OLS", 1, 2)]

tmp2 <- gather(tmp, Parameters, Values, X1:eta) %>%
  split(., .$Continent) 


## ----plot_scatterplots, cache=TRUE, dependson="sensitivity_scatterplots", fig.height=8.5, fig.width=4.7----

# PLOT SCATTERPLOTS OF PARAMETERS VS MODEL OUTPUT ------------------------------

gg <- list()
for(i in names(tmp2)) {
  gg[[i]] <- ggplot(tmp2[[i]], aes(Values, Y)) +
    geom_hex() +
    scale_x_continuous(breaks = pretty_breaks(n = 3)) +
    scale_fill_gradient(breaks = pretty_breaks(n = 2)) +
    scale_y_log10() +
    scale_alpha(guide = "none") + 
    labs(x = "Values",
         y = "Area irrigated 2050 (Mha)") +
    facet_wrap(~Parameters,
               scales = "free_x",
               ncol = 3, 
               labeller = label_parsed) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.background = element_rect(fill = "transparent", 
                                           color = NA), 
          legend.key = element_rect(fill = "transparent", 
                                    color = NA), 
          legend.position = "top") +
    ggtitle(names(tmp2[i]))
}

gg[[1]]
gg[[2]]
gg[[3]]
gg[[4]]


## ----sobol_functions, cache=TRUE, echo=FALSE---------------------------------------------

# FUNCTION TO COMPUTE SOBOL' FIRST AND TOTAL-ORDER EFFECTS USING
# THE JANSEN 1999 ESTIMATOR FOR FIRST AND TOTAL INDICES -------------------------

sobol_computeJ <- function(Y_A, Y_B, Y_AB) {
  # Compute sample mean of output
  f0 <- (1 / (2 * length(Y_A))) * sum(Y_A + Y_B)
  # Compute unconditional variance of output
  VY <- 1 / (2 * length(Y_A) - 1) * sum((Y_A - f0) ^ 2 + (Y_B - f0) ^ 2)
  # Compute first order indices (Jansen 1999)
  Si <- (VY - ((1 / (2 * length(Y_A))) * sum((Y_B - Y_AB) ^ 2))) / VY
  # Compute total order indices (Jansen 1999)
  STi <- ((1 / (2 * length(Y_A))) * sum((Y_A - Y_AB) ^ 2)) / VY
  return(c(Si, STi))
}

sobol_MapplyJ <- function(d, i) {
  data <- d[i, ]
  return(mapply(sobol_computeJ,
                data[, "Y_A"],
                data[, "Y_B"],
                data[, "Y_AB"]))
}

# FUNCTION TO COMPUTE SOBOL' SECOND-ORDER EFFECTS USING
# THE JANSEN 1999 ESTIMATOR ---------------------------------------------------

sobol_compute_secondJ <- function(Y_A, Y_B, Y_ABi,
                                  Y_ABj, Y_ABij) {
  # Compute sample mean of output
  f0 <- (1 / (2 * length(Y_A))) * sum(Y_A + Y_B)
  # Compute unconditional variance of output
  VY <- 1 / (2 * length(Y_A) - 1) * sum((Y_A - f0) ^ 2 + (Y_B - f0) ^ 2)
  # Use Jansen 1999 estimates
  Vi <- VY - ((1 / (2 * length(Y_A))) * sum((Y_B - Y_ABi) ^ 2))
  Vj <- VY - ((1 / (2 * length(Y_A))) * sum((Y_B - Y_ABj) ^ 2))
  Vij <- (VY - ((1 / (2 * length(Y_A))) * sum((Y_B - Y_ABij) ^ 2))) - Vi - Vj
  Sij <- Vij / VY
  return(Sij)
}

sobol_second_MapplyJ <- function(d, i) {
  data <- d[i, ]
  return(mapply(sobol_compute_secondJ,
                data[, "Y_A"],
                data[, "Y_B"],
                data[, "Y_ABi"],
                data[, "Y_ABj"],
                data[, "Y_ABij"]))
}

# FUNCTION TO COMPUTE SOBOL' THIRD-ORDER EFFECTS USING
# THE JANSEN 1999 ESTIMATOR ---------------------------------------------------

sobol_compute_thirdJ <- function(Y_A, Y_B, Y_ABi,
                                 Y_ABj, Y_ABk, Y_ABij,
                                 Y_ABik, Y_ABjk, Y_ABijk) {
  # Compute sample mean of output
  f0 <- (1 / (2 * length(Y_A))) * sum(Y_A + Y_B)
  # Compute unconditional variance of output
  VY <- 1 / (2 * length(Y_A) - 1) * sum((Y_A - f0) ^ 2 + (Y_B - f0) ^ 2)
  # Use Jansen 1999 estimates
  Vi <- VY - ((1 / (2 * length(Y_A))) * sum((Y_B - Y_ABi) ^ 2))
  Vj <- VY - ((1 / (2 * length(Y_A))) * sum((Y_B - Y_ABj) ^ 2))
  Vk <- VY - ((1 / (2 * length(Y_A))) * sum((Y_B - Y_ABk) ^ 2))
  Vij <- (VY - ((1 / (2 * length(Y_A))) * sum((Y_B - Y_ABij) ^ 2))) - Vi - Vj
  Vik <- (VY - ((1 / (2 * length(Y_A))) * sum((Y_B - Y_ABik) ^ 2))) - Vi - Vk
  Vjk <- (VY - ((1 / (2 * length(Y_A))) * sum((Y_B - Y_ABjk) ^ 2))) - Vj - Vk
  Vijk <- (VY - ((1 / (2 * length(Y_A))) * sum((Y_B - Y_ABijk) ^ 2))) - Vij - Vik - Vjk - Vi - Vj - Vk
  Sijk <- Vijk / VY
  return(Sijk)
}

sobol_third_MapplyJ <- function(d, i) {
  data <- d[i, ]
  return(mapply(sobol_compute_thirdJ,
                data[, "Y_A"],
                data[, "Y_B"],
                data[, "Y_ABi"],
                data[, "Y_ABj"],
                data[, "Y_ABk"],
                data[, "Y_ABij"],
                data[, "Y_ABik"],
                data[, "Y_ABjk"],
                data[, "Y_ABijk"]))
}

# FUNCTION TO COMPUTE SOBOL' FIRST AND TOTAL-ORDER EFFECTS USING
# THE JANSEN 1999 ESTIMATOR FOR FIRST AND TOTAL INDICES -----------------------

sobol_computeS <- function(Y_A, Y_B, Y_AB) {
  # Compute sample mean of output
  f0 <- (1 / (2 * length(Y_A))) * sum(Y_A + Y_B)
  # Compute unconditional variance of output
  VY <- 1 / (2 * length(Y_A) - 1) * sum((Y_A - f0) ^ 2 + (Y_B - f0) ^ 2)
  # Compute first order indices (Saltelli et al. 2010)
  Si <- (1 / length(Y_A)) * sum(Y_B * (Y_AB - Y_A)) / VY
  # Compute total order indices (Jansen 1999)
  STi <- ((1 / (2 * length(Y_A))) * sum((Y_A - Y_AB) ^ 2)) / VY
  return(c(Si, STi))
}

sobol_MapplyS <- function(d, i) {
  data <- d[i, ]
  return(mapply(sobol_computeS,
                data[, "Y_A"],
                data[, "Y_B"],
                data[, "Y_AB"]))
}

# FUNCTION TO COMPUTE SOBOL' SECOND-ORDER EFFECTS USING
# THE SALTELLI ET AL. 2010 ESTIMATOR ------------------------------------------

sobol_compute_secondS <- function(Y_A, Y_B, Y_ABi,
                                  Y_ABj, Y_ABij) {
  # Compute sample mean of output
  f0 <- (1 / (2 * length(Y_A))) * sum(Y_A + Y_B)
  # Compute unconditional variance of output
  VY <- 1 / (2 * length(Y_A) - 1) * sum((Y_A - f0) ^ 2 + (Y_B - f0) ^ 2)
  # Use Saltelli et al. 2010 estimate
  Vi <- (1 / length(Y_A)) * sum(Y_B * (Y_ABi - Y_A))
  Vj <- (1 / length(Y_A)) * sum(Y_B * (Y_ABj - Y_A))
  Vij <- ((1 / length(Y_A)) * sum(Y_B * (Y_ABij - Y_A))) - Vi - Vj
  Sij <- Vij / VY
  return(Sij)
}

sobol_second_MapplyS <- function(d, i) {
  data <- d[i, ]
  return(mapply(sobol_compute_secondS,
                data[, "Y_A"],
                data[, "Y_B"],
                data[, "Y_ABi"],
                data[, "Y_ABj"],
                data[, "Y_ABij"]))
}

# FUNCTION TO COMPUTE SOBOL' THIRD-ORDER EFFECTS USING
# THE SALTELLI ET AL. 2010 ESTIMATOR ------------------------------------------

sobol_compute_thirdS <- function(Y_A, Y_B, Y_ABi,
                                 Y_ABj, Y_ABk, Y_ABij,
                                 Y_ABik, Y_ABjk, Y_ABijk) {
  # Compute sample mean of output
  f0 <- (1 / (2 * length(Y_A))) * sum(Y_A + Y_B)
  # Compute unconditional variance of output
  VY <- 1 / (2 * length(Y_A) - 1) * sum((Y_A - f0) ^ 2 + (Y_B - f0) ^ 2)
  # Use Saltelli et al. 2010 estimate
  Vi <- (1 / length(Y_A)) * sum(Y_B * (Y_ABi - Y_A))
  Vj <- (1 / length(Y_A)) * sum(Y_B * (Y_ABj - Y_A))
  Vk <- (1 / length(Y_A)) * sum(Y_B * (Y_ABk - Y_A))
  Vij <- ((1 / length(Y_A)) * sum(Y_B * (Y_ABij - Y_A))) -  Vi - Vj
  Vik <- ((1 / length(Y_A)) * sum(Y_B * (Y_ABik - Y_A))) - Vi - Vk
  Vjk <- ((1 / length(Y_A)) * sum(Y_B * (Y_ABjk - Y_A))) - Vj - Vk
  Vijk <- ((1 / length(Y_A)) * sum(Y_B * (Y_ABijk - Y_A))) - Vij - Vik - Vjk - Vi - Vj - Vk
  Sijk <- Vijk / VY
  return(Sijk)
}

sobol_third_MapplyS <- function(d, i) {
  data <- d[i, ]
  return(mapply(sobol_compute_thirdS,
                data[, "Y_A"],
                data[, "Y_B"],
                data[, "Y_ABi"],
                data[, "Y_ABj"],
                data[, "Y_ABk"],
                data[, "Y_ABij"],
                data[, "Y_ABik"],
                data[, "Y_ABjk"],
                data[, "Y_ABijk"]))
}

# FUNCTION TO COMPUTE SOBOL' INDICES FOR A DUMMY PARAMETER --------------------

sobol_dummyT <- function(Y_A, Y_B, Y_AB) {
  f0 <- (1 / length(Y_A)) * sum(Y_A * Y_B)
  VY <- 1 / (2 * length(Y_A) - 1) * sum(Y_A ^ 2 + Y_B ^ 2) - f0
  Si <- (1 / (length(Y_A) - 1) * sum(Y_A * Y_B) - f0) / VY
  STi <- 1 - (1 / (length(Y_A) - 1) * sum(Y_B * Y_B) - f0) / VY
  return(c(Si, STi))
}

sobol_dummy_Mapply <- function(d, i) {
  data <- d[i, ]
  return(mapply(sobol_dummyT,
                data[, "Y_A"],
                data[, "Y_B"],
                data[, "Y_AB"]))
}

sobol_dummy <- function(Y, params, R, n,
                        parallel = "no", ncpus = 1) {
  # Calculate the number of parameters
  k <- length(params)
  # Calculate the length of the A and B matrices
  p <- length(1:n)
  # Extract the model output of the A matrix
  Y_A <- Y[1:p]
  # Extract the model output of the B matrix
  Y_B <- Y[(p + 1) : (2 * p)]
  # Extract the model output of the AB matrix
  Y_AB <- Y[(2*p+1):(n * (k + 2))]
  # Create vector with parameters
  parameters <- rep(params, each = length(Y_A))
  # merge vector with data table
  vec <- cbind(Y_A, Y_B, Y_AB)
  out <- data.table::data.table(vec, parameters)
  out.1 <- out %>%
    .[Y_A > 0] %>%
    .[Y_B > 0] %>%
    .[Y_AB > 0] %>%
    # remove rows with NA
    stats::na.omit()
  # Bootstrap Sobol'indices
  Si.STi <- out.1[, list(list(boot::boot(.SD,
                                         sobol_dummy_Mapply,
                                         R = R,
                                         parallel = parallel,
                                         ncpus = ncpus)))]
  return(Si.STi)
}


# FUNCTION TO COMPUTE FIRST, SECOND, THIRD AND TOTAL
# SOBOL' INDICES --------------------------------------------------------------

sobol_indices <- function(Y, params, type = "jansen",
                          R, n, parallel = "no", ncpus = 1,
                          second = FALSE, third = FALSE) {
  if(second == FALSE & third == TRUE) {
    stop("The computation of third-order indices requires second = TRUE as it computes second-order indices first")
  }
  # Calculate the number of parameters
  k <- length(params)
  # Calculate the length of the A and B matrices
  p <- length(1:n)
  # Extract the model output of the A matrix
  Y_A <- Y[1:p]
  # Extract the model output of the B matrix
  Y_B <- Y[(p + 1) : (2 * p)]
  # Extract the model output of the AB matrix
  Y_AB <- Y[(2*p+1):(n * (k + 2))]
  # Create vector with parameters
  parameters <- rep(params, each = length(Y_A))
  # merge vector with data table
  vec <- cbind(Y_A, Y_B, Y_AB)
  out <- data.table::data.table(vec, parameters)
  out.1 <- out %>%
    # remove rows with NA
    stats::na.omit() %>%
    # remove negative model outputs
    .[Y_A > 0] %>%
    .[Y_B > 0] %>%
    .[Y_AB > 0]
  # Check which estimator to use for First-order indices
  if(type == "jansen") {
    Estimator1 <- sobol_MapplyJ
    Estimator2 <- sobol_second_MapplyJ
    Estimator3 <- sobol_third_MapplyJ
  } else if(type == "saltelli") {
    Estimator1 <- sobol_MapplyS
    Estimator2 <- sobol_second_MapplyS
    Estimator3 <- sobol_third_MapplyS
  }
  # Bootstrap Sobol'indices
  Si.STi <- out.1[, list(list(boot::boot(.SD,
                                         Estimator1,
                                         R = R,
                                         parallel = parallel,
                                         ncpus = ncpus))),
                  by = parameters]
  # CHECK IF THE MODEL OUTPUT INCLUDES
  # MODEL OUTPUT FOR THE CALCULATION OF
  # SECOND ORDER INDICES
  if(second == TRUE) {
    # Extract the model output of the
    # second order matrix
    Y_ABij <- Y[((n * (k + 2)) + 1):
                  ((n * (k + 2)) +
                     (n * factorial(k) / (factorial(2) * factorial(k - 2))))]
    # Create vector with pairs of parameters
    paired <- utils::combn(params, 2, simplify = FALSE)
    paramet<- lapply(paired, function(x) paste0(x, collapse = ".")) %>%
      unlist()
    parameters <- rep(paramet, each = length(Y_A))
    # create vectpr with Y_A, Y_B and Y_ABij
    vec <- cbind(Y_A, Y_B, Y_ABij)
    # Create data table
    out2 <- data.table::data.table(vec, parameters)
    # Set key in the matrix of the first and total indices
    data.table::setkey(out, "parameters")
    # Extract the first parameter
    first <- sub("\\..*", "", paramet)
    # Extract the second parameter
    second <- sub(".*\\.", "", paramet)
    # Extract the AB vector of the first parameter
    Y_ABi <- out[list(first), allow.cartesian = TRUE][, Y_AB]
    # Extract the AB vector of the second parameter
    Y_ABj <- out[list(second), allow.cartesian = TRUE][, Y_AB]
    # Merge with the AB matrix of the second order
    out2[, Y_ABi:= cbind(Y_ABi)][, Y_ABj:= cbind(Y_ABj)]
    # Remove rows with NA
    out3 <- out2 %>%
      stats::na.omit() %>%
      # remove negative model outputs
      .[Y_A > 0] %>%
      .[Y_B > 0] %>%
      .[Y_ABi > 0] %>%
      .[Y_ABj > 0] %>%
      .[Y_ABij > 0] 
    # Bootstrap second-order indices
    Sij <- out3[, list(list(boot::boot(.SD,
                                       Estimator2,
                                       R = R,
                                       parallel = parallel,
                                       ncpus = ncpus))),
                by = parameters]
  } else {
    Sij <- NULL
  }
  if(third == TRUE) {
    # Extract the model output of the
    # third order matrix
    Y_ABijk <- Y[(((n * (k + 2)) + (n * factorial(k) /
                                      (factorial(2) * factorial(k - 2))))+1):
                   length(Y)]
    # Create vector with pairs of parameters
    triplet <- utils::combn(params, 3, simplify = FALSE)
    paramet3 <- lapply(triplet, function(x) paste0(x, collapse = ".")) %>%
      unlist()
    parameters <- rep(paramet3, each = length(Y_A))
    # create vectpr with Y_A, Y_B and Y_ABijk
    vec <- cbind(Y_A, Y_B, Y_ABijk)
    # Create data table
    out4 <- data.table::data.table(vec, parameters)
    # Set key in the matrix of the first and total indices
    data.table::setkey(out, "parameters")
    # Set key in the matrix of the second-order indices
    data.table::setkey(out2, "parameters")
    # Extract the first parameter
    first <- sub("\\..*", "", paramet3)
    # Extract the second parameter
    second <- unlist(lapply(paramet3, function(x) unlist(strsplit(x, "[.]"))[[2]]))
    # Extract the third parameter
    third <- sub(".*\\.", "", paramet3)
    # Extract the ij parameter
    ij <- sub(".[^.]*$", "", paramet3)
    # Extract the jk parameter
    jk <- gsub("^.*?\\.","", paramet3)
    # Extract the ik parameter
    ik <- paste(stringr::word(paramet3, 1, sep = stringr::fixed(".")),
                stringr::word(paramet3, 3, sep = stringr::fixed(".")),
                sep = ".")
    # Extract the AB vector of the first parameter
    Y_ABi <- out[list(first), allow.cartesian = TRUE][, Y_AB]
    # Extract the AB vector of the second parameter
    Y_ABj <- out[list(second), allow.cartesian = TRUE][, Y_AB]
    # Extract the AB vector of the third parameter
    Y_ABk <- out[list(third), allow.cartesian = TRUE][, Y_AB]
    # Extract the AB vector of the ij
    Y_ABij <- out2[list(ij), allow.cartesian = TRUE][, Y_ABij]
    # Extract the AB vector of the jk
    Y_ABjk <- out2[list(jk), allow.cartesian = TRUE][, Y_ABij]
    # Extract the AB vector of the ik
    Y_ABik <- out2[list(ik), allow.cartesian = TRUE][, Y_ABij]
    # Merge with the AB matrix of the second order
    out4[, Y_ABi:= cbind(Y_ABi)][
      , Y_ABj:= cbind(Y_ABj)
      ][
        , Y_ABk:= cbind(Y_ABk)
        ][
          , Y_ABij:= cbind(Y_ABij)
          ][
            , Y_ABjk:= cbind(Y_ABjk)
            ][
              , Y_ABik:= cbind(Y_ABik)
              ]
    # Remove rows with NA
    out5 <- out4 %>%
      stats::na.omit() %>%
      # remove negative model outputs
      .[Y_A > 0] %>%
      .[Y_B > 0] %>%
      .[Y_ABi > 0] %>%
      .[Y_ABj > 0] %>%
      .[Y_ABk > 0] %>%
      .[Y_ABij > 0] %>%
      .[Y_ABjk > 0] %>%
      .[Y_ABik > 0] %>%
      .[Y_ABijk > 0]
    # Bootstrap third-order indices
    Sijk <- out5[, list(list(boot::boot(.SD,
                                        Estimator3,
                                        R = R,
                                        parallel = parallel,
                                        ncpus = ncpus))),
                 by = parameters]
  } else {
    Sijk <- NULL
  }
  return(rbind(Si.STi, Sij, Sijk))
}




full.dt <- fread("full.dt.csv")
AB.dt <- fread("AB.dt.csv")


## ----sobol_settings, cache=TRUE, dependson="arrange_output"------------------------------

# SETTING FOR SOBOL' INDICES ----------------------------------------------------

# Set the number of bootstraps
R <- 1000

# Set the confidence interval method
type <- "norm"

# Set the confidence interval
conf <- 0.95

# Create vector with the name of the clusters
cluster <- c("Irrigation", "Population", "Model")


## ----sobol_indices, cache=TRUE, dependson=c("arrange_output", "sobol_settings", "sobol_functions")----

# COMPUTE SOBOL' INDICES --------------------------------------------------------

# Compute Sobol' indices
out <- full.dt[, sobol_indices(Y,
                               params = c(parameters.renamed, cluster),
                               type = "saltelli",
                               R = R,
                               n = n,
                               parallel = "multicore",
                               ncpus = floor(detectCores() * 0.75)),
               by = Continent]

## ----sobol_ci, cache=TRUE, dependson=c("sobol_indices", "sobol_settings", "sobol_functions")----

# SOBOL' CONFIDENCE INTERVALS ---------------------------------------------------

# Compute confidence intervals
tmp <- split(out, out$Continent)
out.ci <- list()
for(i in names(tmp)) {
  out.ci[[i]] <- sobol_ci(tmp[[i]],
                          params = c(parameters.renamed, cluster),
                          type = type,
                          conf = conf)
}

sensobol::sobol_c
## ----sobol_indices_dummy, cache=TRUE, dependson=c("arrange_output", "sobol_settings", "sobol_functions")----

# SOBOL' INDICES OF A DUMMY PARAMETER -------------------------------------------

# For the model parameters
out.dummy <- full.dt[, .SD[1:(n * (k + 2))], Continent] %>%
  .[, sobol_dummy(Y,
                  params = parameters.renamed,
                  R = R,
                  n = n,
                  parallel = "multicore",
                  ncpus = floor(detectCores() * 0.5)),
    by = Continent]

# Compute confidence intervals
tmp.dummy <- split(out.dummy, out.dummy$Continent)
out.dummy.ci <- list()
for(i in names(tmp.dummy)) {
  out.dummy.ci[[i]] <- sobol_ci_dummy(tmp.dummy[[i]],
                                      type = type,
                                      conf = conf)
}
out.dummy.ci2 <- rbindlist(out.dummy.ci, idcol = "Continent")

# For the clusters of parameters
tmp1 <- full.dt[, .SD[1:(2 * n)], Continent] %>%
  split(., .$Continent)

tmp2 <- full.dt[full.dt[, tail(.I, length(cluster) * n), by = Continent]$V1, ] %>%
  split(., .$Continent)

for(i in names(tmp1)) {
  tmp1[[i]] <- rbind(tmp1[[i]], tmp2[[i]])
}
out.dummy.cluster <- rbindlist(tmp1) %>%
  .[, sobol_dummy(Y,
                  params = cluster,
                  R = R,
                  n = n,
                  parallel = "multicore",
                  ncpus = floor(detectCores() * 0.5)),
    by = Continent]

# Compute confidence intervals
tmp.dummy <- split(out.dummy.cluster, out.dummy.cluster$Continent)
out.dummy.cluster.ci <- list()
for(i in names(tmp.dummy)) {
  out.dummy.cluster.ci[[i]] <- sobol_ci_dummy(tmp.dummy[[i]],
                                              type = type,
                                              conf = conf)
}

out.dummy.cluster.ci2 <- rbindlist(out.dummy.cluster.ci, idcol = "Continent")


## ----export_sobol_indices, cache=TRUE, dependson=c("sobol_ci_dummy", "sobol_ci", "sobol_settings", "sobol_functions")----

# EXPORT SOBOL' INDICES ---------------------------------------------------------

sobol.ci <- rbindlist(out.ci, idcol = "Continent") 
fwrite(sobol.ci, "sobol.ci.csv")


## ----prepare_plot_sobol, cache=TRUE, dependson=c("sobol_ci_dummy", "sobol_ci", "sobol_settings", "sobol_functions")----

# PREPARE PLOT SOBOL' INDICES ---------------------------------------------------

# Plot Sobol' indices of parameters
a <- rbindlist(out.ci, idcol = "Continent") %>%
  .[!parameters %in% cluster] %>%
  plot_sobol(., type = 1, dummy = out.dummy.ci2) + 
  scale_y_continuous(breaks = pretty_breaks(n = 3)) +
  facet_wrap(~Continent, ncol = 1) +
  labs(y = "Sobol' indices", 
       x = "" )

# Plot Sobol' indices of clusters of parameters
b <- rbindlist(out.ci, idcol = "Continent") %>%
  .[parameters %in% cluster] %>%
  plot_sobol(., type = 1, dummy = out.dummy.cluster.ci2) + 
  scale_y_continuous(breaks = pretty_breaks(n = 3)) +
  facet_wrap(~Continent, ncol = 1) +
  labs(x = "", 
       y = "")


## ----plot_sobol, cache=TRUE, dependson="prepare_plot_sobol", dev="tikz", fig.height=5.3, fig.width=6.2----

# PLOT SOBOL' INDICES ------------------------------------------------------------

# Merge legend and a and b
plot_grid(a + theme(legend.position = c(0.4, 0.95), 
                    legend.direction = "horizontal"), 
                    b + theme(legend.position="none"), 
                    ncol = 2, 
                    labels = "AUTO", 
                    rel_widths = c(2, 1.1))


## ----prepare_plot_sobol2, cache=TRUE, dependson=c("sobol_ci_dummy", "sobol_ci", "sobol_settings", "sobol_functions"), dev="tikz", fig.height=5, fig.width=5.2----

# Merge both plots ----------
bottom <- plot_grid(a + theme(legend.position="none", 
                              axis.text.x = element_text(size = 7.2)), 
                    b + theme(legend.position="none", 
                              axis.text.x = element_text(size = 7.2)), 
                    ncol = 2, 
                    labels = "AUTO", 
                    rel_widths = c(2, 1.1))

plot_grid(legend, 
          bottom, 
          ncol = 1, 
          rel_heights = c(0.15, 1))


## ----check_sum_si, cache=TRUE, dependson="export_sobol_indices"--------------------------

# CHECK SUM OF SI INDICES -------------------------------------------------------

# Parameters only, no cluster
sobol.ci[!parameters %in% c("Irrigation", "Population", "Model")] %>%
  .[sensitivity == "Si"] %>%
  .[, sum(original),  Continent]

# Cluster only
sobol.ci[parameters %in% c("Irrigation", "Population", "Model")] %>%
  .[sensitivity == "Si"] %>%
  .[, sum(original),  Continent]


## ----pop_model, cache=TRUE---------------------------------------------------------------

# CREATE POPULATION MODEL -------------------------------------------------------

population_fun <- function(N0, r, t, gamma) {
  N <- N0
  for(i in 1: (t-1)) {
    N[i+1] <- N[i] + r * N[i] ^ gamma
  }
  return(N[length(N)])
}


## ----run_pop_model, cache=TRUE, dependson=c("arrange_output", "pop_model")---------------

# RUN POPULATION MODEL ----------------------------------------------------------

AB.dt2 <- AB.dt[, N50:= population_fun(N0 = N, r = r, t = t, gamma = gamma ), 
      seq_len(nrow(AB.dt))]


## ----arrange_pop_data, cache=TRUE, dependson=c("preliminary steps", "arrange_output", "global_uncertainty")----

# ARRANGE DATA ------------------------------------------------------------------

projections_N <- fread("projections_N.csv")

# Change level of factors
projections_N[, Estimate:= factor(Estimate, levels = c("Low", "Medium", "High"))]

# Assess uncertainty at the global level
global.population <- AB.dt2 %>%
  .[, .(Continent, Y, N50)] %>%
  split(., .$Continent) %>%
  lapply(., function(x) x[, N50]) %>%
  do.call("cbind", .) %>%
  data.table() %>%
  .[, Total_N50:= rowSums(.)]

# Bind
dt.total <- data.table(cbind(global.uncertainty$Total, global.population$Total_N50)) %>%
  setnames(c("V1", "V2"), c("Total.irrigation", "Total.population"))


## ----plot_pop_plots, cache=TRUE, dependson="arrange_pop_data"----------------------------

# PLOT POPULATION AND IRRIGATED AREAS SCATTERPLOTS -------------------------------

a <- ggplot(AB.dt2, aes(N50, Y)) +
  geom_point(alpha = 0.05, size = 0.1) + 
  geom_vline(data = projections_N,
             aes(xintercept = N,
                 colour = Estimate),
             lty = 2,
             size = 1) + 
  geom_rect(data = quant,
            aes(xmin = -Inf,
                xmax = Inf,
                ymin = X2.5.,
                ymax = X97.5.,
                group = Continent),
            fill = "green",
            color = "darkgreen",
            alpha = 0.1,
            inherit.aes = FALSE) +
  facet_wrap(~Continent, 
             scales = "free") + 
  scale_color_manual(values = c("green", "orange", "red")) +
  labs(x = "", 
       y = "Area irrigated (Mha)") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent",
                                         color = "white"),
        legend.key = element_rect(fill = "transparent",
                                  color = NA))

b <- ggplot(dt.total, aes(Total.population, Total.irrigation)) +
  geom_point(alpha = 0.05, size = 0.1) +
  geom_vline(data = projections_N[, sum(N), Estimate],
             aes(xintercept = V1,
                 colour = Estimate),
             lty = 2,
             size = 1) +
  geom_rect(data = global.quantile,
            aes(ymin = X2.5.,
                ymax = X97.5.,
                xmin = -Inf,
                xmax = Inf),
            fill = "green",
            color = "darkgreen",
            alpha = 0.1,
            inherit.aes = FALSE) +
  scale_color_manual(values = c("green", "orange", "red")) +
  labs(x = "Population (Mha)", 
       y = "Area irrigated (Mha)") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent",
                                         color = "white"),
        legend.key = element_rect(fill = "transparent",
                                  color = NA))

# Extract legend
legend <- get_legend(a + theme(legend.position = "top"))

bottom <- plot_grid(a, b, ncol = 1, labels = "auto")


## ----plot_pop_plots_final, cache=TRUE, dependson="plot_pop_plots", fig.height=8.5, fig.width=6----

# PLOT FINAL ---------------------------------------------------------------------

plot_grid(legend, bottom, rel_heights = c(0.15, 1), ncol = 1)


## ----population_un_sim-------------------------------------------------------------------

# COMPARE OUR POPULATION VALUES WITH THE UN -------------------------------------

# Proportion of extreme irrigated areas in Africa caused by populations higher 
#than maximum projected by the UN
AB.dt2[Y %in% K] %>%
  .[Continent == "Africa", sum(N50 > 2785.0828) / .N]

# Proportion of extreme irrigated areas in Asia caused by populations higher 
#than maximum projected by the UN
AB.dt2[Y %in% K] %>%
  .[Continent == "Asia", sum(N50 > 5860.5128) / .N]


## ----session_information-----------------------------------------------------------------

# SESSION INFORMATION ------------------------------------------------------------

sessionInfo()

## Return the machine CPU
cat("Machine:     "); print(get_cpu()$model_name)

## Return number of true cores
cat("Num cores:   "); print(detectCores(logical = FALSE))

## Return number of threads
cat("Num threads: "); print(detectCores(logical = TRUE))

## Return the machine RAM
cat("RAM:         "); print (get_ram()); cat("\n")




AB.dt <- fread("AB.dt.csv")



# ARRANGE SCATTERPLOTS OF PARAMETERS VS MODEL OUTPUT ----------------------------

# Function to recode some parameters to allow plotting
code_columns <- function(x) {
  dt <- ifelse(x == "Aquastat", 1, 
               ifelse(x == "FAOSTAT", 2,
                      ifelse(x == "Siebert.et.al.2013", 3, 
                             ifelse(x == "Meier.et.al.2018", 4, 
                                    ifelse(x == "Salmon.et.al.2015", 5, 6)))))
  return(dt)
}

code_columns2 <- function(x) ifelse(x == "YES", 1, 2)

# Vector with renamed parameters for better plotting
parameters.renamed <- c("X1", "X2", "X3", "X4", "W1", "W3", "W4", "r", "$\\gamma$", 
                        "Y0", "t", "K", "Wa", "$\\eta$")

# Create temporary data table to plot
tmp <- cbind(AB.dt[, .(Continent)], AB.dt[, ..parameters], AB.dt[, .(Y)]) 

# Update columns and arrange
# Update columns to allow plotting of scatterplots
col_names <- c("X1", "W1")
col_names2 <- c("X3", "W3")
tmp <- tmp[, (col_names):= lapply(.SD, code_columns), .SDcols = col_names]
tmp <- tmp[, (col_names2):= lapply(.SD, code_columns2), .SDcols = col_names2]
tmp <- tmp[, X2:= ifelse(X2 == "OLS", 1, 2)]

tmp2 <- gather(tmp, Parameters, Values, X1:eta) %>%
  split(., .$Continent) 

# PLOT SCATTERPLOTS OF PARAMETERS VS MODEL OUTPUT ------------------------------

gg <- list()
for(i in names(tmp2)) {
  gg[[i]] <- ggplot(tmp2[[i]], aes(Values, Y)) +
    geom_hex() +
    scale_x_continuous(breaks = pretty_breaks(n = 3)) +
    scale_fill_gradient(breaks = pretty_breaks(n = 2)) +
    scale_y_log10() +
    scale_alpha(guide = "none") + 
    labs(x = "Values",
         y = "Area irrigated 2050 (Mha)") +
    facet_wrap(~Parameters,
               scales = "free_x",
               ncol = 3, 
               labeller = label_parsed) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.background = element_rect(fill = "transparent", 
                                           color = NA), 
          legend.key = element_rect(fill = "transparent", 
                                    color = NA), 
          legend.position = "top") +
    ggtitle(names(tmp2[i]))
}

gg[[1]]
gg[[2]]
gg[[3]]
gg[[4]]

















devtools::install_github("arnaldpuy/sensobol", build_vignettes = TRUE)
library(sensobol)


params <- paste("X", 1:3, sep = "")
sensobol::sobol_matrices(N = 10, params = params, cluster = list(c(1, 3)))

