# Forecasting Australian food demand by population scenario

# Inputs:

# Food balance data from FAOSTAT (old version)
# Mapping of commodities to LUTO classes.
# Grams per kcal per commodity
# Population projection

# Outputs:

# CSV with total projection of kcal per capita per day
# CSV with compositional forecast of domestic consumption by SPREAD? commodity
# images for reporting:

# mar77v@csiro.au

# based on https://forecasters.org/wp-content/uploads/gravity_forms/7-621289a708af3e7af65a7cd487aee6eb/2015/07/Ord_Keith_ISF2015.pdf 
# Clear workspace
rm(list=ls())

x = c("forecast", "ggplot2", "RColorBrewer", "scales",  "reshape2", "cowplot", "ggthemes", 
      "dplyr", "randomcoloR", "patchwork", "plotly", "ggpubr", "taRifx", "tidyr")
# lapply(x, install.packages)
lapply(x, library, character.only = TRUE);

# -----------------------------------------------------------------------------------------------
# INPUT DATA

# population

hist.pop = read.csv("Faostat AU population 1950-2018.csv")
hist.pop = hist.pop %>%
  filter(Element == "Total Population - Both sexes") %>%
  filter(Year >= 1961)%>%
  subset(select = c(Year, Value)) %>%
  group_by(Year) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
colnames(hist.pop) = c("Year", "pop1000")

# Population projections
pop = read.csv("Projected population, Australia.csv")
all.scen = c("Low.series", "Medium.series", "High.series", "Zero.net.overseas.migration")

popsrc = "ABS"
# Historical and projected consumption per capita per year
kgpcpy = read.csv("food supply annual kg pc trend and bounds 1961-2060.csv", stringsAsFactors = F)

for (pop.scen in all.scen){
  
  # join historical (1961-2018) and projected (2019-1960) population data for scenario X 
  pop.hp = as.data.frame(1961:2060)
  colnames(pop.hp) = c("Year")
  pop.hp$population = 0
  pop.hp$population[1:58] = hist.pop$pop1000 *1000
  pop.hp$population[59:100] = pop[(pop$Year >=2019 & pop$Year <=2060 ),c(pop.scen)]
  
  # Compute total consumption per tonne
  
  fs.tonnes = merge(kgpcpy, pop.hp, by  = "Year")
  fs.tonnes$lo95.tonnes = fs.tonnes$lo95.kgpc * fs.tonnes$population/1000
  fs.tonnes$lo80.tonnes = fs.tonnes$lo80.kgpc * fs.tonnes$population/1000
  fs.tonnes$trend.tonnes = fs.tonnes$trend.kgpc * fs.tonnes$population/1000
  fs.tonnes$hi80.tonnes = fs.tonnes$hi80.kgpc * fs.tonnes$population/1000
  fs.tonnes$hi95.tonnes = fs.tonnes$hi95.kgpc * fs.tonnes$population/1000
  
  fs.tonnes = fs.tonnes %>% 
        subset(select = -c( lo95.kgpc:population))
# 
#   fs.tonnesL = fs.tonnes %>% 
#     subset(select = -c( lo95.kgpc:population)) %>%
#     pivot_longer(cols = lo95.tonnes:hi95.tonnes, 
#                  names_to = c("categories"), 
#                  values_to = "Tonnes")
  
   # summary(fs.tonnes)
  divisor = 1000

  hp.tonnes <- ggplot(fs.tonnes, aes(x=Year, y = trend.tonnes/divisor)) +
    ylab("1000 tonnes") +
    scale_x_continuous(breaks=c(1961, 1980, 2000, 2020, 2040, 2060)) +
    geom_ribbon(aes(ymin=lo95.tonnes/divisor, ymax=hi95.tonnes/divisor), fill = "grey70", alpha = 0.8) +
    geom_ribbon(aes(ymin=lo80.tonnes/divisor, ymax=hi80.tonnes/divisor), fill = "black", alpha = 0.2) +
    geom_line() +
    facet_wrap( ~ ItemC, ncol=4, scales="free_y") +
    theme_cowplot()
  
  # ggplotly(hp.tonnes)

  ggsave(paste("Historical data and projections, tonnes,", pop.scen, "population", popsrc, ".tiff", sep = " ") , plot = hp.tonnes,
         dpi = 400, width = 300, height = 350, units = "mm",
         compression="lzw", type="cairo")

  write.csv(fs.tonnes, paste("historical and projected food supply", pop.scen, popsrc, ".csv"), row.names = F)
}


