# Forecasting Australian food demand 

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
      "dplyr", "randomcoloR", "patchwork", "plotly", "ggpubr")
# lapply(x, install.packages)
lapply(x, library, character.only = TRUE);

# setwd("H:/Dropbox/Australia/GISERA/SA conventional gas project/projections/")
# setwd("C:/Users/mar77v/Dropbox/Australia/GISERA/SA conventional gas project/projections/")

# setwd("C:/Users/mar77v/Deakin University/LUF Modelling - Trade and Food Demand/domestic_demand/data/")
# setwd("F:/Deakin University/LUF Modelling - Trade and Food Demand/domestic_demand/data/")
# setwd("C:/Users/mar77v/OneDrive - CSIRO/github/FoodDemandAU/")

# -------------------------------------------------------------------------------------------------------
# Load input data

fs.data = read.csv("FAOSTAT Food supply Crops, Livestock and Fish kcal per day.csv", stringsAsFactors = F)

# Remove duplicates

duplicates = c( "Groundnuts (in Shell Eq)",
                "Rice (Paddy Equivalent)",
                "Sugar (Raw Equivalent)",
                "Sugar, Refined Equiv",
                "Milk, Whole",
                "Cheese",
                "Roots & Tuber Dry Equiv"
                )
fs.data = fs.data[!fs.data$Item %in% duplicates,]

# unique(fs.data$Element)
# commodities = unique(fs$Item[fs$Element == "Food supply (kcal/capita/day)"])
# write.csv(commodities, "categories food demand.csv")

# Add categories relevant to our analysis
categories = read.csv("categories food demand.csv")
categories$Considerations = NULL

fs = merge(fs.data, categories, by = "Item", all.x = T, all.y = F)


# kg per capita per day

# g_pc =  fs%>%
#   filter(Element == "Food supply quantity (g/capita/day)") %>%
#   group_by(Year, Item ) %>%
#   subset(select=c("Year", "Item", "Value")) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE) 
# colnames(g_pc) = c("Year", "Item", "grams")
# # kcal per capita per day
# kcal_pc =  fs%>%
#   filter(Element == "Food supply (kcal/capita/day)") %>%
#   group_by(Year, Item ) %>%
#   subset(select=c("Year", "Item", "Value")) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE) 
# colnames(kcal_pc) = c("Year", "Item", "kcal")

# # Merge to get estimate of kcal per gram. The number of commodities is not the same
# 
# g2kcal = merge(kcal_pc, g_pc, by = c("Item", "Year"))
# g2kcal$kcal_gram = g2kcal$grams/g2kcal$kcal
# g2kcal = g2kcal%>% 
#   group_by(Item ) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE) 



# -----------------------------------------------------------------------------------------------------------------
# Preliminary visual analysis kcal per capita per day
# -----------------------------------------------------------------------------------------------------------------

# Change in kcal per capita for crops, livestock and fish products 
kcal_pct = fs%>%
  filter(Element == "Food supply (kcal/capita/day)") %>%
  group_by(Year, Domain ) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(percentage = Value / sum(Value)) 


# Plot
pc_CL = ggplot(kcal_pct, aes(x=Year, y=percentage*100, fill=Domain)) + 
  geom_area(alpha=0.6 , size=0.5, colour="grey") +
  ylab("% kcal/capita/day") +
  theme_cowplot() +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                      name = "Share of \nkcal/capita/day", 
                    labels = c("Crops", "Livestock and seafood")) +
  theme(legend.position="bottom")
  
  
pc_CL

ggsave("Kcal per capita crops, livestock and seafood.tiff", 
       plot = pc_CL, dpi = 400, width = 130, height = 110, units = "mm",
       compression="lzw", type="cairo")

# ggplotly(pc_CL)

# Aggregated FAOSTAT commodities


kcal_pct_item = fs%>%
  filter(Element == "Food supply (kcal/capita/day)") %>%
  group_by(Year, ItemC ) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(percentage = Value / sum(Value)) 

# # Plot
# ap = ggplot(kcal_pct_item, aes(x=Year, y=percentage, fill=ItemC)) + 
#   geom_area(alpha=0.6 , size=0.5, colour="grey") +
#   scale_y_continuous(labels = scales::percent) +
#   scale_fill_brewer(palette = mycolors ) +
#   theme_cowplot()
# 
# ggplotly(ap)
# 
# # Trend lines
# fs.fao.pl = ggplot(kcal_pct_item, aes(x=Year, y=percentage)) + 
#   geom_line(aes( color=ItemC)) + 
#   scale_y_continuous(labels = scales::percent, limits = c(0,0.3)) +
#   theme_cowplot() +
#   ylab("share of kcal/capita/day")
# 
# ggplotly(fs.fao.pl)

# LUF categories

fs.luf = fs%>%
  filter(Element == "Food supply (kcal/capita/day)") %>%
  group_by(Year, ItemC ) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(percentage = Value / sum(Value)) 


# Define the number of colors you want
nb.cols <- length(unique(fs.luf$ItemC))
# mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
mycolors <- distinctColorPalette(nb.cols)

# Plot
fs.luf.pa = ggplot(fs.luf, aes(x=Year, y=percentage, fill=ItemC)) + 
  geom_area(alpha=0.6 , size=0.5, colour="grey") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual("Commodity", values  = mycolors) +
  theme_cowplot() +
  ylab("% of daily kcal intake") 

ggplotly(fs.luf.pa)

ggsave("Trends in food consumption Area.tiff", 
       plot = fs.luf.pa, dpi = 400, width = 400, height = 300, units = "mm",
       compression="lzw", type="cairo")

# Trend lines

fs.luf.pl = ggplot(fs.luf, aes(x=Year, y=percentage)) + 
  geom_line(aes( color=ItemC)) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,0.3)) +
  scale_color_manual("",values  = mycolors) +
  theme_cowplot() +
  ylab("share of kcal/capita/day") +
  theme(legend.position="bottom") +
  theme(legend.key.size = unit(0.6, "cm"))+ # legend box size
  theme(legend.text=element_text(size=rel(0.9))) #legend font size
  

ggsave("Trends in food consumption lines.tiff", 
       plot = fs.luf.pl, dpi = 400, width = 320, height = 220, units = "mm",
       compression="lzw", type="cairo")

ggplotly(fs.luf.pl)


# -----------------------------------------------------------------------------------------------------------------
# Time series modelling
# Kcal per capita per year
# -----------------------------------------------------------------------------------------------------------------
# fsa = read.csv("Food Supply - Crops, Livestock, Fish Primary Equivalent_Aggregated.csv", stringsAsFactors = F)
# 
# unique(fsa$Element)
# 
# # Change in kcal per capita for crops, livestock and fish products
# kcal_total = fsa%>%
#   filter(Element == "Food supply (kcal/capita/day)") %>%
#   filter(ItemC %in%  c("Grand Total") )
# # Remove aggregating categories 
# kcal_pct = fsa%>%
#   filter(Element == "Food supply (kcal/capita/day)") %>%
#   filter(!ItemC %in% c("Vegetal Products", "Animal Products", "Grand Total")) %>%
#   group_by(Year, ItemC ) %>%
#   summarise_if(is.numeric, sum, na.rm = TRUE) %>%
#   mutate(percentage = (Value / sum(Value)))

kcal_total = fs%>%
  filter(Element == "Food supply (kcal/capita/day)") %>%
  group_by(Year) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)


# Plot
ap = ggplot(kcal_total, aes(x=Year, y=Value)) + 
  geom_line(alpha=0.6 , size=0.5, colour="grey") +
  theme_cowplot()+
  labs(title = "Total Food Supply",
       subtitle = "1960 - 2013",
       caption = "Australian Land-Use Futures") +
  ylab("kcal/capita/day" )

# ggplotly(ap)

# # Change per commodity
# # Remove aggregating categories 



kcal_pct = fs%>%
  filter(Element == "Food supply (kcal/capita/day)") %>%
  group_by(Year, ItemC ) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(percentage = (Value / sum(Value)))
# 
# unique(kcal_pct$ItemC)
# Check that the percents add up to 1
kcal_pct[,c("Year", "percentage")] %>% group_by(Year) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)


# Plot

# # area chart
# ap = ggplot(kcal_pct, aes(x=Year, y=percentage, fill=ItemC)) +
#   geom_bar(position="fill", stat="identity") +
#   scale_fill_manual(values = mycolors) +
#   theme_cowplot() +
#   ylab("share of kcal/capita/day")
#
# ggplotly(ap)

# # Trend lines
# ap = ggplot(kcal_pct, aes(x=Year, y=percentage)) +
#   geom_line(aes( color=ItemC)) +
#   scale_y_continuous(labels = scales::percent, limits = c(0,0.3)) +
#   scale_color_manual(values  = mycolors) +
#   theme_cowplot() +
#   ylab("share of kcal/capita/day")
# 
# ggplotly(ap)

# -----------------------------------------------------------------------------------------------------------------
# Trends in total kcal/capita/day 
# -----------------------------------------------------------------------------------------------------------------

tval = kcal_total$Value

# TRENDS IN VALUES
tval.ts = ts(tval, start = c(1961), end =c(2013), frequency = 1)

tval.ts_fit = ets(tval.ts) # Allows maintaining a trend component
projs =  as.data.frame(forecast.ets(tval.ts_fit, h = 47))

df_ = data.frame(matrix(vector(), 100 , 7))
colnames(df_) =c("historical", "fitted", "Lo95", "Lo80","trend", "Hi80", "Hi95")

df_[1:53, "historical"] = as.numeric(tval)
df_[1:53, "fitted"] = as.data.frame(fitted(tval.ts_fit))
df_[54:100, "Lo95"] = as.numeric(projs[,"Lo 95"])
df_[54:100, "Lo80"] = as.numeric(projs[,"Lo 80"])
df_[54:100, "trend"] = as.numeric(projs[,"Point Forecast"])
df_[54:100, "Hi80"] = as.numeric(projs[,"Hi 80"])
df_[54:100, "Hi95"] = as.numeric(projs[,"Hi 95"])
# df_[25:40, "UB80"] = as.numeric(projs[,"Hi 80"])
df_["year"] = 1961:2060

colores <- c("historical"="Black","fitted"="Orange","trend"="Green")

tval_F =   ggplot(data=df_, aes(x=year)) + 
  geom_line(aes(y=historical, color = "historical"), size = 0.8) +
  geom_line(aes(y = fitted, color = "fitted"), size = 0.8, linetype = "dashed") +
  geom_line(aes(y = trend, color = "trend"), size = 0.8, linetype = "dashed") +
  geom_ribbon(aes(ymin= Lo95, ymax= Hi95), linetype=2, alpha=0.1) + 
  geom_ribbon(aes(ymin= Lo80, ymax= Hi80), linetype=2, alpha=0.1) +
  ylim(0,4000) +
  ylab("kcal/capita/day") + 
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  scale_colour_manual(name=" ",values=colores)  +
  scale_x_continuous(breaks=c(1961, 1980, 2000, 2020, 2040, 2060)) +
  theme_cowplot()

tval_F
ggsave("projected total kcal-capita-day.tiff", plot = tval_F, dpi = 400, width = 190, height = 120, units = "mm",
       compression="lzw", type="cairo")

# write.csv(df_shares, paste("projections ", j, "smoothing factor", jf, tF, ".csv",sep = " "))
write.csv(df_, "projected total kcal-capita-day.csv", row.names = F)


# # TS-X
# 
# tvalX = kcal_total$Value[kcal_total$Year >= 1990]
# 
# # TRENDS IN VALUES
# tvalX.ts = ts(tvalX, start = c(1990), end =c(2013), frequency = 1)
# 
# tvalX.ts.fit = ets(tvalX.ts) # Allows maintaining a trend component
# projsX =  as.data.frame(forecast.ets(tvalX.ts.fit, h = 47))
# 
# df_ = data.frame(matrix(vector(), 61 , 5))
# colnames(df_) =c("historical", "fitted", "Lo 95", "trend", "Hi 95")
# 
# df_[1:24, "historical"] = as.numeric(tvalX)
# df_[1:24, "fitted"] = as.data.frame(fitted(tvalX.ts.fit))
# df_[25:61, "Lo 95"] = as.numeric(projsX[,"Lo 95"])
# df_[25:61, "trend"] = as.numeric(projsX[,"Point Forecast"])
# df_[25:61, "Hi 95"] = as.numeric(projsX[,"Hi 95"])
# # df_[25:40, "UB80"] = as.numeric(projs[,"Hi 80"])
# df_["year"] = 1990:2050
# 
# colores <- c("historical"="Red","fitted"="Orange","trend"="Green")
# 
# tval_F =   ggplot(data=df_, aes(x=year)) + 
#   geom_line(aes(y=historical, color = "historical"), size = 0.8) +
#   geom_line(aes(y = fitted, color = "fitted"), size = 0.8, linetype = "dashed") +
#   geom_line(aes(y = trend, color = "trend"), size = 0.8, linetype = "dashed") +
#   geom_ribbon(aes(ymin=LB, ymax=UB), linetype=2, alpha=0.1) + 
#   ylim(0,5000) +
#   ylab("kcal/capita/day") + 
#   guides(colour = guide_legend(override.aes = list(size=3))) + 
#   scale_colour_manual(name=" ",values=colores)  +
#   theme_cowplot()
# tval_F
# 
# LT.projection = forecast(tvalX.ts.fit, 47)
# 
# # Explanatory variables
# X = read.csv("X_historical_data.csv")
# X = X[X$Year <2014, c("Population.WB", "GDP.PPP.OECD.Hist", "Rural.population.WB")]
# mapafit.x <- mapaest(tvalX.ts,type="es",display=1,outplot=1,xreg=as.matrix(X))
# 
# # Xp = read.csv("projX.csv")
# 
# frc.x <- mapafor(tvalX.ts,mapafit.x,ifh=61,fh=61,xreg=as.matrix(Xp),conf.lvl=c(0.8,0.9,0.95),comb="w.mean")
# 



# -----------------------------------------------------------------------------------------------------------------
# COMPOSITIONAL DATA
# -----------------------------------------------------------------------------------------------------------------

# Create the log of the ratios
va_ratios = kcal_pct[, c("Year", "ItemC", "Value", "percentage")]
va_ratios$logratio = 9999

va_ratios$percentage[va_ratios$percentage == 0 ] = 0.0000001

# Fruits is the reference category

for (t in 1961:2013){
  va_ratios$logratio[va_ratios$Year == t] = log(
    va_ratios$percentage[va_ratios$Year == t] / va_ratios$percentage[va_ratios$Year == t & va_ratios$ItemC == "Oilseeds" ])
}

sm_factor = 0.01

# Factor to remove variability and isolate long term trends.
# smooth_fact = seq(0.01, 0.95, 0.05)
# smooth_fact = c(0.01)

items = unique(va_ratios$ItemC)
items = items[items != "Oilseeds"]
# 
# # # Create empty dataframe to store the point forecasts
df = data.frame(matrix(vector(), 47, length(items)))
colnames(df) = items
# 
# for(jf in smooth_fact){
#   print(jf)
#   
#   if(jf != 0.01){
#     jf = jf - 0.01
#   }
#   
for (j in 1:length(items)){
  # declare data as time series
  # Smoothen the time series data to improve the analysis of long term trends
  sm = lowess(x =  1961:2013, y = as.numeric(va_ratios$logratio[va_ratios$ItemC == items[j]]), f = sm_factor)
  # declare data as ts
  tmp.ts = ts(sm$y, start = c(1961), end =c(2013), frequency = 1)
  
  tmp.ts_fit = ets(tmp.ts, damped = T) # Allows maintaining a trend component
  # tmp.ts_fit = auto.arima(tmp.ts)
  # Forecast from 2019 to 2035
  # create an automatic variable name which is a fitted model of that industry
  # # assign(paste("ind_", j, "_proj", sep = ""), as.data.frame(forecast.ets(tmp.ts_fit, h = 16)))
  # assign(paste("ind_", j, "_proj", sep = ""), as.data.frame(forecast(tmp.ts_fit, h = 16)))
  # # Fitted data
  # assign(paste("ind_", j, "_fit", sep = ""), as.data.frame(fitted(tmp.ts_fit)))
  
  # save the point forecasts
  df[,j] =  as.data.frame(forecast(tmp.ts_fit, h = 47)$mean)
}

# Reverse transformation

exp_df = exp(df)
sigma_j =  rowSums(exp_df)
# remove the ratios
zj = exp(df)/ (1 + sigma_j)
zj["Oilseeds"] = 1- rowSums(zj)

# Check 
rowSums(zj)

zj$Year = 2014:2060

# zjL = melt( as.data.frame(lapply(zj, as.numeric)), id.var = "Year", value.name = "percentage")
zjL = melt( as.data.frame(zj), id.var = "Year", value.name = "percentage")

colnames(zjL) = c("Year", "ItemC","percentage")

# the column binding is sticking the two data frames together (they must have the same number of rows)

adval = rbind(as.data.frame(kcal_pct[,c("Year", "ItemC","percentage")]),
              zjL)

#plot
advalT_fig = ggplot(adval, aes(x=Year, y=percentage,group=ItemC, color = ItemC)) + 
  geom_line(size = 0.8) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=c(1961, 1980, 2000, 2020, 2040, 2060)) +
  scale_color_manual(name=" ", values = mycolors) +
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  ylab("composition of kcal/capita/day") +
  theme_cowplot() +
  labs(title = paste("Food supply composition",sep = " ") ) +
  theme(legend.position="bottom") +
  theme(legend.key.size = unit(0.6, "cm"))+ # legend box size
  theme(legend.text=element_text(size=rel(0.8))) #legend font size

ggplotly(advalT_fig)
# scale_color_manual(values = pal(20)) 

# save plots and dataframe for each smoothing factor
ggsave(paste("Food supply composition", ".tiff", sep = " "), plot = advalT_fig, 
       dpi = 400, width = 300, height = 200, units = "mm",
compression="lzw", type="cairo")

write.csv(adval, paste("Food supply composition", ".csv",sep = " "))
write.csv(zj, paste("Food supply composition projections 2014-2060 ", ".csv",sep = " "))



# -----------------------------------------------------------------------
# Combine compositional projections with Kcal projections 
# -----------------------------------------------------------------------

# Per capita, per year, kg consumption

lo95   = 365 * projs$`Lo 95` * zj[, 1:ncol(zj)-1] 
lo80   = 365 * projs$`Lo 80` * zj[, 1:ncol(zj)-1] 
trend  = 365 *projs$`Point Forecast` * zj[, 1:ncol(zj)-1] 
hi80   = 365 * projs$`Hi 80` * zj[, 1:ncol(zj)-1]
hi95   = 365 *projs$`Hi 95` * zj[, 1:ncol(zj)-1]

# kcals to grams
# Create conversion coefficients 
kcal2g = categories %>%
  group_by(ItemC) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) 

commodities = colnames(trend)

# Placeholder dataframes 
lo95.kgpc = lo95 
lo80.kgpc = lo80 
trend.kgpc = trend 
hi80.kgpc = hi80
hi95.kgpc = hi95

gr2kg = 1000 

for (commodity in commodities){
  lo95.kgpc[, commodity] = (lo95[, commodity] / 
                                as.numeric(kcal2g[kcal2g$ItemC == commodity, "kcals.per.gram"])) / gr2kg
  lo80.kgpc[, commodity] = (lo80[, commodity] / 
                                as.numeric(kcal2g[kcal2g$ItemC == commodity, "kcals.per.gram"])) /gr2kg
  trend.kgpc[, commodity] = (trend[, commodity] / 
                                 as.numeric(kcal2g[kcal2g$ItemC == commodity, "kcals.per.gram"]))/gr2kg
  hi80.kgpc[, commodity] = (hi80[, commodity] / 
                                as.numeric(kcal2g[kcal2g$ItemC == commodity, "kcals.per.gram"])) /gr2kg
  hi95.kgpc[, commodity] = (hi95[, commodity] / 
                                as.numeric(kcal2g[kcal2g$ItemC == commodity, "kcals.per.gram"])) /gr2kg
}

summary(trend.kgpc)


# Create per capita projection for trend and prediction intervals




# Historical values

# population

hist.pop = read.csv("Faostat AU population 1950-2018.csv")
hist.pop = hist.pop %>%
  filter(Element == "Total Population - Both sexes") %>%
  filter(Year >= 1961)%>%
  subset(select = c(Year, Value)) %>%
  group_by(Year) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
colnames(hist.pop) = c("Year", "pop1000")

# Consumption
hist.cons = kcal_pct[, c("Year", "ItemC", "Value", "percentage")]
hist.cons = merge(hist.cons, hist.pop, by = c("Year"))

# total consumption per day
kcal_day = read.csv("projected total kcal-capita-day.csv")%>%
  subset(select = c("year", "historical"))

hist.cons = merge(hist.cons, kcal_day, by.x = "Year", by.y = "year")
# Checked recovering back the Values column
hist.cons$hisXpct = hist.cons$historical * hist.cons$percentage

# Add information about kcal per gram

hist.cons = merge(hist.cons, kcal2g, by = "ItemC")

# Total historical consumption per category in kg per capita per year
hist.cons$hist.kgpc = (365 * 
                           hist.cons$historical *  
                           hist.cons$percentage /
                           hist.cons$kcals.per.gram / 
                           gr2kg)  

hist.cons$hist.protein = hist.cons$hist.kgpc * hist.cons$proteingrams.per.gram
hist.cons$hist.fat = hist.cons$hist.kgpc * hist.cons$fatgrams.per.gram

# Add trend projections

trend.kgpc$Year = 2014:2060
trend.kgpcL =melt( trend.kgpc, id.var = "Year", value.name = "hist.kgpc")

colnames(trend.kgpcL) = c("Year", "ItemC", "hist.kgpc")

trend.kgpcL = merge(trend.kgpcL, kcal2g, by = "ItemC")
trend.kgpcL$hist.protein = trend.kgpcL$hist.kgpc * trend.kgpcL$proteingrams.per.gram
trend.kgpcL$hist.fat = trend.kgpcL$hist.kgpc * trend.kgpcL$fatgrams.per.gram

indicators = c("Year", "ItemC", "hist.kgpc", "hist.protein", "hist.fat")
production = rbind(hist.cons[,indicators], trend.kgpcL[,indicators])
# production = merge(production, pop.hp, by = "Year", all.x = T)
production = merge(production, categories[, c("ItemC", "kcals.per.gram")], by = "ItemC", all.x = T)
production = production[order(production$ItemC,production$Year), ]
production$consumption.kcals = production$hist.kgpc * gr2ton * production$kcals.per.gram
colnames(production) = c("ItemC", "Year", "consumption.kgpc", "protein.tonnes", "fat.tonnes", 
                         "kcals.per.gram",  "consumption.kcals")

hp.kgpc <- ggplot(production, aes(x=Year, y = consumption.kgpc)) + 
  ylab("annual Kg per capita") +
  scale_x_continuous(breaks=c(1961, 1980, 2000, 2020, 2040, 2060)) +
  geom_line() + 
  facet_wrap( ~ ItemC, ncol=4, scales="free_y") +
  theme_cowplot()

# ggplotly(hp.kgpc)

ggsave("Historical and projected annual Kg pc.tiff" , 
       plot = hp.kgpc, 
       dpi = 400, width = 300, 
       height = 350, 
       units = "mm",
       compression="lzw", type="cairo")

write.csv(production, paste("historical and projected food supply annual kg pc.csv"), row.names = F)













# Compare trend projections with OECD-FAO meat projections

trends = read.csv("historical and projected food supply Medium.series .csv")






trends$consumption.kg = 1000 * trends$consumption.tonnes/trends$population


trends = trends %>%
  filter(ItemC == "Bovine Meat" |
           ItemC == "Pigmeat"  |
           ItemC == "Poultry Meat"   |
           ItemC == "Mutton & Goat Meat"  ) %>%
  filter(Year >= 1991)%>%
  subset(select = c(Year, ItemC, consumption.kg)) 

oecd.fao = read.csv("oecd-fao meat projection.csv") %>%
  filter(LOCATION == "AUS")%>%
  subset(select = c(SUBJECT, TIME, kgpc))


merge(trends, oecd.fao, by.x = "Year", by.y = "TIME", all.x = T)








&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




# Population projections
pop = read.csv("Projected population, Australia.csv")
all.scen = c("Low.series", "Medium.series", "High.series", "Zero.net.overseas.migration")



for (pop.scen in all.scen){
  # Annual kcal Projections = days in year *
  # total kcal per day *
  # percent consumption per category * 
  # population
  
  lo95   = 365 * projs$`Lo 95` * zj[, 1:ncol(zj)-1] * pop[1:47,pop.scen]
  lo80   = 365 * projs$`Lo 80` * zj[, 1:ncol(zj)-1] * pop[1:47,pop.scen]
  trend  = 365 *projs$`Point Forecast` * zj[, 1:ncol(zj)-1] * pop[1:47,pop.scen]
  hi80   = 365 * projs$`Hi 80` * zj[, 1:ncol(zj)-1] * pop[1:47,pop.scen]
  hi95   = 365 *projs$`Hi 95` * zj[, 1:ncol(zj)-1] * pop[1:47,pop.scen]
  
  
  # kcals to grams
  # Create conversion coefficients 
  
  kcal2g = categories %>%
    group_by(ItemC) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE) 
  
  commodities = colnames(trend)
  
  # Placeholder dataframes 
  lo95.tonnes = lo95 
  lo80.tonnes = lo80 
  trend.tonnes = trend 
  hi80.tonnes = hi80
  hi95.tonnes = hi95
  
  gr2ton = 1000000 
  
  for (commodity in commodities){
    lo95.tonnes[, commodity] = (lo95[, commodity] / 
                                  as.numeric(kcal2g[kcal2g$ItemC == commodity, "kcals.per.gram"])) / gr2ton
    lo80.tonnes[, commodity] = (lo80[, commodity] / 
                                  as.numeric(kcal2g[kcal2g$ItemC == commodity, "kcals.per.gram"])) /gr2ton
    trend.tonnes[, commodity] = (trend[, commodity] / 
                                   as.numeric(kcal2g[kcal2g$ItemC == commodity, "kcals.per.gram"]))/gr2ton
    hi80.tonnes[, commodity] = (hi80[, commodity] / 
                                  as.numeric(kcal2g[kcal2g$ItemC == commodity, "kcals.per.gram"])) /gr2ton
    hi95.tonnes[, commodity] = (hi95[, commodity] / 
                                  as.numeric(kcal2g[kcal2g$ItemC == commodity, "kcals.per.gram"])) /gr2ton
  }
  
  summary(trend.tonnes)
  
  # Historical values
  # population
  
  hist.pop = read.csv("Faostat AU population 1950-2018.csv")
  hist.pop = hist.pop %>%
    filter(Element == "Total Population - Both sexes") %>%
    filter(Year >= 1961)%>%
    subset(select = c(Year, Value)) %>%
    group_by(Year) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  colnames(hist.pop) = c("Year", "pop1000")
  
  # join historical (1961-2018) and projected (2019 - 1960) population data
  pop.hp = as.data.frame(1961:2060)
  colnames(pop.hp) = c("Year")
  pop.hp$population = 0
  pop.hp$population[1:58] = hist.pop$pop1000 *1000
  pop.hp$population[59:100] = pop[(pop$Year >=2019 & pop$Year <=2060 ),c(pop.scen)]
  
  
  # Consumption
  hist.cons = kcal_pct[, c("Year", "ItemC", "Value", "percentage")]
  hist.cons = merge(hist.cons, hist.pop, by = c("Year"))
  
  # total consumption per day
  kcal_day = read.csv("projected total kcal-capita-day.csv")%>%
    subset(select = c("year", "historical"))
  
  hist.cons = merge(hist.cons, kcal_day, by.x = "Year", by.y = "year")
  # Checked recovering back the Values column
  hist.cons$hisXpct = hist.cons$historical * hist.cons$percentage
  
  # Add information about kcal per gram
  
  hist.cons = merge(hist.cons, kcal2g, by = "ItemC")
  
  # Total historical consumption per category
  hist.cons$hist.tonnes = (365 * 
                             hist.cons$historical *  
                             hist.cons$percentage *
                             hist.cons$pop1000 * 1000/
                             hist.cons$kcals.per.gram / 
                             gr2ton)  
  
  hist.cons$hist.protein = hist.cons$hist.tonnes * hist.cons$proteingrams.per.gram
  hist.cons$hist.fat = hist.cons$hist.tonnes * hist.cons$fatgrams.per.gram
  
  # Add trend projections
  
  trend.tonnes$Year = 2014:2060
  trend.tonnesL =melt( trend.tonnes, id.var = "Year", value.name = "hist.tonnes")
  
  colnames(trend.tonnesL) = c("Year", "ItemC", "hist.tonnes")
  
  trend.tonnesL = merge(trend.tonnesL, kcal2g, by = "ItemC")
  trend.tonnesL$hist.protein = trend.tonnesL$hist.tonnes * trend.tonnesL$proteingrams.per.gram
  trend.tonnesL$hist.fat = trend.tonnesL$hist.tonnes * trend.tonnesL$fatgrams.per.gram
  
  indicators = c("Year", "ItemC", "hist.tonnes", "hist.protein", "hist.fat")
  production = rbind(hist.cons[,indicators], trend.tonnesL[,indicators])
  production = merge(production, pop.hp, by = "Year", all.x = T)
  production = merge(production, categories[, c("ItemC", "kcals.per.gram")], by = "ItemC", all.x = T)
  production = production[order(production$ItemC,production$Year), ]
  production$consumption.kcals = production$hist.tonnes * gr2ton * production$kcals.per.gram
  colnames(production) = c("ItemC", "Year", "consumption.tonnes", "protein.tonnes", "fat.tonnes", "population",
                           "kcals.per.gram",  "consumption.kcals")
  
  hp.tonnes <- ggplot(production, aes(x=Year, y = consumption.tonnes/1000)) + 
    ylab("1000 Tonnes") +
    scale_x_continuous(breaks=c(1961, 1980, 2000, 2020, 2040, 2060)) +
    geom_line() + 
    facet_wrap( ~ ItemC, ncol=4, scales="free_y") +
    theme_cowplot()
  
  # ggplotly(hp.tonnes)
  
  ggsave(paste("Historical data and projections, tonnes,", pop.scen, "population.tiff", sep = " ") , plot = hp.tonnes, 
         dpi = 400, width = 300, height = 350, units = "mm",
         compression="lzw", type="cairo")
  
  write.csv(production, paste("historical and projected food supply", pop.scen, ".csv"), row.names = F)
}












