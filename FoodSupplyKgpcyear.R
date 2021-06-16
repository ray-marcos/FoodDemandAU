# Australian food demand model

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

# setwd("H:/Dropbox/Australia/GISERA/SA conventional gas project/projections/")
# setwd("C:/Users/mar77v/Dropbox/Australia/GISERA/SA conventional gas project/projections/")

# setwd("C:/Users/mar77v/Deakin University/LUF Modelling - Trade and Food Demand/domestic_demand/data/")
# setwd("F:/Deakin University/LUF Modelling - Trade and Food Demand/domestic_demand/data/")
# setwd("C:/Users/mar77v/OneDrive - CSIRO/github/FoodDemandAU/")

# -------------------------------------------------------------------------------------------------------
# Load input data
# Food supply primary commodities
# fs.data = read.csv("FAOSTAT Food supply Crops, Livestock and Fish kcal per day.csv", stringsAsFactors = F)
fs.data = read.csv("FAOSTAT_Food supply Crops, Livestock and Fish -- Primary Equivalents.csv", stringsAsFactors = F)
# Food balance data (it has info of kg/pc /year)
# fb.data = read.csv("FAOSTAT_Food Balances old method.csv", stringsAsFactors = F)
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
# fb.data = fb.data[!fb.data$Item %in% duplicates,]
# unique(fs.data$Element)
# commodities = unique(fs$Item[fs$Element == "Food supply (kcal/capita/day)"])
# write.csv(commodities, "categories food demand.csv")

# Add categories relevant to our analysis
categories = read.csv("categories food demand.csv")
categories$Considerations = NULL

fs = merge(fs.data, categories, by = "Item", all.x = T, all.y = F)

# Get Kg pc per year from aggreagated  FAOStat data
# fb = merge(fb.data, categories, by = "Item", all.x = T, all.y = F)
kg_pc_py =  fs%>%
  filter(Element == "Food supply quantity (kg/capita/yr)") %>%
  group_by(Year, ItemC ) %>%
  subset(select=c("Year", "ItemC", "Value")) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
colnames(kg_pc_py) = c("Year", "ItemC", "Kgpcpy")

# # # # Obtain Kcal per kg from FAOSTAT data
# # kcal per capita per day
# kcal_pc_py =  fs%>%
#   filter(Element == "Food supply (kcal/capita/day)") %>%
#   group_by(Year, ItemC ) %>%
#   subset(select=c("Year", "ItemC", "Value")) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE)
# colnames(kcal_pc_py) = c("Year", "ItemC", "kcalpcpy")
# kcal_pc_py$kcalpcpy = kcal_pc_py$kcalpcpy * 365
# 
# # # # # Merge to get estimate of kcal per gram. 
# #  
# kcal.per.kg = merge(kcal_pc_py,kg_pc_py,  by = c("ItemC", "Year"), all.x = T)
# kcal.per.kg$kcal.per.kg = kcal.per.kg$kcalpcpy/kcal.per.kg$Kgpcpy
# # kcal.per.kg[kcal.per.kg$Item == "Miscellaneous", "kcal.per.kg"] = 2357
# 
# kcal.per.kg.mean = kcal.per.kg%>%
#   group_by(ItemC ) %>%
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

# ggplotly(fs.luf.pa)

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

# ggplotly(fs.luf.pl)


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

# ggplotly(advalT_fig)
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

# Total per capita, per year, kg consumption

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



# Create per kg per capita projection for trend and prediction intervals

# Historical values

# Consumption
hist.cons = kcal_pct[, c("Year", "ItemC", "Value", "percentage")]

# total consumption per day
kcal_day = read.csv("projected total kcal-capita-day.csv")%>%
  subset(select = c("year", "historical"))

hist.cons = merge(hist.cons, kcal_day, by.x = "Year", by.y = "year")
# Checked recovering back the Values column
hist.cons$hisXpct = hist.cons$historical * hist.cons$percentage

# Add information about kcal per gram

hist.cons = merge(hist.cons, kcal2g, by = "ItemC")

# Total historical consumption per category in kg per capita per year
hist.cons$kgpc = (365 * 
                           hist.cons$historical *  
                           hist.cons$percentage /
                           hist.cons$kcals.per.gram / 
                           gr2kg)  

hist.cons$protein.kgpc = hist.cons$kgpc * hist.cons$proteingrams.per.gram
hist.cons$fat.kgpc = hist.cons$kgpc * hist.cons$fatgrams.per.gram

# Add projections

fs.fun <- function(proj.kgpc){
  proj.kgpc$Year = 2014:2060
  proj.kgpcL = pivot_longer(proj.kgpc, cols = !Year, names_to = c("ItemC"), values_to = "kgpc")
  colnames(proj.kgpcL) = c("Year", "ItemC", "kgpc")
  proj.kgpcL = merge(proj.kgpcL, kcal2g, by = "ItemC")
  proj.kgpcL$protein.kgpc = proj.kgpcL$kgpc* proj.kgpcL$proteingrams.per.gram
  proj.kgpcL$fat.kgpc = proj.kgpcL$kgpc * proj.kgpcL$fatgrams.per.gram
  indicators = c("Year", "ItemC", "kgpc", "protein.kgpc", "fat.kgpc")
  food.supply = rbind(hist.cons[,indicators], proj.kgpcL[,indicators])
  food.supply = merge(food.supply, kcal2g[, c("ItemC", "kcals.per.gram")], by = "ItemC", all.x = T)
  food.supply = food.supply[order(food.supply$ItemC,food.supply$Year), ]
  food.supply$kcals.pc = food.supply$kgpc * 1000 * food.supply$kcals.per.gram
  colnames(food.supply) = c("ItemC", "Year", "consumption.kgpc", "protein.tonnes", "fat.tonnes", 
                            "kcals.per.gram",  "consumption.kcals")
  food.supply
}

# Estimate per capita annual supply for the projected trend and bounds

fs.lo95.kgpc = fs.fun(lo95.kgpc)
fs.lo80.kgpc = fs.fun(lo80.kgpc)
fs.trend.kgpc = fs.fun(trend.kgpc)
fs.hi80.kgpc = fs.fun(hi80.kgpc)
fs.hi95.kgpc = fs.fun(hi95.kgpc)

# create data only for kg per capita

fs.kgpc = remove.factors(fs.lo95.kgpc[, c("ItemC", "Year", "consumption.kgpc" )])
fs.kgpc = left_join(remove.factors(fs.kgpc), 
                    remove.factors(fs.lo80.kgpc[, c("ItemC", "Year", "consumption.kgpc" )]), 
                    by = c("ItemC", "Year"))
fs.kgpc = left_join(fs.kgpc, 
                    as.data.frame(fs.trend.kgpc[, c("ItemC", "Year", "consumption.kgpc" )]), by = c("ItemC", "Year"))
colnames(fs.kgpc) = c("ItemC", "Year", "lo95.kgpc", "lo80.kgpc", "trend.kgpc")
fs.kgpc = left_join(fs.kgpc, fs.hi80.kgpc[, c("ItemC", "Year", "consumption.kgpc" )], by = c("ItemC", "Year"))
fs.kgpc = left_join(fs.kgpc, fs.hi95.kgpc[, c("ItemC", "Year", "consumption.kgpc" )], by = c("ItemC", "Year"))
colnames(fs.kgpc) = c("ItemC", "Year", "lo95.kgpc", "lo80.kgpc", "trend.kgpc", "hi80.kgpc", "hi95.kgpc")
# add historical data
fs.kgpc = left_join(fs.kgpc, kg_pc_py, by = c("ItemC", "Year"))

hp.kgpc <- ggplot(fs.kgpc, aes(x=Year, y = trend.kgpc)) + 
  ylab("annual Kg per capita") +
  scale_x_continuous(breaks=c(1961, 1980, 2000, 2020, 2040, 2060)) +
  geom_ribbon(aes(ymin=lo95.kgpc, ymax=hi95.kgpc), fill = "grey70", alpha = 0.8) +
  geom_ribbon(aes(ymin=lo80.kgpc, ymax=hi80.kgpc), fill = "black", alpha = 0.2) +
  geom_line() + 
  geom_line(aes(y = Kgpcpy), color = "red")+
  facet_wrap( ~ ItemC, ncol=4, scales="free_y") +
  theme_cowplot()


# ggplotly(hp.kgpc)

ggsave("Annual supply Kg pc 95-80pct pred interval.tiff" , 
       plot = hp.kgpc, 
       dpi = 400, width = 300, 
       height = 350, 
       units = "mm",
       compression="lzw", type="cairo")

write.csv(fs.kgpc, paste("food supply annual kg pc trend and bounds 1961-2060.csv"), row.names = F)

# Save dataframes with protein and fat information
write.csv(fs.lo95.kgpc, "food supply lo95 annual kg pc.csv")
write.csv(fs.lo80.kgpc, "food supply lo80 annual kg pc.csv")
write.csv(fs.trend.kgpc, "food supply trend annual kg pc.csv")
write.csv(fs.hi80.kgpc, "food supply hi80 annual kg pc.csv")
write.csv(fs.hi95.kgpc, "food supply hi95 annual kg pc.csv")


# Compare trend projections with OECD-FAO meat projections

# # This graph compares projections in commodity equivalents with OECD projections in retail weights
# 
# luf =  fs.kgpc %>%
#   filter(ItemC == "Bovine Meat" |
#            ItemC == "Pigmeat"  |
#            ItemC == "Poultry Meat"   |
#            ItemC == "Mutton & Goat Meat"  ) %>%
#   filter(Year >= 1991)
# 
# oecd.fao = read.csv("oecd-fao meat projection.csv", stringsAsFactors = F) %>%
#   filter(LOCATION == "AUS" &
#            MEASURE == "KG_CAP")%>%
#   subset(select = c(SUBJECT, TIME, kgpc))
# 
# 
# luf.oecd.fao = merge(luf, oecd.fao, by.x = c("Year", "ItemC"), by.y = c("TIME", "SUBJECT"), all.x = T)
# 
# luf.oecd.plot <- ggplot(luf.oecd.fao, aes(x=Year, y = trend.kgpc)) + 
#   ylab("annual Kg per capita") +
#   scale_x_continuous(breaks=c(1961, 1980, 2000, 2020, 2040, 2060)) +
#   geom_ribbon(aes(ymin=lo95.kgpc, ymax=hi95.kgpc), fill = "grey70", alpha = 0.8) +
#   geom_ribbon(aes(ymin=lo80.kgpc, ymax=hi80.kgpc), fill = "black", alpha = 0.2) +
#   geom_line() + 
#   geom_line(aes(y = kgpc), color = "red") +
#   facet_wrap( ~ ItemC, ncol=4) +
#   theme_cowplot()
# luf.oecd.plot
# 
# ggsave("Annual supply Kg pc LUF vs OECD.FAOSTAT.tiff" , 
#        plot = hp.kgpc, 
#        dpi = 400, width = 300, 
#        height = 350, 
#        units = "mm",
#        compression="lzw", type="cairo")
# 
