
# SSP1
Xssps = read.csv("data.ssp1.csv", stringsAsFactors = F)
# Subset the original dataset to match the order of the coefficients
f.Xssps = subset(Xssps, select = c("year", "country.code",  "Population.IIASA.WiC")) 

f.Xssps = filter(f.Xssps, (year==2010) | (year==2015) | (year==2020) |
                   (year==2025) | (year==2030) | (year==2035) |
                   (year==2040) | (year==2045) | (year==2050) )

f.Xssps.AU = f.Xssps[f.Xssps$country.code == "AUS",]

# Interpolate estimates to annual time steps.
## Function to interpolate at constant rate for each interval
interp <- function(yrs, values) {
  tt <- diff(yrs)               # interval lengths
  N <- head(values, -1L)     
  P <- tail(values, -1L)
  r <- (log(P) - log(N)) / tt   # rate for interval
  const_rate <- function(N, r, time) N*exp(r*(0:(time-1L)))
  list(year=seq.int(min(yrs), max(yrs), by=1L),
       value=c(unlist(Map(const_rate, N, r, tt)), tail(P, 1L)))
}


f.Xssps.AU.SSP1 =  interp(f.Xssps.AU$year, f.Xssps.AU$Population.IIASA.WiC)


# SSP2
Xssps = read.csv("data.ssp2.csv", stringsAsFactors = F)
# Subset the original dataset to match the order of the coefficients
f.Xssps = subset(Xssps, select = c("year", "country.code",  "Population.IIASA.WiC")) 

f.Xssps = filter(f.Xssps, (year==2010) | (year==2015) | (year==2020) |
                   (year==2025) | (year==2030) | (year==2035) |
                   (year==2040) | (year==2045) | (year==2050) )

f.Xssps.AU = f.Xssps[f.Xssps$country.code == "AUS",]
f.Xssps.AU.SSP2 =  interp(f.Xssps.AU$year, f.Xssps.AU$Population.IIASA.WiC)


# SSP3
Xssps = read.csv("data.ssp3.csv", stringsAsFactors = F)
# Subset the original dataset to match the order of the coefficients
f.Xssps = subset(Xssps, select = c("year", "country.code",  "Population.IIASA.WiC")) 

f.Xssps = filter(f.Xssps, (year==2010) | (year==2015) | (year==2020) |
                   (year==2025) | (year==2030) | (year==2035) |
                   (year==2040) | (year==2045) | (year==2050) )

f.Xssps.AU = f.Xssps[f.Xssps$country.code == "AUS",]
f.Xssps.AU.SSP3 =  interp(f.Xssps.AU$year, f.Xssps.AU$Population.IIASA.WiC)


au.population.ssps = as.data.frame(f.Xssps.AU.SSP1)
colnames(au.population.ssps) = c("year", "SSP1")

au.population.ssps$SSP2 = f.Xssps.AU.SSP2$value
au.population.ssps$SSP3 = f.Xssps.AU.SSP3$value

write.csv(au.population.ssps, "au.population.ssps.csv")

