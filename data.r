library("WDI")
library("ggplot2")

# Search series identifier
WDIsearch(string = "research", field = "name", short = TRUE,
          cache = NULL)

# country = c("SG", "TH", "ID", "PH", "MY")
country = c("ID", "MY", "PH", "SG", "TH", "VN")
N = length(country)

WDI(country = country, 
    indicator = c("RDex"="IP.PAT.RESD"), 
    start = NULL, 
    end = NULL, 
    extra = FALSE, 
    cache = NULL)

# Download number of patents from WDI
patent <- WDI(country = country, 
                  indicator = c("pat_resd"="IP.PAT.RESD"), 
                  start = NULL, 
                  end = NULL, 
                  extra = FALSE, 
                  cache = NULL)

patent.nres <- WDI(country = country, 
                   indicator = c("pat_resd"="IP.PAT.NRES"), 
                   start = NULL, 
                   end = NULL, 
                   extra = FALSE, 
                   cache = NULL)

# Fill missing value for Indonesia
patent[patent$country=="Indonesia" & patent$year==2012, ]$pat_resd = 
  (patent[patent$country=="Indonesia" & patent$year==2011, ]$pat_resd 
   + patent[patent$country=="Indonesia" & patent$year==2013, ]$pat_resd)/2

# Download nominal GDP data from WDI
gdp <- WDI(country = country, 
              indicator = c("ngdp"="NY.GDP.MKTP.CD"), 
              start = NULL, 
              end = NULL, 
              extra = FALSE, 
              cache = NULL)

gdp.wide <- reshape(gdp[c("country", "ngdp", "year")], idvar = "year", timevar = "country", direction = "wide")

patent.wide = reshape(patent[c("country", "pat_resd", "year")], idvar = "year", timevar = "country", direction = "wide")

gdp.sum = data.frame(year=gdp.wide[,1], total=rowSums(gdp.wide[,2:(N+1)]))

weight.wide = data.frame(year=gdp.wide[,1], gdp.wide[,2:(N+1)]/rowSums(gdp.wide[,2:(N+1)], na.rm = TRUE))

weighted.pat = data.frame(year=patent.wide[,1],pat.avg=rowSums(weight.wide[,2:(N+1)]*patent.wide[,2:(N+1)], na.rm = TRUE))

techcomp = data.frame(patent[c("iso2c","country","year")],index=patent$pat_resd/rep(weighted.pat$pat.avg,N))

plot = ggplot(techcomp[techcomp$year>1994,], 
              aes(x=year, y=index, linetype=country, color=country)) + 
  geom_line() + xlab("Year") + ylab("Technological Competitiveness Index") +
  theme_bw() + theme(legend.title = element_blank())

print(plot)

ggsave("techcomp.png", w=6, h=4)

write.csv(patent.wide, "patent.csv")
write.csv(weight.wide, "weight.csv")
