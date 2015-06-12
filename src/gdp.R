jforny <- "~/rstudio/OlympicsPrediction/"
dgribel <- "~/PUC-MSc/datascience/olympics/"
path <- dgribel
source_ gdp <- paste(path, "dataset/ny.gdp.mktp.kd.zg_Indicator_en_csv_v2/ny.gdp.mktp.kd.zg_Indicator_en_csv_v2.csv", sep = "")
gdp <- read.table(source_ gdp,
                  header = T,
                  skip = 1,
                  sep = ",",
                  fill = TRUE,
                  stringsAsFactors = FALSE)

# hosts <- c("Spain", "United.States", "Australia", "Greece", "China", "United Kingdom")
# metadata <- gdp[, head(names(gdp), 4)]
# observations <- gdp[, !names(gdp) %in% head(names(gdp), 4)]
# Data cleaning based on hosts and years
# gdp[gdp$Country.Name %in% hosts, names(gdp) %in% c("Country.Name", years)]

years <- c("X1992", "X1996", "X2000", "X2004", "X2008", "X2012")
nrows = nrow(gdp)
ggfconsolidated = data.frame(yearcountry = character(), country = character(), year4 = numeric(), year3 = numeric(), year2 = numeric(), 
                             year1 = numeric(), average = numeric(), globalaverage = numeric(), ggf = numeric())
for(year in years) {
  worldgdpmean = mean(gdp[,year], na.rm=TRUE)
  
  for(i in 1:nrows) {
    yearpos = which(colnames(gdp)==year)
    pastgdpmean = rowMeans(gdp[i, (yearpos - 4):(yearpos - 1)])
    ggf = pastgdpmean/worldgdpmean
    country = gdp[i, 1]
    yearcountry = paste(year, country, sep='_')
    # tuple = c(yearcountry, country, gdp[i, yearpos-4], gdp[i, yearpos-3], gdp[i, yearpos-2], gdp[i, yearpos-1], pastgdpmean, worldgdpmean, ggf)
    ggfconsolidated <- as.data.frame(rbind(as.matrix(ggfconsolidated),  c(yearcountry, country, gdp[i, yearpos-4], gdp[i, yearpos-3], gdp[i, yearpos-2], gdp[i, yearpos-1], pastgdpmean, worldgdpmean, ggf)))    
    # ggfconsolidated = rbind(ggfconsolidated, tuple)
    print(tuple)
  }
}

#write.csv(ggfconsolidated, file = "MyData.csv")
# print(ggfconsolidated)
