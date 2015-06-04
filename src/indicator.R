# edit this source to change indicator dataset
source_file <- "~/PUC-MSc/datascience/olympics/dataset/gdp_growth.csv"

gdp <- read.csv(source_file, header = T, sep = ",", fill = TRUE, stringsAsFactors = FALSE)

# removing unnecessary columns
gdp <- subset(gdp, select = -Series.Name)
gdp <- subset(gdp, select = -Series.Code)
gdp <- subset(gdp, select = -Time.Code)

# removing 1960 -- everything is blank
gdp <- gdp[-1, ]
# removing last rows -- blank or not important
gdp <- head(gdp, -6)

world_mean_gdp <- c()
bra_gdp <- c()
years <- c()

last_n_games <- 6
j <- 1
for(i in (last_n_games-1):0) {
  years[j] <- 2012 - 4*i
  j <- j+1
}

for(j in(1:length(years))) {
  for(i in (2:length(colnames(gdp)))) {
    country_key <- colnames(gdp)[i]
  }
  world_mean_gdp <- rbind(world_mean_gdp,
                    rowMeans(gdp[which(gdp$Time >= (years[j]-3) & gdp$Time <= years[j]),][-c(1, which(colnames(gdp)=="Brazil..BRA."))],
                    na.rm=TRUE))
}

for(j in(1:length(years))) {
  bra_gdp <- rbind(bra_gdp,
              rowMeans(gdp[which(gdp$Time >= (years[j]-3) & gdp$Time <= years[j]),][which(colnames(gdp)=="Brazil..BRA.")],
              na.rm=TRUE))
}

rownames(world_mean_gdp) <- years
rownames(bra_gdp) <- years
colnames(world_mean_gdp) <- c(1, 2, 3, 4)
colnames(bra_gdp) <- c(1, 2, 3, 4)