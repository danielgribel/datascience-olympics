# edit this source to change indicator dataset
source_gdp_growth <- "~/PUC-MSc/datascience/olympics/dataset/gdp_growth.csv"

ind <- read.csv(source_gdp_growth, header = T, sep = ",", fill = TRUE, stringsAsFactors = FALSE)

# removing unnecessary columns
ind <- subset(ind, select = -Series.Name)
ind <- subset(ind, select = -Series.Code)
ind <- subset(ind, select = -Time.Code)

# removing 1960 -- everything is blank
ind <- ind[-1, ]
# removing last rows -- blank or not important
ind <- head(ind, -6)

years <- c()

# filling years with last K games
last_n_games <- 6
j <- 1
for(i in (last_n_games-1):0) {
  years[j] <- 2012 - 4*i
  j <- j+1
}

# renaming country names
colnames(ind)[-1] <- gsub("\\.\\.\\.", ".", colnames(ind)[-1])
colnames(ind)[-1] <- gsub("\\.\\.", ".", colnames(ind)[-1])
colnames(ind)[-1] <- substr(colnames(ind)[-1], 1, nchar(colnames(ind)[-1])-5)
colnames(ind)[which(colnames(ind)=="United.Kingdom")] <- "Great.Britain"

# calculate world mean indicator (except for country in question) considering past 4 years for each year in period
world_ind_growth <- function(years, country) {
  world_mean_ind <- c()
  for(j in(1:last_n_games)) {
    world_mean_ind <- rbind(world_mean_ind,
                            rowMeans(ind[which(ind$Time >= (years[j]-4) & ind$Time <= years[j]-1),][-c(1, which(colnames(ind)==country))],
                                     na.rm=TRUE))
  }
  return(world_mean_ind)
}

# calculate indicator for a country considering past 4 years for each year in period
country_ind_growth <- function(years, country) {
  country_ind <- c()
  for(i in(1:last_n_games)) {
    country_ind <- rbind(country_ind,
                   rowMeans(ind[which(ind$Time >= (years[i]-4) & ind$Time <= years[i]-1),][which(colnames(ind)==country)],
                            na.rm=TRUE))
  }
  return(country_ind)
}

# calculate indicator factor -- ratio between country mean indicator and global mean for each year in period
ind_factor <- c()
for(ct in colnames(ind)[-1]) {
  ind_factor <- rbind(ind_factor, rowMeans(country_ind_growth(years, ct))/rowMeans(world_ind_growth(years, ct)))
}
colnames(ind_factor) <- years
rownames(ind_factor) <- colnames(ind)[-1]