jforny <- "~/rstudio/OlympicsPrediction/"
dgribel <- "~/PUC-MSc/datascience/olympics/"
path <- dgribel
source_medals <- paste(path, "dataset/medals.tsv", sep = "")

medals <- read.table(source_medals, header = T, sep = "\t", fill = TRUE, stringsAsFactors = FALSE)

# remove last row
medals <- medals[ -nrow(medals), ]
# replace NA by 0
medals[is.na(medals)] <- 0

hosts <- c("Spain", "United.States", "Australia", "Greece", "China", "Great.Britain")
hosts_acronym <- c("SPA", "USA", "AUS", "GRE", "CHI", "GBR")
years <- c()

last_n_games <- 6
j <- 1
for(i in (last_n_games-1):0) {
  years[j] <- 2012 - 4*i
  j <- j+1
}

# define colors for total medals chart
total_colors <- rep("#ADCFFF", times = last_n_games*last_n_games)
j <- 1
for (i in 1:last_n_games-1) {
   total_colors[j] <- "#388BFF"
   j <- j+last_n_games+1
}

home_factor <- c()
mean_away_factor <- c()
total_medals <- c()

for(i in(1:length(hosts))) {
  k <- hosts[i]
  host_pos <- which(medals$Year==years[i])
  host_pos_tail <- last_n_games - (nrow(medals)-host_pos)
  
  key_gold <- paste(k, "Gold", sep="..")
  key_silver <- paste(k, "Silver", sep="..")
  key_bronze <- paste(k, "Bronze", sep="..")
  
  mean_away_factor <- cbind(mean_away_factor, mean(3*tail(medals[[key_gold]], last_n_games)[-host_pos_tail] +
                                  2*tail(medals[[key_silver]], last_n_games)[-host_pos_tail] +
                                  tail(medals[[key_bronze]], last_n_games)[-host_pos_tail]))
  
  home_factor <- cbind(home_factor, 3*medals[[key_gold]][host_pos] +
    2*medals[[key_silver]][host_pos] +
    medals[[key_bronze]][host_pos])
  
  total_medals <- cbind(total_medals, tail(medals[[key_gold]], last_n_games) + 
                                         tail(medals[[key_silver]], last_n_games) + 
                                         tail(medals[[key_bronze]], last_n_games))
}

y_lim_total <- c(0, 1.2*max(total_medals))
y_lim_score <- c(0, 1.2*max(home_factor))

bra_score <- 3*tail(medals$Brazil..Gold, last_n_games) +
  2*tail(medals$Brazil..Silver, last_n_games) +
  tail(medals$Brazil..Bronze, last_n_games)

bra_gold_totals <- sum(tail(medals$Brazil..Gold, last_n_games))
bra_silver_totals <- sum(tail(medals$Brazil..Silver, last_n_games))
bra_bronze_totals <- sum(tail(medals$Brazil..Bronze, last_n_games))

sum_bra_medals <- bra_gold_totals + bra_silver_totals + bra_bronze_totals

bra_gold_perc <- bra_gold_totals/sum_bra_medals
bra_silver_perc <- bra_silver_totals/sum_bra_medals
bra_bronze_perc <- bra_bronze_totals/sum_bra_medals

# ratio: mean between home factor and mean away factor
r <- mean(home_factor/mean_away_factor)

# predicting BRA score
pred_bra_score <- mean(bra_score) * r

# predicting BRA medals distribution
p <- pred_bra_score/3 + pred_bra_score/6 + pred_bra_score/9
pred_bra_gold <- round(bra_gold_perc*p)
pred_bra_silver <- round(bra_silver_perc*p)
pred_bra_bronze <- round(bra_bronze_perc*p)

pred_bra <- cbind(pred_bra_gold, pred_bra_silver, pred_bra_bronze)

# medals ratio, year by year (1992--2012)
country_medals_ratio <- function(country) {
  y <- years[1]
  medals_ratio <- c()
  for(i in(1:length(years))) {
    medals_ratio <- rbind(medals_ratio,
                    (3*medals[[paste(country, "Gold", sep="..")]][which(medals$Year==y)]+
                       2*medals[[paste(country, "Silver", sep="..")]][which(medals$Year==y)]+
                       medals[[paste(country, "Bronze", sep="..")]][which(medals$Year==y)])/
                    (3*medals[[paste(country, "Gold", sep="..")]][which(medals$Year==y-4)]+
                       2*medals[[paste(country, "Silver", sep="..")]][which(medals$Year==y-4)]+
                       medals[[paste(country, "Bronze", sep="..")]][which(medals$Year==y-4)]))
    y <- y+4
  }
  rownames(medals_ratio) <- years
  return(medals_ratio)
}

# loading indicator script
cor_gdp <- c()
source(paste(path, "src/indicator.R", sep = ""))
more_countries <- c("Spain", "United.States", "Australia", "Greece", "China", "Great.Britain",
                    "France", "Canada", "Italy", "Japan", "Sweden", "Brazil", "Norway", "Finland",
                    "Netherlands", "Switzerland", "Austria", "Romania", "Bulgaria", "Denmark", "Belgium")

# medals x gdp correlation for hosts -- removing year which country was the host (host factor influences)
groupeddata <- data.frame("Country" = character(), "Year" = character(),  "GGF" = character(), "MGR" = character())
for(c in more_countries) {
  if(c %in% hosts) {
    pos <- match(c, hosts)
    cor_gdp <- cbind(cor_gdp, cor(country_medals_ratio(c)[-pos], ind_factor[c,][-pos]))
  } else {
    cor_gdp <- cbind(cor_gdp, cor(country_medals_ratio(c), ind_factor[c,]))
  }
  #Grouping data
  for(year in years) {
    groupeddata <- as.data.frame(rbind(as.matrix(groupeddata), c(c, year, ind_factor[c,toString(year)], country_medals_ratio(c)[toString(year),])))
  }
}

colnames(cor_gdp) <- more_countries

#Removing hosts data
groupeddata <- groupeddata[order(groupeddata$Year),]
for(i in (1:length(hosts))) {
   groupeddata <- groupeddata[!(groupeddata$Country == hosts[i] & groupeddata$Year == years[i]),]
}

#Prediction for Great.Britain
gbindex <- which(hosts=="Great.Britain")
britaindata <- groupeddata[!(groupeddata$Country != hosts[gbindex]),]

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

britaindata$MGR <- as.numeric.factor(britaindata$MGR)
britaindata$GGF <- as.numeric.factor(britaindata$GGF)

str(britaindata)

plot(britaindata$MGR, britaindata$GGF)
britainmodel <- lm(britaindata$MGR ~ britaindata$GGF)
summary(britainmodel)

# predictionModel <- predict(britainmodel)
# plot(predictionModel)
# predictionModel