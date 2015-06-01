medals <- read.table("~/PUC-MSc/datascience/olympics/dataset/medals.tsv",
                     header = T,
                     sep = "\t",
                     fill = TRUE,
                     stringsAsFactors = FALSE)

hosts <- c("Spain", "United.States", "Australia", "Greece", "China", "Great.Britain")
years <- c(1992, 1996, 2000, 2004, 2008, 2012)

# remove last row
medals <- medals[ -nrow(medals), ]
# replace NA by 0
medals[is.na(medals)] <- 0

last_n_games <- length(hosts)

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

for(i in(1:last_n_games)) {
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

# replacing "." by " " (original dataset comes with ".")
colnames(total_medals) <- gsub("[.]", " ", hosts)
colnames(mean_away_factor) <- gsub("[.]", " ", hosts)
colnames(home_factor) <- gsub("[.]", " ", hosts)

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
pred_bra_gold <- round(bra_gold_perc*pred_bra_score)
pred_bra_silver <- round(bra_silver_perc*pred_bra_score)
pred_bra_bronze <- round(bra_bronze_perc*pred_bra_score)

pred_bra <- cbind(pred_bra_gold, pred_bra_silver, pred_bra_bronze)