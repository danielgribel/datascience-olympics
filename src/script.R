host_year <- new.env()
host_year$'Spain' <- 1992
host_year$'United.States' <- 1996
host_year$'Australia' <- 2000
host_year$'Greece' <- 2004
host_year$'China' <- 2008
host_year$'Great.Britain' <- 2012

medals <- read.table("~/PUC-MSc/datascience/olympics/dataset/medals.tsv",
                     header = T,
                     sep = "\t",
                     fill = TRUE,
                     stringsAsFactors = FALSE)
# remove last row
medals <- medals[ -nrow(medals), ]
# replace NA by 0
medals[is.na(medals)] <- 0

interval <- length(host_year)
mean_away_factor <- new.env()
home_factor <- new.env()
country <- ls(host_year)

for(k in country) {
  host_pos <- which(medals$Year==host_year[[k]])
  host_pos_tail <- (-1)*(interval - (nrow(medals)-host_pos))
  key_gold <- paste(k, "Gold", sep="..")
  key_silver <- paste(k, "Silver", sep="..")
  key_bronze <- paste(k, "Bronze", sep="..")
  
  mean_away_factor[[k]] <- mean(3*tail(medals[[key_gold]], interval)[host_pos_tail] +
                                  2*tail(medals[[key_silver]], interval)[host_pos_tail] +
                                  tail(medals[[key_bronze]], interval)[host_pos_tail])
  
  home_factor[[k]] <- 3*medals[[key_gold]][host_pos] +
    2*medals[[key_silver]][host_pos] +
    medals[[key_bronze]][host_pos]
}