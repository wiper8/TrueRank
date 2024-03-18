library(ggplot2)

show_current_ranking <- function(players) {
  ranks <- sapply(players, calculate_ranking)
  ranks <- ranks[order(ranks, decreasing = TRUE)]
  print(ranks)
  
  x <- 0:100
  
  graph_data <- data.frame(
    score = ranks,
    player = names(ranks)
  )
  
  ggplot()+
    geom_vline(aes(xintercept = score, col = player), data=graph_data, size=2)+
    theme_bw()+
    xlim(0, 100)
}

show_current_detailed_ranking <- function(players) {
  
  ranks <- sapply(players, calculate_ranking)
  ranks <- ranks[order(ranks, decreasing = TRUE)]
  print(ranks)
  
  x <- 0:100
  
  likely <- lapply(players, function(distr) apply(apply(distr, 1, function(d) dnorm(x, d[1], d[2]) * d[3]), 1, sum))
  likely <- lapply(likely, function(x) x/sum(x)) 
  
  graph_data <- data.frame(
    score = ranks,
    player = names(ranks)
  )
  
  graph_data2 <- data.frame(
    score = rep(x, length(likely)),
    likelihood = unlist(likely),
    player = rep(names(likely), each = length(x))
  )
  
  ggplot()+
    geom_vline(aes(xintercept = score, col = player), data=graph_data, size=2)+
    geom_line(aes(x=score, y=likelihood, col = player), data = graph_data2,
              size=1, alpha=0.8)+
    theme_bw()+
    xlim(0, 100)
  
}

show_ranking_history <- function(scores) {
  
  name <- unique(unlist(scores[, c("joueur_A1", "joueur_A2", "joueur_B1", "joueur_B2")]))
  name <- name[!is.na(name)]
  
  players <- list()
  for(n in name) players <- add_player(n, players)
  
  
  dates <- as.Date(unique(scores[, "date"]))
  dates <- sort(dates)
  
  graph_data <- data.frame(
    date = rep(dates, each = length(name)),
    player = rep(name, length(dates)),
    score = NA
  )
  
  
  for(d in as.character(dates)) {
    players <- update_scores(players, scores[scores[, "date"] == d, ])
    ranks <- sapply(players, calculate_ranking)
    ranks <- ranks[order(ranks, decreasing = TRUE)]
    for(n in name) graph_data[graph_data[, "date"] == d & graph_data[, "player"] == n, "score"] <- ranks[n]
  }
  
  ggplot(graph_data)+
    geom_line(aes(x=date, y=score, col=player), size=1)+
    theme_bw()
  
}