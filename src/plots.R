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
    geom_vline(aes(xintercept = score, col = player), data=graph_data, linewidth=2)+
    theme_bw()+
    xlim(0, 100)
}

show_current_probs <- function(players) {
  ranks <- sapply(players, calculate_ranking)
  ranks <- ranks[order(ranks, decreasing = TRUE)]
  n <- length(ranks) * (length(ranks)-1) / 2
  pairs <- t(combn(1:4, 2))
  
  data.frame(A=names(ranks)[pairs[, 1]], B=names(ranks)[pairs[, 2]], prob=round(ranks[pairs[, 1]] / apply(cbind(ranks[pairs[, 1]], ranks[pairs[, 2]]), 1, sum), 3))
}

show_current_detailed_ranking <- function(players) {
  
  ranks <- sapply(players, calculate_ranking)
  ranks <- ranks[order(ranks, decreasing = TRUE)]
  print(ranks)
  
  graph_data <- data.frame(
    score = ranks,
    player = names(ranks)
  )
  
  graph_data2 <- do.call(rbind, mapply(function(x, n) data.frame(x, "player"=n), players, names(players), SIMPLIFY = FALSE))
  
  ggplot()+
    geom_vline(aes(xintercept = score, col = player), data=graph_data, linewidth=2)+
    geom_line(aes(x=mu, y=p, col = player), data = graph_data2,
              linewidth=1, alpha=0.8)+
    xlab("Score")+ylab("Likelihood")+
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
    geom_line(aes(x=date, y=score, col=player), linewidth=1)+
    geom_point(aes(x=date, y=score, col=player))+
    theme_bw()
  
}