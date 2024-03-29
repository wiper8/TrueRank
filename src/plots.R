library(ggplot2)

show_skill_level <- function(players) {
  ranks <- sapply(players, calculate_skill)
  ranks <- ranks[order(ranks, decreasing = TRUE)]
  
  x <- 0:100
  
  graph_data <- data.frame(
    skill = ranks,
    player = names(ranks)
  )
  
  print(ggplot()+
    geom_vline(aes(xintercept = skill, col = player), data=graph_data, linewidth=2)+
    theme_bw()+
    xlim(0, 100)
  )
  ranks
}

show_current_probs <- function(players) {
  ranks <- sapply(players, calculate_skill)
  ordre <- order(ranks, decreasing = TRUE)
  players <- players[ordre]
  ranks <- ranks[ordre]
  pairs <- t(combn(1:length(ranks), 2))
  
  prob <- mapply(function(distr_S1, distr_S2) {
    
    distr_S1_S2 <- distr_F1_F2_1vs1(distr_S1, distr_S2)
    
    MS1 <- rep(lapply(distr_S1[, "mu"] / 100, transition_matrix), nrow(distr_S2))
    MS2 <- rep(lapply(distr_S2[, "mu"] / 100, transition_matrix), each = nrow(distr_S1))
    
    
    sum(mapply(function(MS1, MS2) prob_win_point_1vs1_knowing_skills(MS1, MS2),
               MS1, MS2) * distr_S1_S2[, "p1"] * distr_S1_S2[, "p2"])
  },
  players[pairs[, 1]],
  players[pairs[, 2]]
  )
  
  data.frame(A=names(ranks)[pairs[, 1]], B=names(ranks)[pairs[, 2]],
             prob = round(prob, 3), prob_win_11 = round(sapply(prob, function(p) p_win_game_of(p, 11)), 3))
}

show_current_ranking <- function(players) {
  probs <- show_current_probs(players)
  
  ranks <- sapply(players, calculate_skill)
  ordre <- order(ranks, decreasing = TRUE)
  players <- players[ordre]
  ranks <- ranks[ordre]
  pairs <- t(combn(1:length(ranks), 2))
  
  
  to_optim <- function(Forces) {
    
    estim <- 1/(1+10^(-(Forces[pairs[, 1]] - Forces[pairs[, 2]]) / 20))
    #estim <- Forces[pairs[, 1]] / (Forces[pairs[, 1]] + Forces[pairs[, 2]])
    
    mean((estim - probs[, "prob_win_11"])^2)
  }
  
  #min 0
  # res <- constrOptim(rep(50, length(players)), to_optim, grad = NULL,
  #             ui = rbind(diag(length(players))),
  #             ci = rep(0, length(players)))
  
  #moyenne 50 et min 0 et max 100
  #pourrait donner des bugs si le monde est très dispersé
  res <- constrOptim(rep(50, length(players)), to_optim, grad = NULL,
              ui = rbind(diag(length(players)), -diag(length(players)), rep(1, length(players)), rep(-1, length(players))),
              ci = c(rep(0, length(players)), rep(-100, length(players)), 49.5*length(players), -50.5*length(players)))

  
  #scores entre 1 et 100
  # res <- constrOptim(rep(50, length(players)), to_optim, grad = NULL,
  #                   ui = rbind(diag(length(players)), -diag(length(players))),
  #                   ci = c(rep(1, length(players)), rep(-100, length(players))))
  
  #sum(scores) = 50*n
  # res <- constrOptim(rep(50, length(players)), to_optim, grad = NULL,
  #             ui = rbind(diag(length(players)), -diag(length(players)), rep(1, length(players)), rep(-1, length(players))),
  #             ci = c(rep(1, length(players)), rep(-100, length(players)), 49.5*length(players), -50.5*length(players)))
  names(res$par) <- names(players)
  
  if(sqrt(res$value) > 0.02) warning("Convergence non parfaite des scores")
  
  graph_data <- data.frame(
    score = res$par,
    player = names(res$par)
  )
  
  print(
    ggplot()+
      geom_vline(aes(xintercept = score, col = player), data=graph_data, linewidth=2)+
      #xlim(0, 100)+
      theme_bw()
  )
  
  round(res$par, 1)
}


show_detailed_skill <- function(players) {
  
  ranks <- sapply(players, calculate_skill)
  ranks <- ranks[order(ranks, decreasing = TRUE)]
  print(ranks)
  
  graph_data <- data.frame(
    skill = ranks,
    player = names(ranks)
  )
  
  players <- lapply(players, function(distr) {
    y <- sapply(init_distr()[, "mu"], function(x) sum(distr[, "p"] * dnorm(x, distr[, "mu"], 1.5*mean(diff(distr[, "mu"])))))
    y <- y/sum(y)
    cbind(mu=init_distr()[, "mu"], p=y)
  })
  
  
  graph_data2 <- do.call(rbind, mapply(function(x, n) data.frame(x, "player"=n), players, names(players), SIMPLIFY = FALSE))
  
  ggplot()+
    geom_vline(aes(xintercept = skill, col = player), data=graph_data, linewidth=2)+
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
  
  game_dates <- as.Date(unique(scores[, "date"]))
  game_dates <- sort(game_dates)
  
  drift_per_player <- lapply(name, function(nom) {
    played <- apply(scores, 1, function(x) nom %in% x)
    date <- seq.Date(as.Date(max(scores[played, "date"])), as.Date(max(scores[, "date"])), by = "+1 month")
    date <- date[date > as.Date(max(scores[played, "date"]))]
    as.character(date)
  })
  
  drift_dates <- unique(unlist(drift_per_player))
  all_dates <- c(game_dates, drift_dates)
  all_dates <- unique(all_dates)
  all_dates <- as.Date(all_dates)
  all_dates <- sort(all_dates)
  
  
  graph_data <- data.frame(
    date = rep(all_dates, each = length(name)),
    player = rep(name, length(all_dates)),
    score = NA
  )
  
  for(d in as.character(all_dates)) {
    player_in_ranking <- unique(unlist(scores[scores[, "date"] <= d, c("joueur_A1", "joueur_A2", "joueur_B1", "joueur_B2")]))
    player_in_ranking <- player_in_ranking[!is.na(player_in_ranking)]
    if(d %in% drift_dates) {
      drifting_players <- sapply(drift_per_player, function(x) d %in% x)
      players[player_in_ranking[name[drifting_players] %in% player_in_ranking]] <- lapply(players[player_in_ranking[name[drifting_players] %in% player_in_ranking]], drift)
    }
    if(d %in% game_dates) {
      players[player_in_ranking] <- update_scores(players[player_in_ranking], scores[scores[, "date"] == d, ])
    }
    ranks <- show_current_ranking(players[player_in_ranking])
    for(n in player_in_ranking) graph_data[graph_data[, "date"] == d & graph_data[, "player"] == n, "score"] <- ranks[n]
  }
  
  list(players, graph_data)
}
