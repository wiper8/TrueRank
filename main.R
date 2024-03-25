source("src/players.R")
source("src/scores.R")
source("src/update_scores.R")
source("src/plots.R")


scores <- scores_init()

players <- list()

players <- add_player("Will", players)
players <- add_player("Éti", players)
players <- add_player("Vic", players)
players <- add_player("Phil", players)
players <- add_player("Ant", players)
players <- add_player("Xav", players)
players <- add_player("Gab", players)


scores <- add_scores(
  data.frame(
    joueur_A1 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    joueur_A2 = c("Éti", "Will", "Will", "Will", "Will", "Will", "Will", "Will", "Will", "Will", "Vic", "Phil", "Will", "Phil", "Will", "Will", "Will", "Will", "Will", "Will", "Phil", "Will", "Will", "Will", "Phil", "Will", "Will", "Will", "Phil", "Will", "Phil", "Will", "Will", "Will"),
    joueur_B1 = c("Vic", "Éti", "Vic", "Vic", "Vic", "Vic", "Éti", "Vic", "Vic", "Éti", "Éti", "Éti", "Phil", "Éti", "Phil", "Éti", "Éti", "Phil", "Éti", "Phil", "Éti", "Éti", "Phil", "Éti", "Éti", "Éti", "Phil", "Éti", "Éti", "Éti", "Éti", "Éti", "Éti", "Éti"),
    joueur_B2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    win = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1),
    game_length = c(7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, rep(7, 23))
  ),
  scores,
  date = "2024-03-12"
)

scores <- add_scores(
  daily_scores = data.frame(
    joueur_A1 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Éti", "Will", "Will", "Will", NA, NA, NA, NA, NA, NA, NA, NA),
    joueur_A2 = c("Vic", "Will", "Will", "Vic", "Will", "Will", "Will", "Vic", "Phil", "Will", "Vic", "Will", "Phil", "Vic", "Éti", "Vic", "Vic", "Éti", "Phil", "Éti", "Éti", "Phil", "Éti"),
    joueur_B1 = c("Éti", "Éti", "Vic", "Éti", "Éti", "Vic", "Éti", "Éti", "Éti", "Éti", "Éti", "Phil", "Éti", "Phil", "Phil", "Vic", "Vic", "Éti", "Phil", "Éti", "Éti", "Phil", "Éti"),
    joueur_B2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Vic", "Vic", "Éti", "Vic", NA, NA, NA, NA, NA, NA, NA, NA),
    win = c(0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1),
    game_length = c(7, 7, 7, 7, 7, 7, 7, 7, 5, 5, 5, 11, 11, 11, 11, 5, 5, 5, 5, 5, 5, 5, 5)
  ),
  scores,
  date = "2024-03-14"
)

scores <- add_scores(
  daily_scores = data.frame(
    joueur_A1 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Will", "Will", "Will", "Will", NA, "Will", "Will", NA, "Will", "Will", "Will", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    joueur_A2 = c("Phil", "Will", "Will", "Will", "Phil", "Phil", "Phil", "Phil", "Phil", "Phil", "Will", "Will", "Vic", "Vic", "Phil", "Éti", "Vic", "Vic", "Phil", "Éti", "Éti", "Vic", "Phil", "Éti", "Phil", "Vic", "Éti", "Will", "Will", "Éti", "Will", "Will", "Éti", "Will", "Phil", "Éti", "Will", "Will", "Vic"),
    joueur_B1 = c("Vic", "Phil", "Vic", "Phil", "Vic", "Éti", "Will", "Vic", "Éti", "Will", "Vic", "Éti", "Éti", "Éti", "Éti", "Vic", "Éti", "Éti", "Éti", "Vic", "Vic", "Éti", "Éti", "Phil", "Éti", "Vic", "Vic", "Phil", "Éti", "Phil", "Éti", "Phil", "Phil", "Éti", "Vic", "Vic", "Vic", "Éti", "Éti"),
    joueur_B2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Phil", "Vic", "Phil", "Phil", NA, "Vic", "Phil", NA, "Phil", "Vic", "Vic", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    win = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0),
    game_length = c(7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 11, 11, 11, 11, 3, 11, 11, 3, 11, 11, 11, 7, 5, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7)
  ),
  scores,
  date = "2024-03-19"
)

scores <- add_scores(
  daily_scores = data.frame(
    joueur_A1 = c(NA, NA, NA, NA, NA),
    joueur_A2 = c("Will", "Will", "Will", "Will", "Will"),
    joueur_B1 = c("Éti", "Éti", "Éti", "Éti", "Éti"),
    joueur_B2 = c(NA, NA, NA, NA, NA),
    win = c(1, 1, 0, 0, 1),
    game_length = c(11, 11, 11, 11, 11)
  ),
  scores,
  date = "2024-03-20"
)

scores <- add_scores(
  daily_scores = data.frame(
    joueur_A1 = c(NA, NA, NA, NA, NA, NA, NA, NA, "Xav", "Vic", "Phil", "Will", "Vic", NA, NA, NA),
    joueur_A2 = c("Vic", "Vic", "Vic", "Phil", "Phil", "Phil", "Phil", "Vic", "Phil", "Phil", "Gab", "Éti", "Gab", "Xav", "Will", "Will"),
    joueur_B1 = c("Ant", "Ant", "Ant", "Gab", "Xav", "Vic", "Will", "Éti"),
    joueur_B2 = c(NA, NA, NA, NA, NA, NA, NA, NA, "Phil", "Phil", "Gab", "Éti", "Gab", NA, NA, NA),
    win = c(0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1),
    game_length = c(11, 11, 11, 5, 5, 5, 5, 5, 7, 7, 7, 7, 7, 5, 5, 7)
  ),
  scores,
  date = "2024-03-21"
)

players <- update_scores(players, scores)

show_current_ranking(players)

show_current_detailed_ranking(players)

show_current_probs(players)

lapply(players, marginal_per_player)

show_ranking_history(scores)

#combien de fois plus lent une partie de double que de simple
(dim_len_mu * dim_len_sig * dim_len_F_2vs2)^4 / (dim_len_mu * dim_len_sig * dim_len_F_1vs1)^2
