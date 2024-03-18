source("src/players.R")
source("src/scores.R")
source("src/update_scores.R")
source("src/plots.R")

scores <- scores_init()

players <- list()

players <- add_player("Will", players)
players <- add_player("Étienne", players)
players <- add_player("Victor", players)
players <- add_player("Phil", players)

scores <- add_scores(
  data.frame(
    joueur_A1 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    joueur_A2 = c("Étienne", "Will", "Will", "Will", "Will", "Will", "Will", "Will", "Will", "Will", "Victor", "Phil", "Will", "Phil", "Will", "Will", "Will", "Will", "Will", "Will", "Phil", "Will", "Will", "Will", "Phil", "Will", "Will", "Will", "Phil", "Will", "Phil", "Will", "Will", "Will"),
    joueur_B1 = c("Victor", "Étienne", "Victor", "Victor", "Victor", "Victor", "Étienne", "Victor", "Victor", "Étienne", "Étienne", "Étienne", "Phil", "Étienne", "Phil", "Étienne", "Étienne", "Phil", "Étienne", "Phil", "Étienne", "Étienne", "Phil", "Étienne", "Étienne", "Étienne", "Phil", "Étienne", "Étienne", "Étienne", "Étienne", "Étienne", "Étienne", "Étienne"),
    joueur_B2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    win = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1),
    game_length = c(7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, rep(7, 23))
  ),
  scores,
  date = "2024-03-12"
)

scores <- add_scores(
  daily_scores = data.frame(
    joueur_A1 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Étienne", "Will", "Will", "Will", NA, NA, NA, NA, NA, NA, NA, NA),
    joueur_A2 = c("Victor", "Will", "Will", "Victor", "Will", "Will", "Will", "Victor", "Phil", "Will", "Victor", "Will", "Phil", "Victor", "Étienne", "Victor", "Victor", "Étienne", "Phil", "Étienne", "Étienne", "Phil", "Étienne"),
    joueur_B1 = c("Étienne", "Étienne", "Victor", "Étienne", "Étienne", "Victor", "Étienne", "Étienne", "Étienne", "Étienne", "Étienne", "Phil", "Étienne", "Phil", "Phil", "Victor", "Victor", "Étienne", "Phil", "Étienne", "Étienne", "Phil", "Étienne"),
    joueur_B2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Victor", "Victor", "Étienne", "Victor", NA, NA, NA, NA, NA, NA, NA, NA),
    win = c(0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1),
    game_length = c(7, 7, 7, 7, 7, 7, 7, 7, 5, 5, 5, 11, 11, 11, 11, 5, 5, 5, 5, 5, 5, 5, 5)
  ),
  scores,
  date = "2024-03-14"
)

players <- update_scores(players, scores)

show_current_ranking(players)

show_current_detailed_ranking(players)

lapply(players, marginal_per_player)

show_ranking_history(scores)

#combien de fois plus lent une partie de double que de simple
(dim_len_mu * dim_len_sig * dim_len_F_2vs2)^4 / (dim_len_mu * dim_len_sig * dim_len_F_1vs1)^2
