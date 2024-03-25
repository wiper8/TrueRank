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
    joueur_A2 = c("Vic", "Will", "Will", "Vic", "Will", "Will", "Will", "Vic", "Phil", "Will", "Vic", "Will", "Phil", "Vic", "Éti", "Phil", "Phil", "Phil", "Will", "Will", "Phil", "Will", "Will"),
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
    joueur_A2 = c("Phil", "Will", "Will", "Will", "Phil", "Phil", "Phil", "Phil", "Phil", "Phil", "Will", "Will", "Vic", "Vic", "Phil", "Éti", "Vic", "Vic", "Phil", "Éti", "Éti", "Vic", "Phil", "Éti", "Phil", "Will", "Éti", "Will", "Will", "Éti", "Will", "Will", "Éti", "Will", "Phil", "Éti", "Will", "Will", "Vic"),
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
    joueur_B1 = c("Ant", "Ant", "Ant", "Gab", "Xav", "Vic", "Will", "Éti", "Will", "Xav", "Vic", "Vic", "Phil", "Gab", "Gab", "Vic"),
    joueur_B2 = c(NA, NA, NA, NA, NA, NA, NA, NA, "Gab", "Éti", "Will", "Xav", "Xav", NA, NA, NA),
    win = c(0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1),
    game_length = c(11, 11, 11, 5, 5, 5, 5, 5, 7, 7, 7, 7, 7, 5, 5, 7)
  ),
  scores,
  date = "2024-03-21"
)

#show_current_ranking(update_scores(players, scores[1:34, ]))



tmp <- show_ranking_history(scores)
players <- tmp[[1]]
ggplot(tmp[[2]])+
  geom_line(aes(x=date, y=score, col=player), linewidth=1)+
  geom_point(aes(x=date, y=score, col=player))+
  theme_bw()

show_current_ranking(players) #based on first version of ranking

show_current_probs(players)

show_skill_level(players)

show_detailed_skill(players)

lapply(show_skill_level(players) / 100, transition_matrix)

scores
