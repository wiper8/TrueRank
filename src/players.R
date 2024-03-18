players <- list()

dim_len_mu <- 25
dim_len_sig <- 3 #faible car ya déjà beaucoup de variance dans negbin

init_distr <- function() {
  sig1 <- qbeta(seq(0 + 1/(dim_len_mu + 1), 1 - 1/(dim_len_mu + 1), length.out = dim_len_sig), 3, 4) * 9 + 1
  mu1 <- qbeta(seq(0, 1, length.out = dim_len_mu), 2, 2) * (100-max(sig1)*2) + max(sig1)*2
  distr_mu1 <- cbind("mu"=mu1, "p"=1/dim_len_mu)
  distr_sig1 <- cbind("sig"=sig1, "p"=1/dim_len_sig)
  cbind(expand.grid(mu=mu1, sig=sig1), p = 1/dim_len_mu/dim_len_sig)
}

distr_simplifier_1vs1 <- function(distr1, distr2) {
  
  flush1 <- distr1[, "p"] < 1 / nrow(distr1) / 10
  flush2 <- distr2[, "p"] < 1 / nrow(distr2) / 10
  
  new_dim_len_mu <- max(length(unique(distr1[!flush1, "mu"])),
                        length(unique(distr2[!flush2, "mu"])))
  #sigma ne sera quasiement jamais simplifié à cause de la variance inhérente à la negbin.
  
  keep <- unique(distr1[order(distr1[, "p"], decreasing = T), ][, "mu"])[1:new_dim_len_mu]
  keep1 <- distr1[, "mu"] %in% keep
  distr1 <- distr1[keep1, ]
  distr1[, "p"] <- distr1[, "p"] / sum(distr1[, "p"])
  
  keep <- unique(distr2[order(distr2[, "p"], decreasing = T), ][, "mu"])[1:new_dim_len_mu]
  keep2 <- distr2[, "mu"] %in% keep
  distr2 <- distr2[keep2, ]
  distr2[, "p"] <- distr2[, "p"] / sum(distr2[, "p"])
  
  list(distr1 = distr1, distr2 = distr2, dim_mu = new_dim_len_mu, keep1 = keep1, keep2 = keep2)
}

distr_simplifier_2vs2 <- function(distrA1, distrA2, distrB1, distrB2) {
  
  flushA1 <- distrA1[, "p"] < 1 / nrow(distrA1) / 10
  flushA2 <- distrA2[, "p"] < 1 / nrow(distrA2) / 10
  flushB1 <- distrB1[, "p"] < 1 / nrow(distrB1) / 10
  flushB2 <- distrB2[, "p"] < 1 / nrow(distrB2) / 10
  
  new_dim_len_mu <- max(length(unique(distrA1[!flushA1, "mu"])),
                        length(unique(distrA2[!flushA2, "mu"])),
                        length(unique(distrB1[!flushB1, "mu"])),
                        length(unique(distrB2[!flushB2, "mu"])))
  #sigma ne sera quasiement jamais simplifié à cause de la variance inhérente à la negbin.
  
  keep <- unique(distrA1[order(distrA1[, "p"], decreasing = T), ][, "mu"])[1:new_dim_len_mu]
  keepA1 <- distrA1[, "mu"] %in% keep
  distrA1 <- distrA1[keepA1, ]
  distrA1[, "p"] <- distrA1[, "p"] / sum(distrA1[, "p"])
  
  keep <- unique(distrA2[order(distrA2[, "p"], decreasing = T), ][, "mu"])[1:new_dim_len_mu]
  keepA2 <- distrA2[, "mu"] %in% keep
  distrA2 <- distrA2[keepA2, ]
  distrA2[, "p"] <- distrA2[, "p"] / sum(distrA2[, "p"])
  
  keep <- unique(distrB1[order(distrB1[, "p"], decreasing = T), ][, "mu"])[1:new_dim_len_mu]
  keepB1 <- distrB1[, "mu"] %in% keep
  distrB1 <- distrB1[keepB1, ]
  distrB1[, "p"] <- distrB1[, "p"] / sum(distrB1[, "p"])
  
  keep <- unique(distrB2[order(distrB2[, "p"], decreasing = T), ][, "mu"])[1:new_dim_len_mu]
  keepB2 <- distrB2[, "mu"] %in% keep
  distrB2 <- distrB2[keepB2, ]
  distrB2[, "p"] <- distrB2[, "p"] / sum(distrB2[, "p"])
  
  list(distrA1 = distrA1, distrA2 = distrA2, distrB1 = distrB1, distrB2 = distrB2,
       dim_mu = new_dim_len_mu, keepA1 = keepA1, keepA2 = keepA2, keepB1 = keepB1, keepB2 = keepB2)
}

distr_simplifier_ALL <- function(players) {
  
  flush <- lapply(players, function(distr) {
    distr[, "p"] < 1 / nrow(distr) / 20 & distr[, "p"] < 1/5000
  })
  
  new_dim_len_mu <- max(mapply(function(distr, flush) length(unique(distr[!flush, "mu"])), players, flush))
  
  #TODO vérifier le précédent
  
  #sigma ne sera quasiement jamais simplifié à cause de la variance inhérente à la negbin.
  
  #TODO
  keep <- unique(distrA1[order(distrA1[, "p"], decreasing = T), ][, "mu"])[1:new_dim_len_mu]
  keepA1 <- distrA1[, "mu"] %in% keep
  distrA1 <- distrA1[keepA1, ]
  distrA1[, "p"] <- distrA1[, "p"] / sum(distrA1[, "p"])
  
  keep <- unique(distrA2[order(distrA2[, "p"], decreasing = T), ][, "mu"])[1:new_dim_len_mu]
  keepA2 <- distrA2[, "mu"] %in% keep
  distrA2 <- distrA2[keepA2, ]
  distrA2[, "p"] <- distrA2[, "p"] / sum(distrA2[, "p"])
  
  keep <- unique(distrB1[order(distrB1[, "p"], decreasing = T), ][, "mu"])[1:new_dim_len_mu]
  keepB1 <- distrB1[, "mu"] %in% keep
  distrB1 <- distrB1[keepB1, ]
  distrB1[, "p"] <- distrB1[, "p"] / sum(distrB1[, "p"])
  
  keep <- unique(distrB2[order(distrB2[, "p"], decreasing = T), ][, "mu"])[1:new_dim_len_mu]
  keepB2 <- distrB2[, "mu"] %in% keep
  distrB2 <- distrB2[keepB2, ]
  distrB2[, "p"] <- distrB2[, "p"] / sum(distrB2[, "p"])
  
  list(distrA1 = distrA1, distrA2 = distrA2, distrB1 = distrB1, distrB2 = distrB2,
       dim_mu = new_dim_len_mu, keepA1 = keepA1, keepA2 = keepA2, keepB1 = keepB1, keepB2 = keepB2)
}

add_player <- function(name, players) {
  players[[name]] <- init_distr()
  players
}
