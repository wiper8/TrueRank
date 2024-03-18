library(data.table)

dim_len_F_1vs1 <- 10
dim_len_F_2vs2 <- 1

calculate_ranking <- function(distr_mu_sig) {
  round(sum(apply(distr_mu_sig, 1, function(x) x[1] - 2*x[2]) * distr_mu_sig[, "p"]), 1)
}

marginal_per_player <- function(distr) {
  distr <- setDT(as.data.frame(distr))
  list(
    as.data.frame(distr[, .(p=sum(p)), by = .(mu)]),
    as.data.frame(distr[, .(p=sum(p)), by = .(sig)])
  )
}

post_marginal_per_player <- function(posteriori) {
  if(ncol(posteriori) == 5) {
    posteriori <- setDT(as.data.frame(posteriori))
    list(
      as.data.frame(posteriori[, .(p=sum(p)), by = .(mu1, sig1)][, .(mu=mu1, sig=sig1, p=p)]),
      as.data.frame(posteriori[, .(p=sum(p)), by = .(mu2, sig2)][, .(mu=mu2, sig=sig2, p=p)])
    )
  } else {
    posteriori <- setDT(as.data.frame(posteriori))
    list(
      as.data.frame(posteriori[, .(p=sum(p)), by = .(muA1, sigA1)][, .(mu=muA1, sig=sigA1, p=p)]),
      as.data.frame(posteriori[, .(p=sum(p)), by = .(muA2, sigA2)][, .(mu=muA2, sig=sigA2, p=p)]),
      as.data.frame(posteriori[, .(p=sum(p)), by = .(muB1, sigB1)][, .(mu=muB1, sig=sigB1, p=p)]),
      as.data.frame(posteriori[, .(p=sum(p)), by = .(muB2, sigB2)][, .(mu=muB2, sig=sigB2, p=p)])
    )
  }
}

distr_F_finder <- function(distr_mu_sig, dim_len_F) {
  distr_F <- apply(
    distr_mu_sig,
    1,
    function(x) cbind("mu"=x[1],
                      "sig"=x[2],
                      "F"=pmax(1, qnorm(seq(0 + 1/(dim_len_F+1), 1 - 1/(dim_len_F+1), length.out = dim_len_F), x[1], x[2])),
                      p=1/dim_len_F),
    simplify = F
  )
  
  do.call(rbind, mapply(
    function(x, y) cbind("mu"=y[, "mu"], "sig" = y[, "sig"],
                         "F"=x[, "F"], p=x[, "p"] * y[, "p"]),
    x=distr_F, y=split.data.frame(as.data.frame(distr_mu_sig), 1:nrow(distr_mu_sig)), SIMPLIFY = F))
}

distr_F1_F2_1vs1 <- function(distr_F1, distr_F2) {
  
  tmp <- faster_expand.grid(1:nrow(distr_F1), 1:nrow(distr_F2))
  distr_F1_F2 <- cbind(distr_F1[tmp[, 1], ], distr_F2[tmp[, 2], ])
  colnames(distr_F1_F2) <- c(t(sapply(colnames(distr_F1), function(x) paste0(x, c("1", "2")))))
  distr_F1_F2
}

faster_expand.grid <- function (...) {
  nargs <- length(args <- list(...))
  cargs <- vector("list", nargs)
  iArgs <- seq_len(nargs)
  rep.fac <- 1L
  d <- lengths(args)
  orep <- prod(d)
  
  res <- matrix(NA, nrow = orep, ncol = nargs)
  
  for (i in iArgs) {
    x <- args[[i]]
    orep <- orep/d[i]
    res[, i] <- rep.int(rep.int(x, rep.int(rep.fac, d[i])), orep)
    rep.fac <- rep.fac * d[i]
  }
  res
}

distr_F1_F2_2vs2 <- function(distr_FA1, distr_FA2, distr_FB1, distr_FB2) {
  
  tmp <- faster_expand.grid(1:nrow(distr_FA1), 1:nrow(distr_FA2), 1:nrow(distr_FB1), 1:nrow(distr_FB2))
  distr_F1_F2 <- cbind(distr_FA1[tmp[, 1], ], distr_FA2[tmp[, 2], ], distr_FB1[tmp[, 3], ], distr_FB2[tmp[, 4], ])
  colnames(distr_F1_F2) <- c(t(sapply(colnames(distr_FA1), function(x) paste0(x, c("A1", "A2", "B1", "B2")))))
  distr_F1_F2
}

p_win_game_of <- function(p, g = 7) {
  sapply(p, function(p) sum(dnbinom(0:(g-1), g, p)))
}


distr_P_1vs1 <- function(distr_F1_F2) {
  cbind(distr_F1_F2, "P"=
          distr_F1_F2[, "F1"] / (distr_F1_F2[, "F1"] + distr_F1_F2[, "F2"])
  )
}


distr_P_2vs2 <- function(distr_F1_F2) {
  cbind(distr_F1_F2, 
        "P" = (distr_F1_F2[, "FA1"] / (distr_F1_F2[, "FA1"] + distr_F1_F2[, "FB1"]) + distr_F1_F2[, "FA2"] / (distr_F1_F2[, "FA2"] + distr_F1_F2[, "FB1"]) +
     distr_F1_F2[, "FA1"] / (distr_F1_F2[, "FA1"] + distr_F1_F2[, "FB2"]) + distr_F1_F2[, "FA2"] / (distr_F1_F2[, "FA2"] + distr_F1_F2[, "FB2"])) / 4
  )
}

posteriori_1vs1 <- function(distr_mu_sig1, distr_mu_sig2, game_len, win,
                            dim_mu = dim_len_mu, dim_sig = dim_len_sig, dim_F = dim_len_F_1vs1) {
  
  distr_F1 <- distr_F_finder(distr_mu_sig1, dim_F)
  
  distr_F2 <- distr_F_finder(distr_mu_sig2, dim_F)
  
  distr_F1_F2 <- distr_F1_F2_1vs1(distr_F1, distr_F2)
  
  distr_P <- distr_P_1vs1(distr_F1_F2)
  
  distr_P <- cbind(distr_P, "P_win" = p_win_game_of(distr_P[, "P"], game_len))
  
  Likelihood <- (distr_P[, "P_win"] * win + (1-distr_P[, "P_win"]) * (1-win)) * distr_P[, "p1"] * distr_P[, "p2"]
  Likelihood <- Likelihood/sum(Likelihood)
  Likelihood <- cbind(Likelihood, distr_P)
  
  id <- rep(1:(dim_mu * dim_sig) * dim_F, dim_mu * dim_sig)
  id <- id + rep((seq(dim_mu * dim_sig)-1) * dim_mu * dim_sig * dim_F^2, each = dim_mu * dim_sig)
  
  id2 <- rep(rep(rep(1:(dim_mu * dim_sig), each=dim_F), dim_F), dim_mu * dim_sig)
  id2 <- id2 + rep((seq(dim_mu * dim_sig)-1) * (dim_mu * dim_sig), each = dim_mu * dim_sig * dim_F^2)
  
  posteriori <- cbind(
    Likelihood[id, c("mu1", "sig1", "mu2", "sig2")],
    "p"=sapply(split(Likelihood[, "Likelihood"], id2), sum)
  )
  
  posteriori
}


posteriori_2vs2 <- function(distr_mu_sigA1, distr_mu_sigA2,
                            distr_mu_sigB1, distr_mu_sigB2,
                            game_len, win,
                            dim_mu = dim_len_mu, dim_sig = dim_len_sig, dim_F = dim_len_F_2vs2) {
  
  distr_FA1 <- distr_F_finder(distr_mu_sigA1, dim_F)
  
  distr_FA2 <- distr_F_finder(distr_mu_sigA2, dim_F)
  
  distr_FB1 <- distr_F_finder(distr_mu_sigB1, dim_F)
  
  distr_FB2 <- distr_F_finder(distr_mu_sigB2, dim_F)
  
  distr_F1_F2 <- distr_F1_F2_2vs2(distr_FA1, distr_FA2, distr_FB1, distr_FB2)
  
  distr_P <- distr_P_2vs2(distr_F1_F2)
  
  distr_P <- cbind(distr_P, "P_win" = p_win_game_of(distr_P[, "P"], game_len))
  
  Likelihood <- (distr_P[, "P_win"] * win + (1-distr_P[, "P_win"]) * (1-win)) * distr_P[, "pA1"] * distr_P[, "pA2"] * distr_P[, "pB1"] * distr_P[, "pB2"]
  Likelihood <- Likelihood/sum(Likelihood)
  Likelihood <- cbind(Likelihood, distr_P)
  
  id <- rep(1:(dim_mu * dim_sig) * dim_F, (dim_mu * dim_sig)^3)
  id <- id + rep(rep((seq(dim_mu * dim_sig)-1) * dim_mu * dim_sig * dim_F^2, each = dim_mu * dim_sig), (dim_mu * dim_sig)^2)
  id <- id + rep(rep((seq(dim_mu * dim_sig)-1) * (dim_mu * dim_sig * dim_F)^2 * dim_F, each = (dim_mu * dim_sig)^2), dim_mu * dim_sig)
  id <- id + rep((seq(dim_mu * dim_sig)-1) * (dim_mu * dim_sig * dim_F)^3 * dim_F, each = (dim_mu * dim_sig)^3)
  
  id2 <- rep(rep(1:(dim_mu * dim_sig), each=dim_F), (dim_mu * dim_sig * dim_F)^3)
  id2 <- id2 + rep(rep((seq(dim_mu * dim_sig)-1) * (dim_mu * dim_sig), each = dim_mu * dim_sig * dim_F^2), (dim_mu * dim_sig * dim_F)^2)
  id2 <- id2 + rep(rep((seq(dim_mu * dim_sig)-1) * (dim_mu * dim_sig)^2, each = (dim_mu * dim_sig * dim_F)^2 * dim_F), (dim_mu * dim_sig * dim_F))
  id2 <- id2 + rep((seq(dim_mu * dim_sig)-1) * (dim_mu * dim_sig)^3, each = (dim_mu * dim_sig * dim_F)^3 * dim_F)
  
  posteriori <- cbind(
    Likelihood[id, c("muA1", "sigA1", "muA2", "sigA2", "muB1", "sigB1", "muB2", "sigB2")],
    "p"=sapply(split(Likelihood[, "Likelihood"], id2), sum)
  )
  
  posteriori
}

posteriori_of_game <- function(players, score) {
  if(is.na(score[, "joueur_A1"])) {
    posteriori <- posteriori_1vs1(distr_mu_sig1 = players[[score[, "joueur_A2"]]],
                                  distr_mu_sig2 = players[[score[, "joueur_B1"]]],
                                  game_len = as.numeric(score[, "game_length"]),
                                  win = as.numeric(score[, "win"]))
    posteriori_per_player <- post_marginal_per_player(posteriori)
    players[[score[, "joueur_A2"]]] <- posteriori_per_player[[1]]
    players[[score[, "joueur_B1"]]] <- posteriori_per_player[[2]]
    players
  } else {
    posteriori <- posteriori_2vs2(distr_mu_sigA1 = players[[score[, "joueur_A1"]]],
                                  distr_mu_sigA2 = players[[score[, "joueur_A2"]]],
                                  distr_mu_sigB1 = players[[score[, "joueur_B1"]]],
                                  distr_mu_sigB2 = players[[score[, "joueur_B2"]]],
                                  game_len = as.numeric(score[, "game_length"]),
                                  win = as.numeric(score[, "win"]))
    posteriori_per_player <- post_marginal_per_player(posteriori)
    players[[score[, "joueur_A1"]]] <- posteriori_per_player[[1]]
    players[[score[, "joueur_A2"]]] <- posteriori_per_player[[2]]
    players[[score[, "joueur_B1"]]] <- posteriori_per_player[[3]]
    players[[score[, "joueur_B2"]]] <- posteriori_per_player[[4]]
    players
  }
}

#TODO valider que ça marche bien
posteriori_of_game_simplified <- function(players, score) {
  if(is.na(score[, "joueur_A1"])) {
    
    tmp <- distr_simplifier_1vs1(players[[score[, "joueur_A2"]]], players[[score[, "joueur_B1"]]])
    print(paste0("  ", tmp[["dim_mu"]]))
    
    probs_ignorees1 <- sum(players[[score[, "joueur_A2"]]][!tmp[["keep1"]], "p"])
    probs_ignorees2 <- sum(players[[score[, "joueur_B1"]]][!tmp[["keep2"]], "p"])
    if(probs_ignorees1 > 0.02 | probs_ignorees2 > 0.02) warning(
      paste0("prob élevée ", round(max(c(probs_ignorees1, probs_ignorees2)),3), collapse = " "
      ))
    
    posteriori <- posteriori_1vs1(distr_mu_sig1 = tmp[["distr1"]],
                                  distr_mu_sig2 = tmp[["distr2"]],
                                  game_len = as.numeric(score[, "game_length"]),
                                  win = as.numeric(score[, "win"]),
                                  dim_mu = tmp[["dim_mu"]])
    posteriori_per_player <- post_marginal_per_player(posteriori)
    
    posteriori_per_player[[1]][, "p"] <- posteriori_per_player[[1]][, "p"] * (1-probs_ignorees1)
    posteriori_per_player[[2]][, "p"] <- posteriori_per_player[[2]][, "p"] * (1-probs_ignorees2)
    
    players[[score[, "joueur_A2"]]][tmp[["keep1"]], ] <- as.matrix(posteriori_per_player[[1]])
    players[[score[, "joueur_B1"]]][tmp[["keep2"]], ] <- as.matrix(posteriori_per_player[[2]])
    players
  } else {
    
    tmp <- distr_simplifier_2vs2(players[[score[, "joueur_A1"]]], players[[score[, "joueur_A2"]]],
                                 players[[score[, "joueur_B1"]]], players[[score[, "joueur_B2"]]])
    print(paste0("  ", tmp[["dim_mu"]]))
    
    probs_ignoreesA1 <- sum(players[[score[, "joueur_A1"]]][!tmp[["keepA1"]], "p"])
    probs_ignoreesA2 <- sum(players[[score[, "joueur_A2"]]][!tmp[["keepA2"]], "p"])
    probs_ignoreesB1 <- sum(players[[score[, "joueur_B1"]]][!tmp[["keepB1"]], "p"])
    probs_ignoreesB2 <- sum(players[[score[, "joueur_B2"]]][!tmp[["keepB2"]], "p"])
    
    if(probs_ignoreesA1 > 0.02 | probs_ignoreesA2 > 0.02 |
       probs_ignoreesB1 > 0.02 | probs_ignoreesB2 > 0.02) warning(
         paste0("prob élevée ", round(max(c(probs_ignoreesA1,probs_ignoreesA2,probs_ignoreesB1,probs_ignoreesB2)), 3), collapse = " "
                ))
    
    posteriori <- posteriori_2vs2(distr_mu_sigA1 = tmp[["distrA1"]],
                                  distr_mu_sigA2 = tmp[["distrA2"]],
                                  distr_mu_sigB1 = tmp[["distrB1"]],
                                  distr_mu_sigB2 = tmp[["distrB2"]],
                                  game_len = as.numeric(score[, "game_length"]),
                                  win = as.numeric(score[, "win"]),
                                  dim_mu = tmp[["dim_mu"]])
    posteriori_per_player <- post_marginal_per_player(posteriori)
    
    
    posteriori_per_player[[1]][, "p"] <- posteriori_per_player[[1]][, "p"] * (1-probs_ignoreesA1)
    posteriori_per_player[[2]][, "p"] <- posteriori_per_player[[2]][, "p"] * (1-probs_ignoreesA2)
    posteriori_per_player[[3]][, "p"] <- posteriori_per_player[[3]][, "p"] * (1-probs_ignoreesB1)
    posteriori_per_player[[4]][, "p"] <- posteriori_per_player[[4]][, "p"] * (1-probs_ignoreesB2)
    
    players[[score[, "joueur_A1"]]][tmp[["keepA1"]], ] <- as.matrix(posteriori_per_player[[1]])
    players[[score[, "joueur_A2"]]][tmp[["keepA2"]], ] <- as.matrix(posteriori_per_player[[2]])
    players[[score[, "joueur_B1"]]][tmp[["keepB1"]], ] <- as.matrix(posteriori_per_player[[3]])
    players[[score[, "joueur_B2"]]][tmp[["keepB2"]], ] <- as.matrix(posteriori_per_player[[4]])
    
    players
  }
}

update_scores <- function(players, scores) {
  for(i in 1:nrow(scores)) {
    print(i)
    players <- posteriori_of_game_simplified(players, scores[i, ])
  }
  players
}


 
