library(data.table)

calculate_skill <- function(distr_mu) {
  round(sum(distr_mu[, "mu"] * distr_mu[, "p"]), 1)
}


post_marginal_per_player <- function(posteriori) {
  if(ncol(posteriori) == 3) {
    posteriori <- setDT(as.data.frame(posteriori))
    list(
      as.data.frame(posteriori[, .(p=sum(p)), by = .(mu1)][, .(mu=mu1, p=p)]),
      as.data.frame(posteriori[, .(p=sum(p)), by = .(mu2)][, .(mu=mu2, p=p)])
    )
  } else {
    posteriori <- setDT(as.data.frame(posteriori))
    list(
      as.data.frame(posteriori[, .(p=sum(p)), by = .(muA1)][, .(mu=muA1, p=p)]),
      as.data.frame(posteriori[, .(p=sum(p)), by = .(muA2)][, .(mu=muA2, p=p)]),
      as.data.frame(posteriori[, .(p=sum(p)), by = .(muB1)][, .(mu=muB1, p=p)]),
      as.data.frame(posteriori[, .(p=sum(p)), by = .(muB2)][, .(mu=muB2, p=p)])
    )
  }
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
  sapply(p, function(p_i) sum(dnbinom(0:(g-1), g, p_i)))
}


distr_P_1vs1 <- function(distr_F1_F2) {
  cbind(distr_F1_F2, "P"=
          distr_F1_F2[, "F1"] / (distr_F1_F2[, "F1"] + distr_F1_F2[, "F2"])
  )
}

#TODO réfléchir a si c'est vraiment le calcul que je veux
#présentement c'est l'équivalent à dire chq point sont en 1vs1, mais on alternes les possibilitées
distr_P_2vs2 <- function(distr_F1_F2) {
  cbind(distr_F1_F2, 
        "P" = (distr_F1_F2[, "FA1"] / (distr_F1_F2[, "FA1"] + distr_F1_F2[, "FB1"]) + distr_F1_F2[, "FA2"] / (distr_F1_F2[, "FA2"] + distr_F1_F2[, "FB1"]) +
     distr_F1_F2[, "FA1"] / (distr_F1_F2[, "FA1"] + distr_F1_F2[, "FB2"]) + distr_F1_F2[, "FA2"] / (distr_F1_F2[, "FA2"] + distr_F1_F2[, "FB2"])) / 4
  )
}

transition_matrix <- function(skill, k = 3) {
  P <- matrix(NA, nrow = k+1, ncol = k+1, dimnames = list(paste0("reçu F", 0:k), paste0("frappé F", 0:k)))
  P[-1, ] <- t(sapply(1:k, function(ki) dbinom(0:k, k, skill * ((1 - ki) / (k - 1) + 1))))
  P[1, k+1] <- 1
  P[1, -(k+1)] <- 0
  P
}

prob_win_point_1vs1_knowing_skills <- function(MA, MB, k = 3) {
  
  M <- matrix(0, 2*nrow(MA), 2*ncol(MA), dimnames = list(
    c(paste0("A ", rownames(MA)), paste0("B ", rownames(MB))),
    c(paste0("B ", colnames(MA)), paste0("A ", colnames(MB)))
  ))
  M[1:nrow(MA), -1:-nrow(MA)] <- MA
  M[-1:-nrow(MA), 1:nrow(MA)] <- MB
  
  for(i in 1:5) {
    M <- M %*% M
  }
  
  # prob que A gagne sachant que A sert +  prob que A gagne sachant que B sert.
  0.5 * (M[3, 1]+ M[(k+1)+3, 2*(k+1)])
}

prob_win_point_2vs2_knowing_skills <- function(MA1, MA2, MB1, MB2, k = 3) {
  
  #A1->B1->A2->B2
  M1 <- matrix(0, 4*nrow(MA1), 4*ncol(MA1), dimnames = list(
    c(paste0("A1 ", rownames(MA1)), paste0("A2 ", rownames(MA2)), paste0("B1 ", rownames(MB1)), paste0("B2 ", rownames(MB2))),
    c(paste0("B2 ", colnames(MB1)), paste0("B1 ", colnames(MB2)), paste0("A1 ", colnames(MA1)), paste0("A2 ", colnames(MA1)))
  ))
  
  #A1->B2->A2->B1
  M2 <- matrix(0, 4*nrow(MA1), 4*ncol(MA1), dimnames = list(
    dimnames(M1)[[1]],
    c(paste0("B1 ", colnames(MB1)), paste0("B2 ", colnames(MB2)), paste0("A2 ", colnames(MA1)), paste0("A1 ", colnames(MA1)))
  ))
  
  
  M1[1:nrow(MA1), ncol(MB1)+ncol(MB2) + 1:ncol(MA1)] <- MA1
  M1[nrow(MA1) + 1:nrow(MA2), ncol(MB1)+ncol(MB2) + ncol(MA1) + 1:ncol(MA2)] <- MA2
  M1[nrow(MA1) + nrow(MA2) + 1:nrow(MB1), ncol(MB2) + 1:ncol(MB1)] <- MB1
  M1[nrow(MA1) + nrow(MA2) + nrow(MB2) + 1:nrow(MB2), 1:ncol(MB2)] <- MB2
  
  M2[1:nrow(MA1), ncol(MB1)+ncol(MB2)+ncol(MA2) + 1:ncol(MA1)] <- MA1
  M2[nrow(MA1) + 1:nrow(MA2), ncol(MB1)+ncol(MB2) + 1:ncol(MA2)] <- MA2
  M2[nrow(MA1) + nrow(MA2) + 1:nrow(MB1), 1:ncol(MB1)] <- MB1
  M2[nrow(MA1) + nrow(MA2) + nrow(MB2) + 1:nrow(MB2), ncol(MB1) + 1:ncol(MB2)] <- MB2
  
  #eigen_info <- eigen(t(M1))
  #round(Re(eigen_info$vectors %*% diag(eigen_info$values) %*% solve(eigen_info$vectors)), 1)
  
  for(i in 1:5) {
    M1 <- M1 %*% M1
    M2 <- M2 %*% M2
  }
  
  0.25 * (M1[3, 1] + M1[k+1+3, k+1+1] + M1[2*(k+1)+3, 2*(k+1)+1] + M1[3*(k+1)+3, 3*(k+1)+1])+
    0.25 * (M2[3, 1] + M1[k+1+3, k+1+1] + M1[2*(k+1)+3, 2*(k+1)+1] + M1[3*(k+1)+3, 3*(k+1)+1])
  
}

posteriori_1vs1 <- function(distr_S1, distr_S2, game_len, win) {
  
  distr_S1_S2 <- distr_F1_F2_1vs1(distr_S1, distr_S2)
  
  MS1 <- rep(lapply(distr_S1[, "mu"] / 100, transition_matrix), nrow(distr_S2))
  MS2 <- rep(lapply(distr_S2[, "mu"] / 100, transition_matrix), each = nrow(distr_S1))
  
  
  distr_P <- cbind(mu1=distr_S1_S2[, "mu1"],
                   mu2=distr_S1_S2[, "mu2"],
                   #TODO vectoriser pour faire plusieurs games en même temps.
                   #TODO weighter les games selon le nombre de jours passé avec (0.5^(1/121.67))^-x
                   "P_1_wins_pt"=mapply(function(MS1, MS2) prob_win_point_1vs1_knowing_skills(MS1, MS2),
         MS1, MS2),
         "p_s1_s2" = distr_S1_S2[, "p1"] * distr_S1_S2[, "p2"]
  )
  
  distr_P <- cbind(distr_P, "P_win_game" = p_win_game_of(distr_P[, "P_1_wins_pt"], game_len))
  
  Likelihood <- (distr_P[, "P_win_game"] * win + (1-distr_P[, "P_win_game"]) * (1-win)) * distr_P[, "p_s1_s2"]
  Likelihood <- Likelihood/sum(Likelihood)
  
  posteriori <- cbind(
    distr_P[, c("mu1", "mu2")],
    "p"=Likelihood
  )
  
  posteriori
}


posteriori_2vs2 <- function(distr_SA1, distr_SA2,
                            distr_SB1, distr_SB2,
                            game_len, win) {
  
  distr_SA1_SA2_SB1_SB2 <- distr_F1_F2_2vs2(distr_SA1, distr_SA2,
                                  distr_SB1, distr_SB2)
  
  MSA1 <- rep(lapply(distr_SA1[, "mu"] / 100, transition_matrix),
              nrow(distr_SA2) * nrow(distr_SB1) * nrow(distr_SB2))
  MSA2 <- rep(rep(lapply(distr_SA2[, "mu"] / 100, transition_matrix),
                  each = nrow(distr_SA1)), nrow(distr_SB1) * nrow(distr_SB2))
  MSB1 <- rep(rep(lapply(distr_SB1[, "mu"] / 100, transition_matrix),
                  each = nrow(distr_SA1) * nrow(distr_SA2)), nrow(distr_SB2))
  MSB2 <- rep(lapply(distr_SB2[, "mu"] / 100, transition_matrix),
                  each = nrow(distr_SA1) * nrow(distr_SA2) * nrow(distr_SB1))
  
  distr_P <- cbind(muA1=distr_SA1_SA2_SB1_SB2[, "muA1"],
                   muA2=distr_SA1_SA2_SB1_SB2[, "muA2"],
                   muB1=distr_SA1_SA2_SB1_SB2[, "muB1"],
                   muB2=distr_SA1_SA2_SB1_SB2[, "muB2"],
                   "P_A_wins_pt"=mapply(function(MSA1, MSA2, MSB1, MSB2) prob_win_point_2vs2_knowing_skills(MSA1, MSA2, MSB1, MSB2),
                                        MSA1, MSA2, MSB1, MSB2),
                   "p_sa1_sa2_sb1_sb2" = apply(distr_SA1_SA2_SB1_SB2[, c("pA1", "pA2", "pB1", "pB2")], 1, prod)
  )
  
  distr_P <- cbind(distr_P, "P_win_game" = p_win_game_of(distr_P[, "P_A_wins_pt"], game_len))
  
  Likelihood <- (distr_P[, "P_win_game"] * win + (1-distr_P[, "P_win_game"]) * (1-win)) * distr_P[, "p_sa1_sa2_sb1_sb2"]
  Likelihood <- Likelihood/sum(Likelihood)
  Likelihood <- cbind(Likelihood, distr_P)
  
  posteriori <- cbind(
    Likelihood[, c("muA1", "muA2", "muB1", "muB2")],
    "p"=Likelihood[, "Likelihood"]
  )
  
  posteriori
}

#TODO valider que ça marche bien
posteriori_of_game_simplified <- function(players, score) {
  if(is.na(score[, "joueur_A1"])) {
    
    tmp <- distr_simplifier_1vs1(distr1 = players[[score[, "joueur_A2"]]],
                                 distr2 = players[[score[, "joueur_B1"]]])
    
    probs_ignorees1 <- sum(players[[score[, "joueur_A2"]]][!tmp[["keep1"]], "p"])
    probs_ignorees2 <- sum(players[[score[, "joueur_B1"]]][!tmp[["keep2"]], "p"])
    
    if(probs_ignorees1 > 0.01 | probs_ignorees2 > 0.01) {
      probs_ignorees1 <- 0
      probs_ignorees2 <- 0
      tmp <- list(keep1 = rep(T, length(tmp[["keep1"]])),
                  keep2 = rep(T, length(tmp[["keep2"]])))
    } else {
      #print(paste0("  ", prod(sapply(tmp, sum))))
    }
    
    posteriori <- posteriori_1vs1(distr_S1 = players[[score[, "joueur_A2"]]][tmp[["keep1"]], ],
                                  distr_S2 = players[[score[, "joueur_B1"]]][tmp[["keep2"]], ],
                                  game_len = as.numeric(score[, "game_length"]),
                                  win = as.numeric(score[, "win"]))
    posteriori_per_player <- post_marginal_per_player(posteriori)
    
    posteriori_per_player[[1]][, "p"] <- posteriori_per_player[[1]][, "p"] * (1-probs_ignorees1)
    posteriori_per_player[[2]][, "p"] <- posteriori_per_player[[2]][, "p"] * (1-probs_ignorees2)
    
    players[[score[, "joueur_A2"]]][tmp[["keep1"]], ] <- as.matrix(posteriori_per_player[[1]])
    players[[score[, "joueur_B1"]]][tmp[["keep2"]], ] <- as.matrix(posteriori_per_player[[2]])
    players
  } else {
    
    #très lent donc on va davantage simplifier. discrétisation max de n.
    distrA1 <- distr_simplifier_top_n(players[[score[, "joueur_A1"]]], 10)
    distrA2 <- distr_simplifier_top_n(players[[score[, "joueur_A2"]]], 10)
    distrB1 <- distr_simplifier_top_n(players[[score[, "joueur_B1"]]], 10)
    distrB2 <- distr_simplifier_top_n(players[[score[, "joueur_B2"]]], 10)
    
    posteriori <- posteriori_2vs2(distr_SA1 = distrA1,
                                  distr_SA2 = distrA2,
                                  distr_SB1 = distrB1,
                                  distr_SB2 = distrB2,
                                  game_len = as.numeric(score[, "game_length"]),
                                  win = as.numeric(score[, "win"]))
    posteriori_per_player <- post_marginal_per_player(posteriori)
    
    posteriori_per_player[[1]] <- distr_unsimplifier_top_n(distr = posteriori_per_player[[1]], init_distr = players[[score[, "joueur_A1"]]], cap_factor = 1)
    posteriori_per_player[[2]] <- distr_unsimplifier_top_n(distr = posteriori_per_player[[2]], init_distr = players[[score[, "joueur_A2"]]], cap_factor = 1)
    posteriori_per_player[[3]] <- distr_unsimplifier_top_n(distr = posteriori_per_player[[3]], init_distr = players[[score[, "joueur_B1"]]], cap_factor = 1)
    posteriori_per_player[[4]] <- distr_unsimplifier_top_n(distr = posteriori_per_player[[4]], init_distr = players[[score[, "joueur_B2"]]], cap_factor = 1)
    
    players[[score[, "joueur_A1"]]] <- as.matrix(posteriori_per_player[[1]])
    players[[score[, "joueur_A2"]]] <- as.matrix(posteriori_per_player[[2]])
    players[[score[, "joueur_B1"]]] <- as.matrix(posteriori_per_player[[3]])
    players[[score[, "joueur_B2"]]] <- as.matrix(posteriori_per_player[[4]])
    
    players
  }
}

update_scores <- function(players, scores) {
  for(i in 1:nrow(scores)) {
    print(paste0(i, ifelse(is.na(scores[i, "joueur_A1"]), " 1vs1", " 2vs2")))
    players <- posteriori_of_game_simplified(players, scores[i, ])
    #plot(players$Éti)
    #Sys.sleep(1.5)
    if(max(sapply(players, function(distr) sum(distr[, "p"]))) > 1.0001) stop("Erreur de prob A")
    players <- lapply(players, simplifier_domain)
    #plot(players$Éti, col="red")
    if(max(sapply(players, function(distr) sum(distr[, "p"]))) > 1.0001) stop("Erreur de prob A")
    #Sys.sleep(1.5)
    
  }
  players
}


 
