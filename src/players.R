#TODO améliorer la précision des dimensions (dim_len plus grands)
#TODO accélérer le code encore une fois
#TODO possiblement flusher des valeurs quasi-impossibles par personne
#TODO améliorer la modélisation du 2vs2. Possiblement refaire le système partiellement s'il faut modéliser les probs par coups.
#TODO ajouter possible drift dans le temps
#TODO que faire si qqun arrête de jouer pendant un bout.
#TODO shiny app?
#TODO excel partagé et trouver une manière de s'assurer que ce soit de bonnes données
#TODO se questionner si le score des gens devrait varier si les autres jouent en leur absence.
#TODO vérifier si l'ordonnancement des parties a un impact.


players <- list()

dim_len_mu <- 25

init_distr <- function() {
  mu1 <- qbeta(seq(0, 1, length.out = dim_len_mu), 2, 2) * 99 + 1
  distr_mu1 <- cbind("mu"=mu1, "p"=1/dim_len_mu)
  cbind(mu=mu1, p = 1/dim_len_mu)
}

distr_simplifier <- function(distr) {
  distr[, "p"] >= 1 / nrow(distr) / 12
}


distr_simplifier_top_10 <- function(distr) {
  
  repart <- cumsum(distr[, "p"])
  
  y <- seq(1/(10+1), 1-1/(10+1), length.out=10)
  
  x <- sapply(y, function(p) {
    if(!any(repart <= p)) {
      1 #minimum du range de skill
    } else {
      
    id <- tail(which(repart <= p), 1)
    
    #interpolation linéaire
    (distr[id+1, "mu"] - distr[id, "mu"]) / distr[id + 1, "p"] * (p - repart[id]) + distr[id, "mu"]
    }
  })
  
  y <- rep(1/10, 10)
  
  if(sum(x == 1) > 1) {
    y_1 <- sum(y[x == 1])
    y <- y[x != 1]
    x <- x[x != 1]
    x <- c(1, x)
    y <- c(y_1, y)
  }
  
  matrix(c(x, y), ncol=2, dimnames = dimnames(distr))
}

#kernel smoothing
distr_unsimplifier_top_10 <- function(distr, init_dom_x) {
  
  y <- sapply(init_dom_x, function(x) sum(dnorm(x, distr[, "mu"], sd(diff(distr[, "mu"]))*3)))
  y <- y/sum(y)
  
  matrix(c(init_dom_x, y), ncol=2, dimnames = dimnames(distr))
}

distr_simplifier_1vs1 <- function(distr1, distr2) {
  list(keep1 = distr_simplifier(distr1), keep2 = distr_simplifier(distr2))
}

distr_simplifier_2vs2 <- function(distrA1, distrA2, distrB1, distrB2) {
  list(keepA1 = distr_simplifier(distrA1), keepA2 = distr_simplifier(distrA2),
       keepB1 = distr_simplifier(distrB1), keepB2 = distr_simplifier(distrB2))
}

add_player <- function(name, players) {
  players[[name]] <- init_distr()
  players
}
