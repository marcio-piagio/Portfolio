glm.TAXA <- function(MODELO, digits = 2) {

  LABEL     <- "TAXA"
  COEF      <- stats::coef(MODELO)
  CONFINT   <- stats::confint(MODELO)
  TABLE     <- cbind(coef=COEF, CONFINT)
  TABLE.EXP <- round(exp(TABLE), digits)
  
  colnames(TABLE.EXP)[1] <- LABEL
  
  TABLE.EXP
}

res <- function(fit){
  X <- model.matrix(fit)
  n <- nrow(X)
  p <- ncol(X)
  w <- fit$weights
  h <- hatvalues(fit) 	# h_ii - Alavanca
  
  fi <- fit$theta
  if(fit$family[1]=="poisson"){fi <- 1} ### Phi consistent estimate for gamma distributio
  ts <- resid(fit, type="pearson")*sqrt(fi/(1-h))  	# Res Padronizado
  td <- resid(fit, type="deviance")*sqrt(fi/(1-h))	# Res Deviance
  di <- cooks.distance(fit)							            # Distancia de Cook
  
  eta  <- predict(fit, type='link')					        # Preditor Linear
  pred <- predict(fit, type='response')				      # Valores preditos
  
  z <- eta + resid(fit, type="pearson")/sqrt(w)	   	# Variavel Z
  
  library(statmod)
  rq <- qresiduals(fit)
  
  out <- data.frame(rq=rq, pred=pred, eta=eta, z=z, di=di, hii=h, model.matrix(fit)[,-1], id=1:n)
  return(out)
}

disGlm <- function(modelo, dados){
  # Dispersion statistic
  E2 <- resid(modelo, type = "pearson")
  N  <- nrow(dados)
  p  <- length(coef(modelo)) + 1  # '+1' is for variance parameter in NB
  sum(E2^2) / (N - p)
  
}
