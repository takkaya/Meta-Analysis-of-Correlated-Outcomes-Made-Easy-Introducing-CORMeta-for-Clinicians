#Try to sort by domain
create_plot <- function(data, x_left, x_center, x_right, y, mu_hat, se) {
  p <- ggplot(data, aes_string(x_center, y)) +
    geom_segment(aes_string(y = y, yend = y, x = x_left, xend = x_right)) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.3) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = seq(-20, 18, by = 2)) +
    scale_y_discrete(limits = rev(levels(data$dependent))) +
    
    # ✅ Replaced scalar-based aesthetics with annotate
    annotate("point", x = mu_hat, y = -1, shape = "|", size = 5) +
    annotate("segment", x = mu_hat - (2 * se), xend = mu_hat + (2 * se),
             y = -1, yend = -1, size = 0.5)
  
  p1 <- p + xlab("") + ylab("")
  
  return(p1 + theme(axis.text = element_text(size = 14),
                    axis.title = element_text(size = 14, face = "bold")))
}


# metafor.mod.cor <- function(y, v, R, data, PRINT=FALSE) {
#     tau_squared <- .1
#     labs <- data$dependent
#     A <- chol(R)
#     U <- t(A)
#     del <- 10
#     iter <- 0
#     n <- length(y)
#     X <- matrix(1, nrow = n, ncol = 1)
#     while (del > .01) {
#         iter <- iter + 1
#         if(PRINT) print(paste("Iteration ",as.character(iter),": tau_squared",as.character(tau_squared),": mu_hat",as.character(mu_hat)) )
#         Sigma <- tau_squared *  diag(1, n)  + U %*% diag(v) %*% t(U)
#         Sigma_inv <- solve(Sigma)
#         mu_hat <-
#             solve(t(X) %*% Sigma_inv %*% X) %*% t(X) %*% Sigma_inv %*% y
#         se_mu_hat <- sqrt(solve(t(X) %*% Sigma_inv %*% X))
#         res <- y - X %*% mu_hat
#         ll <- function(tsq) {
#             Sigma <- tsq *  diag(1, n)  + U %*% diag(v) %*% t(U)
#             Sigma_inv <- solve(Sigma)
#             if(tsq >= 1) ldetSigma = n*log(tsq) + log(det(diag(1, n)  + U %*% diag(v) %*% t(U)/tsq))
#             if(tsq < 1) ldetSigma = log(det(tsq*diag(1,n) + U %*% diag(v) %*% t(U)))
#             ll <- .5 * t(res) %*% Sigma_inv %*% res + .5 * ldetSigma
#             ll
#         }
#         tau_squared_new <-
#             optim(tau_squared, ll, lower = 0, method = "L-BFGS-B")$par
#         del <- abs(tau_squared_new - tau_squared)
#         tau_squared <- tau_squared_new
#     }
#     mu_hat_vec <- rep(mu_hat, length(y))
#     y_BLUP <- mu_hat_vec + tau_squared * Sigma_inv %*% (y - mu_hat_vec)
#     v_BLUP <-
#         diag(tau_squared * diag(length(y)) - tau_squared ^ 2  * Sigma_inv)
#     plot <- create_plot(
#         data,
#         y - 2 * sqrt(v),
#         y,
#         y + 2 * sqrt(v),
#         data$dependent,
#         mu_hat,
#         se_mu_hat
#     )
#     plot_post <- create_plot(
#         data,
#         y_BLUP - 2 * sqrt(v_BLUP),
#         y_BLUP,
#         y_BLUP + 2 * sqrt(v_BLUP),
#         data$dependent,
#         mu_hat,
#         se_mu_hat
#     )
#     return(list(
#         plot = plot,
#         plot_post = plot_post,
#         value_1 = mu_hat,
#         value_2 = se_mu_hat,
#         value_3 = tau_squared
#     ))
# }


metafor.mod <- function(y, v, R,RWorking, data, PRINT=FALSE) {

  s <- sqrt(v)
  V <- outer(s, s) * RWorking
  n <- length(y)
  X <- matrix(1, nrow = n, ncol = 1)
  lltsq <- function(tsq){
    # compute Sigma and its inverse
    Sigma <- tsq *diag(1, n) + V
    Sigma_inv <- solve(Sigma)
    # compute mu_hat
    mu_hat <- solve(t(X) %*% Sigma_inv %*% X) %*% t(X) %*% Sigma_inv %*% y
    pred <- X %*% mu_hat
    res <- y - pred
    if(tsq >= 1) ldetSigma = n*log(tsq) + log(det(diag(1, n) + V/tsq))
    if(tsq < 1) ldetSigma = log(det(tsq*diag(1,n) + V))
    ll <- .5 * t(res) %*% Sigma_inv %*% res + .5 * ldetSigma
    ll}
  # feed into optim to get mle of tausq
  tau_squared <- optim(6, lltsq, lower = 0, method = "L-BFGS-B")$par
  # get fixed effects
  Sigma <- tau_squared *diag(1, n) + V
  Sigma_inv <- solve(Sigma)
  # compute mu_hat
  weights <- solve(t(X) %*% Sigma_inv %*% X) %*% t(X) %*% Sigma_inv
  mu_hat <- weights %*% y
  se_mu_hat_naive <- sqrt(diag(solve(t(X) %*% Sigma_inv %*% X)))
  Sigma.True <- tau_squared *diag(1, n) + outer(s, s) * R
  se_mu_hat_adjusted <- sqrt( weights %*% Sigma.True %*% t(weights))
  t <- mu_hat/se_mu_hat_adjusted
  pvalue <- 2*pt(-abs(t),df=n-1)
  mu_hat_vec <- rep(mu_hat, length(y))
  y_BLUP <- mu_hat_vec + tau_squared * Sigma_inv %*% (y - mu_hat_vec)
  v_BLUP <-
    diag(tau_squared * diag(length(y)) - tau_squared ^ 2  * Sigma_inv)
  data$dependent <- as.character(data$dependent)
  data$dependent  <- factor(data$dependent, levels=unique(data$dependent))
  plot <- create_plot(
    data,
    y - 2 * sqrt(v),
    y,
    y + 2 * sqrt(v),
    data$dependent,
    mu_hat,
    se_mu_hat_adjusted

  )
  plot_post <- create_plot(
    data,
    y_BLUP - 2 * sqrt(v_BLUP),
    y_BLUP,
    y_BLUP + 2 * sqrt(v_BLUP),
    data$dependent,
    mu_hat,
    se_mu_hat_adjusted

  )
  return(list(
    plot = plot,
    plot_post = plot_post,
    value_1 = mu_hat,
    value_2 = se_mu_hat_adjusted,
    value_3 = tau_squared,
    value_4=pvalue
  ))
}


metafor.mod.cohort <- function(y, v, R, RWorking,XMatrix, data, PRINT=FALSE) {
  s <- sqrt(v)
  V <- outer(s, s) * RWorking
  n <- length(y)
  # cohort<-d$Cohort
  # group1 <-d$group1
  # group2<-d$group2
  # group3<-d$group3
  # group4 <-d$group4
  # group5 <-d$group5
  # X <- matrix(1, nrow = n, ncol = 1)
  # group1 <- rep(0,n)
  # group1[cohort==1] <- 1
  # group1[cohort==6] <- -1
  # #
  # group2 <- rep(0,n)
  # group2[cohort==2] <- 1
  # group2[cohort==6] <- -1
  # #
  # group3 <- rep(0,n)
  # group3[cohort==3] <- 1
  # group3[cohort==6] <- -1
  # #
  # group4 <- rep(0,n)
  # group4[cohort==4] <- 1
  # group4[cohort==6] <- -1
  # #
  # group5 <- rep(0,n)
  # group5[cohort==5] <- 1
  # group5[cohort==6] <- -1
  # X <- as.matrix(cbind(rep(1,n),group1,group2,group3,group4,group5))

    lltsq <- function(tsq){
    # compute Sigma and its inverse
    Sigma <- tsq *diag(1, n) + V
    Sigma_inv <- solve(Sigma)
    # compute mu_hat
    mu_hat <- solve(t(XMatrix) %*% Sigma_inv %*% XMatrix) %*% t(XMatrix) %*% Sigma_inv %*% y
    pred <- XMatrix %*% mu_hat
    res <- y - pred
    if(tsq >= 1) ldetSigma = n*log(tsq) + log(det(diag(1, n) + V/tsq))
    if(tsq < 1) ldetSigma = log(det(tsq*diag(1,n) + V))
    ll <- .5 * t(res) %*% Sigma_inv %*% res + .5 * ldetSigma
    ll}
  # feed into optim to get mle of tausq
  tau_squared <- optim(6, lltsq, lower = 0, method = "L-BFGS-B")$par
  # get fixed effects
  Sigma <- tau_squared *diag(1, n) + V
  Sigma_inv <- solve(Sigma)
  # compute mu_hat
  weights <- solve(t(XMatrix) %*% Sigma_inv %*% XMatrix) %*% t(XMatrix) %*% Sigma_inv
  mu_hat <- weights %*% y
  se_mu_hat_naive <- sqrt(diag(solve(t(XMatrix) %*% Sigma_inv %*% XMatrix)))
  Sigma.True <- tau_squared *diag(1, n) + outer(s, s) * R
  se_mu_hat_adjusted <- round(sqrt( weights %*% Sigma.True %*% t(weights)),3)
  t <- mu_hat[1,1]/se_mu_hat_adjusted[1,1]
  p_value <- round(2*pt(-abs(t),df=n-1),3)
  mu_hat_overall<- mu_hat[1,1]
  se_mu_hat_adjusted_overall<-se_mu_hat_adjusted[1,1]
  tau_squared<-tau_squared
  p_value<-p_value
  mu_hat_vec <- rep(mu_hat_overall, length(y))
  y_BLUP <- mu_hat_vec + tau_squared * Sigma_inv %*% (y - mu_hat_vec)
  v_BLUP <-
    diag(tau_squared * diag(length(y)) - tau_squared ^ 2  * Sigma_inv)
  data$dependent <- as.character(data$dependent)
  data$dependent  <- factor(data$dependent, levels=unique(data$dependent))
  plot <- create_plot(
    data,
    y - 2 * sqrt(v),
    y,
    y + 2 * sqrt(v),
    data$dependent,
    mu_hat_overall,
    se_mu_hat_adjusted_overall

  )
  plot_post <- create_plot(
    data,
    y_BLUP - 2 * sqrt(v_BLUP),
    y_BLUP,
    y_BLUP + 2 * sqrt(v_BLUP),
    data$dependent,
    mu_hat_overall,
    se_mu_hat_adjusted_overall

  )
  # result<- c(tau_squared, mu_hat[1,1], se_mu_hat_adjusted[1,1],p_value )
  # result <- cbind(y,v,as.vector(weights))
  # rownames(result) <- labs
  # if(PRINT) print(result)
  # if(PRINT) print("Here's a summary of the weights")
  # if(PRINT) print(summary(as.vector(weights)))
  # print(tau_squared)

  return(list(
    plot = plot,
    plot_post = plot_post,
    value_1 = mu_hat_overall,
    value_2 = se_mu_hat_adjusted_overall,
    value_3 = tau_squared,
    value_4=p_value
  ))
}
