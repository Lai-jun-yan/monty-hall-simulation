set.seed(123)

# =========================
# 1. 模擬三門問題資料
# =========================
N <- 1000  # 做幾次遊戲

# 理論上換門勝率 = 2/3
true_p <- 2/3

# 模擬結果 (1 = win, 0 = lose)
x <- rbinom(N, size = 1, prob = true_p)

S <- sum(x)  # win 次數

# =========================
# 2. MCMC 設定 (Metropolis-Hastings)
# =========================

n_iter <- 10000
p_chain <- numeric(n_iter)

# 初始值
p_chain[1] <- 0.5

# proposal sd
sigma <- 0.05

# likelihood function (Bernoulli)
log_likelihood <- function(p, S, N) {
  if (p <= 0 || p >= 1) return(-Inf)
  S * log(p) + (N - S) * log(1 - p)
}

# prior Beta(1,1) => constant => ignore or include log prior
log_prior <- function(p) {
  if (p <= 0 || p >= 1) return(-Inf)
  0  # uniform
}

# =========================
# 3. MCMC sampling
# =========================
for (t in 2:n_iter) {
  
  current_p <- p_chain[t - 1]
  
  # proposal
  p_star <- rnorm(1, mean = current_p, sd = sigma)
  
  # symmetric proposal => MH ratio only likelihood + prior
  log_r <- (log_likelihood(p_star, S, N) +
              log_prior(p_star)) -
    (log_likelihood(current_p, S, N) +
       log_prior(current_p))
  
  # acceptance probability
  if (log(runif(1)) < log_r) {
    p_chain[t] <- p_star
  } else {
    p_chain[t] <- current_p
  }
}

# =========================
# 4. burn-in
# =========================
burn <- 2000
posterior <- p_chain[(burn + 1):n_iter]

# =========================
# 5. 結果輸出
# =========================

# posterior mean
mean(posterior)

# credible interval
quantile(posterior, c(0.025, 0.5, 0.975))

# =========================
# 6. 畫圖
# =========================

par(mfrow = c(1,2))

# trace plot
plot(p_chain, type = "l",
     main = "Trace plot of p",
     ylab = "p", xlab = "iteration")

abline(h = 2/3, col = "red", lwd = 2)

# posterior density
plot(density(posterior),
     main = "Posterior of p (Switching strategy)",
     xlab = "p")
abline(v = 2/3, col = "red", lwd = 2)