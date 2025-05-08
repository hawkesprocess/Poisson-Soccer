# 1. Historical goals scored by each team
goals_A <- c(2,1,3,0,1,2,1,2,3,1,
             2,0,1,2,1,3,2,1,0,2,
             1,1,2,3,1,0,2,1,1,2)
goals_B <- c(1,2,1,4,3,2,3,2,4,3,
             2,3,2,1,2,4,3,2,1,3,
             2,3,1,2,3,4,2,3,2,3)

# 2. Estimate Poisson λ for each team
lambda_A <- mean(goals_A)
lambda_B <- mean(goals_B)

# 3. Choose goal range to capture ≈99.99% of probability mass
max_goals <- qpois(0.9999, max(lambda_A, lambda_B))
goals     <- 0:max_goals

# 4. Compute marginal Poisson PMFs
pa <- dpois(goals, lambda_A)
pb <- dpois(goals, lambda_B)

# 5. Joint probability matrix P(X=i, Y=j)
joint_mat <- outer(pa, pb)

# 6. Compute win/draw probabilities
P_A_win <- sum(joint_mat[row(joint_mat) > col(joint_mat)])
P_B_win <- sum(joint_mat[row(joint_mat) < col(joint_mat)])
P_draw  <- sum(diag(joint_mat))
P_total <- P_A_win + P_B_win + P_draw

# 7. Print 
print(paste("Estimated lambda_A =", round(lambda_A, 3)))
print(paste("Estimated lambda_B =", round(lambda_B, 3)))
print(paste("Probability Team A wins =", round(P_A_win, 4)))
print(paste("Probability Team B wins =", round(P_B_win, 4)))
print(paste("Probability of draw       =", round(P_draw, 4)))
print(paste("Total probability sum     =", round(P_total, 4)))
