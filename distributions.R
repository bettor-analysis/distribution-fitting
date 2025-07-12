# --------------------------------------------------------
# Probability Distributions Fitting For Player Props
# Author: Adam Wickwire - Bettor Analysis
# --------------------------------------------------------

# --------------------------------------------------------
# What This Script Is About
# --------------------------------------------------------
# This script is an introduction to the concept of probability 
# distributions in the context of player prop betting.
#
# It’s not about how to build complex models—it’s about 
# understanding what distributions are, how they differ,
# and why choosing the right one matters.
#
# A stat might seem like it fits a common distribution (like 
# Normal or Poisson), but testing it against actual data often 
# reveals a better fit—like Negative Binomial or Gamma.
#
# Picking the correct distribution helps us estimate the 
# true probability of an outcome. And in sports betting, 
# that’s the whole game: comparing your probability estimate 
# to the price the sportsbook is offering.
#
# This script shows how to:
# - Understand the types of distributions used in betting
# - Visualize their shapes and behaviors
# - Fit real NFL player data to different distributions
# - Use AIC/BIC to test which one fits best
#
# Bottom line: you can’t beat the books if you don’t know 
# what the underlying stats really look like.

# --------------------------------------------------------
# What is a Probability Distribution?
# --------------------------------------------------------
# A probability distribution describes the likelihood of different outcomes for a stat.
# It helps us answer questions like:
# - What’s the chance a wide receiver gets 5 receptions?
# - How likely is it that a QB throws exactly 2 touchdowns?
# - What’s the probability a running back rushes for over 100 yards?

# --------------------------------------------------------
# Why Use a Probability Distribution?
# --------------------------------------------------------
# In many cases, we don’t have enough historical data to model a player’s outcomes directly.
# Sometimes, we only have a projection from a source like RotoWire or FantasyPros — like “4.2 receptions."
# But to make a bet, we need probabilities (e.g., over/under 4.5).
# That’s where probability distributions come in.
# So we rely on probability distributions to simulate outcomes 
# using just a projection (mean) and sometimes variance.
# This gives us a way to estimate probabilities—even with limited or no game log data.

# --------------------------------------------------------
# Two Types of Distributions
# --------------------------------------------------------

# Discrete Distributions
# These are for stats you count—whole numbers only.
# You either catch a pass or you don’t. You can’t have 2.7 receptions.
# Examples: receptions, touchdowns, interceptions, targets.

# Continuous Distributions
# These are for stats you measure—can include decimals.
# You might see 58.3 receiving yards or 312.6 passing yards.
# Examples: receiving yards, rushing yards, passing yards.

# --------------------------------------------------------
# Common Probability Distribution Types
# --------------------------------------------------------

# **Probability Distribution Visualizations**
# The following code for visualizing probability distributions
# are adapted from the book:
# "Bayesian Sports Models in R" by Andrew Mack
# https://www.amazon.com/Bayesian-Sports-Models-Andrew-Mack/dp/B0D9GNZY2B/
#
# Visuals have been reused or modified for educational purposes
# in this player prop modeling tutorial to show the shapes of different distributions.
#
# NOTE: This 'data' object is separate from player stats—it’s just for distribution plots.
# --------------------------------------------------------

# Load necessary libraries
library(ggplot2) # for plotting  

# Normal Distribution
# This is your classic bell curve.
# Most games fall near the player’s average. Some games are better, some worse.
# Example: A WR averages 60 yards. Most games are close to that, with a few much higher or lower.

means <- c(-2, 0, 2) # mu (mean)
sds <- c(0.5, 1, 1.5) # sigma (standard deviation)
n <- 100
x <- seq(-10, 10, length.out = n)  # Sequence of values

# Create a data frame with means & standard deviations
data <- data.frame(
  x = rep(x, times = length(means)),
  y = c(dnorm(x, mean = means[1], sd = sds[1]),
        dnorm(x, mean = means[2], sd = sds[2]),
        dnorm(x, mean = means[3], sd = sds[3])),
  group = factor(rep(1:length(means), each = n),
                 labels = paste0("mu = ", means,
                                 ", sigma = ", sds)))
# Plot the distributions
ggplot(data, aes(x = x, y = y, color = group)) +
  geom_line(linewidth = 1.2) +
  labs(x = "X", y = "Density",
       color = "Normal Distribution") + theme_minimal()
# --------------------------------------------------------

# Poisson Distribution
# This is for counting things that don’t happen a lot—like touchdowns or sacks.
# Works best when the average number is low.
# Example: A WR might average 0.7 touchdowns per game. Poisson helps model how often they get 0, 1, or 2.

# Lambda rate parameters
lambdas <- c(4.5, 6.5, 10)
max_x <- 20

# Create a data frame for the Poisson distributions
data <- data.frame(
  x = rep(0:max_x, times = length(lambdas)),
  y = c(dpois(0:max_x, lambda = lambdas[1]),
        dpois(0:max_x, lambda = lambdas[2]),
        dpois(0:max_x, lambda = lambdas[3])),
  group = factor(rep(1:length(lambdas),
                     each = max_x + 1),
                 labels = paste0("lambda = ", lambdas)))

# Plot the distributions
ggplot(data, aes(x = x, y = y, color = group)) +
  geom_line(linewidth = 1.2) +
  labs(x = "X", y = "Probability",
       color = "Poisson Distribution") + theme_minimal()
# --------------------------------------------------------

# Negative Binomial Distribution
# Like Poisson, but allows for more "boom or bust."
# It handles players who sometimes get very few stats, and sometimes get a lot.
# Example: A WR gets 3 targets one game, 12 the next. Poisson isn’t flexible enough—Negative Binomial is better.

# Means and overdispersions (variance exceeding mean)
means <- c(10, 10, 10)
dispersions <- c(12, 22, 42)
max_x <- 35

# Calculate the size and prob parameters
sizes <- (means^2) / (dispersions - means)
probs <- means / dispersions

# Create data frame for negative binomial distributions
data_list <- lapply(1:length(means), function(i) {
  x_vals <- 0:max_x
  y_vals <- dnbinom(x_vals, size = sizes[i], prob = probs[i])
  data.frame(x = x_vals, y = y_vals, group = paste0("mu = ", means[i], ", theta = ", dispersions[i]))})
data <- do.call(rbind, data_list)

# Plot the distributions
ggplot(data, aes(x = x, y = y, color = group)) +
  geom_line(linewidth = 1.2) +
  labs(x = "X", y = "Probability",
       color = "Negative Binomial Distribution") + theme_minimal()
# --------------------------------------------------------

# Binomial Distribution
# This is for yes/no outcomes repeated several times.
# Example: A WR gets 10 targets—how many does he catch? Each one is catch or no catch.

# Number of trials and probabilities of success
n_trials <- c(10, 20, 30)
probs <- c(0.45, 0.55, 0.65)
max_x <- max(n_trials)

# Create a data frame for the binomial distributions
data_list <- lapply(1:length(n_trials), function(i) {
  x_vals <- 0:n_trials[i]
  y_vals <- dbinom(x_vals, size = n_trials[i],
                   prob = probs[i])
  
  # Make all y vectors the same length
  y_vals <- c(y_vals, rep(0, max_x + 1 - length(y_vals)))
  data.frame(x = 0:max_x, y = y_vals,
             group = paste0("n = ", n_trials[i], ", p = ", probs[i]))})
data <- do.call(rbind, data_list)

# Plot the distributions
ggplot(data, aes(x = x, y = y, color = group)) +
  geom_line(linewidth = 1.2) +
  labs(x = "X", y = "Probability",
       color = "Binomial Distribution") + theme_minimal()
# --------------------------------------------------------

# Gamma Distribution
# This models stats that are always positive and often skewed.
# It's good for things like rushing or receiving yards, where a few players have big games.
# Example: Most RBs get 50–80 yards, but sometimes they explode for 150+. Gamma handles that.

# Shape and rate parameters
shapes <- c(2, 5, 9)
rates <- c(1, 0.65, 0.75)
n <- 100
# Sequence of x values
x <- seq(0, 20, length.out = n)

# Create a data frame for the gamma distributions
data <- data.frame(
  x = rep(x, times = length(shapes)),
  y = c(dgamma(x, shape = shapes[1], rate = rates[1]),
        dgamma(x, shape = shapes[2], rate = rates[2]),
        dgamma(x, shape = shapes[3], rate = rates[3])),
  group = factor(rep(1:length(shapes), each = n),
                 labels = paste0("alpha = ", shapes,
                                 ", beta = ", rates)))
# Plot the distributions
ggplot(data, aes(x = x, y = y, color = group)) +
  geom_line(linewidth = 1.2) +
  labs(x = "X", y = "Density",
       color = "Gamma Distribution") + theme_minimal()
# --------------------------------------------------------

# Beta Distribution
# This is used for stats that are percentages between 0 and 1.
# Not directly used for props, but helpful in background models.
# Example: A QB completes 65% of passes—Beta models that kind of percentage.

# Alpha and Beta parameters
alphas <- c(1, 2, 5)
betas <- c(1, 2, 5)
n <- 100
# Sequence of x values
x <- seq(0, 1, length.out = n)

# Create a data frame for the beta distributions
data <- data.frame(
  x = rep(x, times = length(alphas)),
  y = c(dbeta(x, shape1 = alphas[1],
              shape2 = betas[1]),
        dbeta(x, shape1 = alphas[2],
              shape2 = betas[2]),
        dbeta(x, shape1 = alphas[3],
              shape2 = betas[3])),
  group = factor(rep(1:length(alphas), each = n),
                 labels = paste0("alpha = ", alphas,
                                 ", beta = ", betas)))
# Plot the distributions
ggplot(data, aes(x = x, y = y, color = group)) +
  geom_line(linewidth = 1.2) +
  labs(x = "X", y = "Density",
       color = "Beta Distribution") + theme_minimal()




# -------------------------------------------
# Load Libraries
# -------------------------------------------
library(tidyverse)     # for data manipulation and visualization
library(nflfastR)      # for NFL data
library(MASS)          # for Negative Binomial fitting
library(fitdistrplus)  # for distribution fitting and comparison


# -------------------------------------------
# Load and Clean WR Data
# -------------------------------------------
# player_data <- load_player_stats(2014:2024)

player_data <- load_player_stats(2024)

# Keep only the columns relevant to betting props
player_data_clean <- player_data %>%
  filter(season_type == "REG") %>%
  dplyr::select(
    position, season, completions, attempts, passing_yards, passing_tds, 
    interceptions, carries, rushing_yards, rushing_tds,
    receptions, targets, receiving_yards, receiving_tds
  )

# Filter to just wide receivers and keep key stats
wr_data <- player_data_clean %>%
  filter(position == "WR") %>%
  dplyr::select(season, receptions, targets, receiving_yards, receiving_tds) %>%
  drop_na() %>% 
  filter(targets >= 1)


# -------------------------------------------
# Plot WR Stat Distributions
# -------------------------------------------

# Receptions Histogram
wr_data %>%
  ggplot(aes(x = receptions)) +
  geom_histogram(binwidth = 1, fill = "#1f77b4", color = "white", boundary = 0) +
  labs(
    title = "How Often Do WRs Catch Passes?",
    subtitle = "Histogram of Receptions (per game)",
    x = "Receptions",
    y = "Number of Games"
  ) +
  theme_minimal(base_size = 14)

# Receiving Yards Histogram
wr_data %>%
  ggplot(aes(x = receiving_yards)) +
  geom_histogram(binwidth = 10, fill = "#1f77b4", color = "white", boundary = 0) +
  labs(
    title = "How Many Receiving Yards Do WRs Get?",
    subtitle = "Histogram of Receiving Yards (per game)",
    x = "Receiving Yards",
    y = "Number of Games"
  ) +
  theme_minimal(base_size = 14)

# calculate the number of receiving yards less than 0
wr_data %>%
  filter(receiving_yards < 0) %>%
  nrow() -> negative_yards_count

# count the total number of observations of receiving yards 
wr_data %>%
  nrow() -> total_yards_count

# calculate the percentage of times a player had negative receiving yards
negative_yards_percentage <- round((negative_yards_count / total_yards_count) * 100, 4)


# -------------------------------------------
# Fitting Distributions
# -------------------------------------------
# To figure out which distribution fits the data best,
# we’ll use two scoring methods: AIC and BIC.
#
# - AIC = Akaike Information Criterion
# - BIC = Bayesian Information Criterion
#
# These scores tell us how well a model fits the data,
# while also penalizing it for being too complex.
# In other words, a model that fits well *and* keeps it simple
# will have a lower AIC or BIC score.
#
# You don’t need to know the math—just remember: **lower is better**.
# That’s all we care about for this test.
#
# There are other ways to measure fit (like chi-squared tests or
# the Kolmogorov-Smirnov test), but to keep things simple,
# we’re only using AIC and BIC for now.


# -------------------------------------------
# Receptions — Poisson vs Negative Binomial
# -------------------------------------------
# We'll compare two models for WR receptions:
# - Poisson: assumes a steady rate
# - Negative Binomial: handles high variation or "boom/bust" usage
# Poisson assumes the mean equals the variance. But in real data, variance is often much higher.
# Negative Binomial accounts for that overdispersion—making it a better fit for boom/bust stats.

fit_pois <- fitdist(wr_data$receptions, "pois")
fit_nbinom <- fitdist(wr_data$receptions, "nbinom")

# Compare fits using AIC/BIC
gofstat(list(fit_pois, fit_nbinom))  # returns AIC/BIC table

# Optional: Plot comparison (can skip if you’re not explaining graphs)
plot.legend <- c("Poisson", "Negative Binomial")
denscomp(list(fit_pois, fit_nbinom), legendtext = plot.legend)
cdfcomp(list(fit_pois, fit_nbinom), legendtext = plot.legend)
qqcomp(list(fit_pois, fit_nbinom), legendtext = plot.legend)
ppcomp(list(fit_pois, fit_nbinom), legendtext = plot.legend)


# -------------------------------------------
# Receiving Yards — Normal vs Gamma
# -------------------------------------------
# For yards, we compare:
# - Normal: assumes a bell curve around the average
# - Gamma: assumes values are always positive and skewed

# Filter out games with 0 or negative yards (Gamma can’t handle them)
wr_yards_pos <- wr_data %>% filter(receiving_yards > 0)

fit_norm <- fitdist(wr_yards_pos$receiving_yards, "norm")
fit_gamma <- fitdist(wr_yards_pos$receiving_yards, "gamma")

# Compare fits using AIC/BIC
gofstat(list(fit_norm, fit_gamma))

# Optional: Plot comparison
plot.legend <- c("Normal", "Gamma")
denscomp(list(fit_norm, fit_gamma), legendtext = plot.legend)
cdfcomp(list(fit_norm, fit_gamma), legendtext = plot.legend)
qqcomp(list(fit_norm, fit_gamma), legendtext = plot.legend)
ppcomp(list(fit_norm, fit_gamma), legendtext = plot.legend)


# -------------------------------------------
# Final Takeaway
# -------------------------------------------
# Based on AIC/BIC:
# - WR Receptions are better modeled by a Negative Binomial distribution
#                                 1-mle-pois 2-mle-nbinom
# Akaike's Information Criterion   9760.117     9150.056
# Bayesian Information Criterion   9765.765     9161.353

# - WR Receiving Yards are better modeled by a Gamma distribution
#                                 1-mle-norm 2-mle-gamma
# Akaike's Information Criterion   18604.39    17697.13
# Bayesian Information Criterion   18615.45    17708.19


# This matters because the shape of the distribution affects:
# - How we simulate alt lines
# - How we price bets
# - How we find +EV angles the books might miss

# Picking the right distribution is the first step in modeling prop value correctly.
# Don’t blindly trust a default distribution.
# Test what actually fits the data. That’s how you separate sharp from square.