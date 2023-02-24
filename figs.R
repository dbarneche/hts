library(tidyverse)
library(patchwork)
library(brms)
options(cores = parallel::detectCores())
dir.create("output", recursive = TRUE, showWarnings = FALSE)

rounded <- function(value, precision = 1) {
  sprintf(paste0("%.", precision, "f"), round(value, precision))
}

# Figure 1
M <- 1:1e3
b_o <- 5
alpha <- 0.75
E_a <- 0.65
E_i <- 3
T_opt <- 30 + 273.15
T_c <- 20 + 273.15
T <- 15:40 + 273.15

met_rates_m <- function(b_o, alpha, M) {
  b_o * M^alpha
}

met_rates_t <- function(b_o, alpha, E_a, E_i, T_opt, T_c, k = 8.62e-5, T) {
  f_T <- exp(E_a / k * (1 / T_c - 1 / T))
  f_T2 <- 1 / (1 + (E_a / (E_i - E_a)) * exp(E_i / k * (1 / T_opt - 1 / T)))
  b_o * f_T * f_T2
}

B_1 <- met_rates_m(b_o, alpha, M)
B_2 <- met_rates_t(b_o, alpha, E_a, E_i, T_opt, T_c, T = T)
B_T_opt <- met_rates_t(b_o, alpha, E_a, E_i, T_opt, T_c, T = T_opt)


fig_1a <- ggplot(data = data.frame(x = M, y = B_1)) +
  geom_line(mapping = aes(x = x, y = y), linetype = "dashed") +
  labs(x = substitute("Body mass, " * italic("M")),
       y = substitute("Metabolic rate, " * italic("B")),
       subtitle = substitute("At fixed temperature (= " * italic(Y) * ")")) +
  geom_segment(mapping = aes(x = 4e2, xend = 6e2, y = 500, yend = 660),
               arrow = arrow(angle = 40, length = unit(0.15, "cm")),
               linewidth = 0.2) +
  annotate(geom = "text", x = 470, y = 625, hjust = 0, vjust = 0, size = 4.5,
           label = deparse(substitute(italic("M")^alpha)), parse = TRUE) +
  scale_y_continuous(
    breaks = b_o,
    labels = expression(paste(italic("b"["o"]), "(", italic("T"["c"]), ")"))
  ) +
  scale_x_continuous(breaks = 1) +
  theme_classic() +
  theme(
     plot.subtitle = element_text(size = .pt * 4.5, colour = "black"),
     axis.title = element_text(size = .pt * 5, colour = "black"),
     axis.text = element_text(size = .pt * 4.5, colour = "black")
  ) +
  coord_cartesian(xlim = c(1, max(M) + 50), ylim = c(-10, 900),
                  expand = FALSE)

fig_1b <- ggplot(data = data.frame(x = T, y = B_2)) +
  geom_line(mapping = aes(x, y), linetype = 2) +
  labs(x = substitute("Temperature, " * italic("T")),
       y = "",
       subtitle = substitute("At fixed mass (= " * italic(Z) * ")")) +
  geom_segment(
    mapping = aes(
      x = min(T[B_2 >= 5 & B_2 <= 7 & T < T_opt]),
      xend = max(T[B_2 >= 5 & B_2 <= 7 & T < T_opt]),
      y = min(B_2[B_2 >= 5 & B_2 <= 7 & T < T_opt] + 0.8),
      yend = max(B_2[B_2 >= 5 & B_2 <= 7 & T < T_opt] + 0.8)
    ), arrow = arrow(angle = 40, length = unit(0.15, "cm")), linewidth = 0.2
  ) +
  annotate(geom = "text", x = 295, y = 7.6, hjust = 0.5, vjust = 0.5,
           size = 4.5,
           label = deparse(substitute(italic("E"["a"]))), parse = TRUE) +
  geom_segment(
    mapping = aes(
      x = min(T[B_2 >= 5 & B_2 <= 7.3 & T > T_opt]),
      xend = max(T[B_2 >= 5 & B_2 <= 7.3 & T > T_opt]),
      y = max(B_2[B_2 >= 5 & B_2 <= 7.3 & T > T_opt] + 0.8),
      yend = min(B_2[B_2 >= 5 & B_2 <= 7.3 & T > T_opt] + 0.8)
    ), arrow = arrow(angle = 40, length = unit(0.15, "cm")), linewidth = 0.2
  ) +
  annotate(geom = "text", x = 309, y = 7.5, hjust = 0.5, vjust = 0.5,
           size = 4.5,
           label = deparse(substitute(italic("E"["i"]))), parse = TRUE) +
  annotate(geom = "segment", x = T_c, xend = T_c, y = 0, yend = b_o,
           linetype = 3) +
  annotate(geom = "segment", x = 15 + 273.15, xend = T_c, y = b_o, yend = b_o,
           linetype = 3) +
  annotate(geom = "segment", x = T_opt, xend = T_opt, y = 0, yend = B_T_opt,
           linetype = 3) +
  scale_y_continuous(
    breaks = b_o,
    labels = expression(paste(italic("b"["o"]), "(", italic("T"["c"]), ")"))
  ) +
  scale_x_continuous(
    breaks = c(T_c, T_opt),
    labels = c(expression(paste(italic("T"["c"]))),
               expression(paste(italic("T"["opt"]))))
  ) +
  theme_classic() +
  theme(
     plot.subtitle = element_text(size = .pt * 4.5, colour = "black"),
     axis.title = element_text(size = .pt * 5, colour = "black"),
     axis.text = element_text(size = .pt * 4.5, colour = "black")
  ) +
  coord_cartesian(xlim = c(15, 41) + 273.15, ylim = c(2, B_T_opt + 1),
                  expand = FALSE)

ggsave("output/figure_1.pdf", fig_1a + fig_1b, width = 7.13, height = 3.23)

# Figure 2
set.seed(2)
M <- exp(rnorm(1e3, log(500)))
set.seed(10)
B_hetero_error <- rnorm(length(M), log(met_rates_m(b_o, alpha, M)), 0.2)
fig_2a_df <- data.frame(x = M, y = exp(B_hetero_error))
priors <- prior(normal(0, 1), nlpar = "botc") +
  prior(normal(0, 1), nlpar = "alpha")
fig_2a_mod <- brms::brm(
  bf(y ~ botc * x^alpha, botc + alpha ~ 1, nl = TRUE),
  data = fig_2a_df, family = Gamma(link = "identity"), prior = priors,
  chains = 4, iter = 2000, warmup = 1000, thin = 1, seed = 10,
  cores = 4
)
cols <- c("Estimate", "Q2.5", "Q97.5")
fig_2a_botc <- rounded(brms::fixef(fig_2a_mod)["botc_Intercept", cols], 2)
fig_2a_alpha <- rounded(brms::fixef(fig_2a_mod)["alpha_Intercept", cols], 2)
fake_x <- data.frame(
  x = seq(min(fig_2a_df$x), max(fig_2a_df$x), length.out = 100)
)
preds_g <- cbind(fake_x, data.frame(predict(fig_2a_mod, newdata = fake_x)))
fig_2b_df <- log(fig_2a_df)
priors <- prior(normal(0, 1), class = "b")
fig_2b_mod <- brms::brm(
  y ~ 0 + Intercept + x, data = fig_2b_df, family = gaussian(),
  prior = priors, chains = 4, iter = 2000, warmup = 1000, thin = 1, seed = 10,
  cores = 4
)
cols <- c("Estimate", "Q2.5", "Q97.5")
fig_2b_botc <- rounded(brms::fixef(fig_2b_mod)["Intercept", cols], 2)
fig_2b_alpha <- rounded(brms::fixef(fig_2b_mod)["x", cols], 2)
fake_x <- data.frame(
  x = seq(min(fig_2b_df$x), max(fig_2b_df$x), length.out = 100)
)
preds_n <- cbind(fake_x, data.frame(predict(fig_2b_mod, newdata = fake_x)))
fig_2a <- ggplot() +
  geom_ribbon(
    data = preds_g, mapping = aes(x = x, ymin = `Q2.5`, ymax = `Q97.5`),
    fill = "grey90", alpha = 0.8
  ) +
  geom_point(data = fig_2a_df, mapping = aes(x, y), alpha = 0.5, size = 2) +
  geom_line(
    data = preds_g, mapping = aes(x = x, y = Estimate),
    colour = "tomato", linetype = 2
  ) +
  labs(x = substitute("Body mass, " * italic("M")),
       y = substitute("Metabolic rate, " * italic("B")),
       subtitle = substitute(
         italic("b"["o"]) * "(" * italic("T"["c"]) * ") = 5; " * alpha *
           " = 0.75"
       )
  ) +
  annotate(
    geom = "text", x = 0, y = 7500, hjust = 0, vjust = 0.5, size = 3,
    label = paste0(deparse(
      substitute(
        italic("B") == a %.% italic("M")^b,
        list(a = fig_2a_botc[1], b = fig_2a_alpha[1])
      )
    ), collapse = ""), parse = TRUE
  ) +
  annotate(
    geom = "text", x = 0, y = 6800, hjust = 0, vjust = 0.5, size = 3,
    label = paste0(deparse(
      substitute(
        "95% C.I. " * italic("b"["o"]) * "(" * italic("T"["c"]) * "): " *
          a - b, list(a = fig_2a_botc[2], b = fig_2a_botc[3])
      )
    ), collapse = ""), parse = TRUE
  ) +
  annotate(
    geom = "text", x = 0, y = 6100, hjust = 0, vjust = 0.5, size = 3,
    label = paste0(deparse(
      substitute(
        "95% C.I. " * alpha * ": " * a - b,
        list(a = fig_2a_alpha[2], b = fig_2a_alpha[3])
      )
    ), collapse = ""), parse = TRUE
  ) +
  theme_classic() +
  theme(
     axis.title = element_text(size = .pt * 4, colour = "black"),
     axis.text = element_text(size = .pt * 3, colour = "black")
  )

fig_2b <- ggplot() +
  geom_ribbon(
    data = preds_n, mapping = aes(x = x, ymin = `Q2.5`, ymax = `Q97.5`),
    fill = "grey90", alpha = 0.8
  ) +
  geom_point(data = fig_2b_df, mapping = aes(x, y), alpha = 0.5, size = 2) +
  geom_line(
    data = preds_n, mapping = aes(x = x, y = Estimate),
    colour = "tomato", linetype = 2
  ) +
  labs(x = substitute("ln Body mass, ln" * italic("M")),
       y = substitute("ln Metabolic rate, ln" * italic("B"))) +
  annotate(
    geom = "text", x = 3, y = 8.6, hjust = 0, vjust = 0.5, size = 3,
    label = paste0(deparse(
      substitute(
        "ln" * italic("B") == a + b %.% "ln" * italic("M"),
        list(a = fig_2b_botc[1], b = fig_2b_alpha[1])
      )
    ), collapse = ""), parse = TRUE
  ) +
  annotate(
    geom = "text", x = 3, y = 8.1, hjust = 0, vjust = 0.5, size = 3,
    label = paste0(deparse(
      substitute(
        "95% C.I. " * "ln" * italic("b"["o"]) * "(" * italic("T"["c"]) *
          "): " * a - b, list(a = fig_2b_botc[2], b = fig_2b_botc[3])
      )
    ), collapse = ""), parse = TRUE
  ) +
  annotate(
    geom = "text", x = 3, y = 7.6, hjust = 0, vjust = 0.5, size = 3,
    label = paste0(deparse(
      substitute(
        "95% C.I. " * alpha * ": " * a - b,
        list(a = fig_2b_alpha[2], b = fig_2b_alpha[3])
      )
    ), collapse = ""), parse = TRUE
  ) +
  theme_classic() +
  theme(
     axis.title = element_text(size = .pt * 4, colour = "black"),
     axis.text = element_text(size = .pt * 3, colour = "black")
  )

ggsave("output/figure_2.pdf", fig_2a + fig_2b, width = 7.13, height = 3.23)
