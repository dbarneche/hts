library(tidyverse)
library(patchwork)
library(brms)
library(ggdist)
options(mc.cores = parallel::detectCores())
dir.create("output", recursive = TRUE, showWarnings = FALSE)

annotate_text <- function(label, x, y, facets = NULL, hjust = 0,
                          vjust = 0, color = "black", alpha = NA,
                          family = thm$text$family,
                          size = thm$text$size, fontface = 1, line_height = 1.0,
                          box_just = ifelse(c(x, y) < 0.5, 0, 1),
                          margin = grid::unit(size / 2, "pt"),
                          thm = theme_get()) {
# from stackoverflow
# question 22488563
# ggplot2-annotate-layer-position-in-r
  x <- scales::squish_infinite(x)
  y <- scales::squish_infinite(y)

  tg <- grid::textGrob(label, x = 0, y = 0, hjust = hjust, vjust = vjust,
                       gp = grid::gpar(col = alpha(color, alpha),
                                       fontsize = size, fontfamily = family,
                                       fontface = fontface,
                                       lineheight = line_height))
  ts <- grid::unit.c(grid::grobWidth(tg), grid::grobHeight(tg))
  vp <- grid::viewport(x = x, y = y, width = ts[1], height = ts[2],
                       just = box_just)
  tg <- grid::editGrob(tg, x = ts[1] * hjust, y = ts[2] * vjust, vp = vp)
  unt <- grid::unit(1, "npc") - margin * 2
  inr <- grid::grobTree(tg, vp = grid::viewport(width = unt, height = unt))

  layer(data = NULL, stat = StatIdentity, position = PositionIdentity,
        geom = GeomCustomAnn, inherit.aes = TRUE,
        params = list(grob = grid::grobTree(inr),
                      xmin = -Inf,
                      xmax = Inf,
                      ymin = -Inf,
                      ymax = Inf))
}

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
fig_2a_priors <- prior(normal(0, 1), nlpar = "botc") +
  prior(normal(0, 1), nlpar = "alpha")
fig_2a_mod <- brms::brm(
  bf(y ~ botc * x^alpha, botc + alpha ~ 1, nl = TRUE),
  data = fig_2a_df, family = Gamma(link = "identity"), prior = fig_2a_priors,
  chains = 4, iter = 2000, warmup = 1000, thin = 1, seed = 10,
  cores = 4, save_pars = TRUE
)
cols <- c("Estimate", "Q2.5", "Q97.5")
fig_2a_botc <- rounded(brms::fixef(fig_2a_mod)["botc_Intercept", cols], 2)
fig_2a_alpha <- rounded(brms::fixef(fig_2a_mod)["alpha_Intercept", cols], 2)
fake_x <- data.frame(
  x = seq(min(fig_2a_df$x), max(fig_2a_df$x), length.out = 100)
)
preds_g <- cbind(fake_x, data.frame(predict(fig_2a_mod, newdata = fake_x)))
fig_2b_df <- log(fig_2a_df)
fig_2b_priors <- prior(normal(0, 1), class = "b")
fig_2b_mod <- brms::brm(
  y ~ 0 + Intercept + x, data = fig_2b_df, family = gaussian(),
  prior = fig_2b_priors, chains = 4, iter = 2000, warmup = 1000, thin = 1,
  seed = 10, cores = 4, save_all_pars = TRUE
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

fig_3_null_priors <- prior(normal(0, 1), class = "b", coef = "Intercept") +
  prior(normal(0.67, 0.01), class = "b", coef = "x")
fig_3_null_mod <- brms::brm(
  y ~ 0 + Intercept + x, data = fig_2b_df, family = gaussian(),
  prior = fig_3_null_priors, chains = 4, iter = 2e4, warmup = 1e4, thin = 1,
  seed = 10, cores = 4, save_pars = save_pars(all = TRUE),
  sample_prior = "yes"
)

fig_3_alt_priors <- prior(normal(0, 1), class = "b", coef = "Intercept") +
  prior(normal(0.75, 0.01), class = "b", coef = "x")
fig_3_alt_mod <- brms::brm(
  y ~ 0 + Intercept + x, data = fig_2b_df, family = gaussian(),
  prior = fig_3_alt_priors, chains = 4, iter = 2e4, warmup = 1e4, thin = 1,
  seed = 10, cores = 4, save_pars = save_pars(all = TRUE),
  sample_prior = "yes"
)

options(mc.cores = 1)
set.seed(10)
bf_mods_fig_3 <- brms::bayes_factor(fig_3_alt_mod, fig_3_null_mod)
set.seed(10)
pp_mods_fig_3 <- brms::post_prob(fig_3_alt_mod, fig_3_null_mod, prior_prob = c(0.5, 0.5))

# So in the case of equal prior probabilities, BF == Posterior odds
all.equal(bf_mods_fig_3$bf, pp_mods_fig_3[[1]] / pp_mods_fig_3[[2]])

prior_df <- cbind(
  brms::as_draws_df(fig_3_null_mod) %>%
    dplyr::select(M1 = prior_b_x),
  brms::as_draws_df(fig_3_alt_mod) %>%
    dplyr::select(M2 = prior_b_x)
) %>%
  dplyr::mutate(Draw = seq_len(n())) %>%
  tidyr::pivot_longer(
    cols = c("M1", "M2"), names_to = "Model"
  )

post_df <- cbind(
  brms::as_draws_df(fig_3_null_mod) %>%
    dplyr::select(M1 = b_x),
  brms::as_draws_df(fig_3_alt_mod) %>%
    dplyr::select(M2 = b_x)
) %>%
  dplyr::mutate(Draw = seq_len(n())) %>%
  tidyr::pivot_longer(
    cols = c("M1", "M2"), names_to = "Model"
  )

dens_pr <- ggplot(data = prior_df) +
  stat_halfeye(mapping = aes(x = value, fill = Model), adjust = 2) +
  scale_x_continuous(breaks = c(0.67, 0.75), limits = c(0.63, 0.79)) +
  labs(y = "Density", x = substitute(italic(alpha)), fill = "Model: ") +
  annotate_text(
    x = 0, y = 1.05,
    label = substitute("Prior distributions of " * italic(alpha))
  ) +
  scale_fill_manual(
    values = c("tomato", "dodgerblue3"),
    labels = c(
      expression(paste(italic("M"[1]))), expression(paste(italic("M"[2])))
    )
  ) +
  theme_classic() +
  theme(
    panel.background = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    plot.background = element_blank()
  )

dens_po <- ggplot(data = post_df) +
  stat_halfeye(mapping = aes(x = value, fill = Model), adjust = 2) +
  scale_x_continuous(breaks = c(0.67, 0.75), limits = c(0.63, 0.79)) +
  labs(y = "Density", x = substitute(italic(alpha)), fill = "Model: ") +
  annotate_text(
    x = 0, y = 1.05,
    label = substitute("Posterior distributions of " * italic(alpha))
  ) +
  scale_fill_manual(
    values = c("tomato", "dodgerblue3"),
    labels = c(
      expression(paste(italic("M"[1]))), expression(paste(italic("M"[2])))
    )
  ) +
  theme_classic() +
  theme(
    panel.background = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    plot.background = element_blank()
  )

prob_pr <- ggplot(data = data.frame(y = c(0.5, 0.5), x = 1:2)) +
  geom_point(mapping = aes(x, y), shape = c(15, 16), size = 3,
             colour = c("tomato", "dodgerblue3")) +
  annotate_text(x = 0, y = 1.05, label = "Prior model probs.") +
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(-0.05, 1.3)) +
  scale_x_continuous(
    breaks = c(1, 2), limits = c(-0, 3),
    labels = c(
      expression(paste(italic("M"[1]))), expression(paste(italic("M"[2])))
    )
  ) +
  labs(x = "", y = "") +
  theme_classic() +
  theme(
    axis.title = element_text(size = .pt * 4, colour = "black"),
    axis.text = element_text(size = .pt * 3, colour = "black"),
    panel.background = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    plot.background = element_blank()
  ) +
  coord_cartesian(clip = "off")

prob_po <- ggplot(data = data.frame(y = rev(pp_mods_fig_3), x = 1:2)) +
  geom_point(mapping = aes(x, y), shape = c(15, 16), size = 3,
             colour = c("tomato", "dodgerblue3")) +
  annotate_text(x = 0, y = 1.05, label = "Post. model probs.") +
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(-0.05, 1.3)) +
  scale_x_continuous(
    breaks = c(1, 2), limits = c(-0, 3),
    labels = c(
      expression(paste(italic("M"[1]))), expression(paste(italic("M"[2])))
    )
  ) +
  labs(x = "", y = "") +
  theme_classic() +
  theme(
    axis.title = element_text(size = .pt * 4, colour = "black"),
    axis.text = element_text(size = .pt * 3, colour = "black"),
    panel.background = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    plot.background = element_blank()
  ) +
  coord_cartesian(clip = "off")

layout <- "
#AAA#####BBB###
#AAA#####BBB###
#AAA#####BBB###
CCCCCCC#DDDDDDD
CCCCCCC#DDDDDDD
CCCCCCC#DDDDDDD
CCCCCCC#DDDDDDD
CCCCCCC#DDDDDDD
###############
"

fig_3 <- prob_pr + prob_po + dens_pr + dens_po +
  plot_layout(guides = "collect", design = layout) &
  theme(legend.position = "top")

ggsave("output/figure_3.pdf", fig_3, width = 7, height = 3.8)
