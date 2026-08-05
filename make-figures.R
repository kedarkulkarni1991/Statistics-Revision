## Figures for the revision notes.
## Run with:  Rscript make-figures.R
## Writes PNGs into figures/. Every figure in the document comes from here,
## so a change to the styling below propagates everywhere.

library(ggplot2)
dir.create("figures", showWarnings = FALSE)

ink    <- "#1f2933"
accent <- "#2b6cb0"
warm   <- "#c05621"
fill   <- "#bee3f8"

theme_rev <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title = element_text(colour = "grey25"),
          plot.title = element_text(size = base, face = "plain"))
}

save_fig <- function(name, plot, w = 6.5, h = 3.6) {
  ggsave(file.path("figures", paste0(name, ".png")), plot,
         width = w, height = h, dpi = 200, bg = "white")
}

## ---- 1. Mean, median, mode on a skewed distribution ------------------------
set.seed(1)
inc <- rlnorm(4000, meanlog = 10.2, sdlog = 0.8) / 1000
dens <- density(inc, from = 0, to = 200)
dd <- data.frame(x = dens$x, y = dens$y)
mo <- dens$x[which.max(dens$y)]

p <- ggplot(dd, aes(x, y)) +
  geom_area(fill = fill, alpha = 0.6) +
  geom_line(colour = ink, linewidth = 0.7) +
  geom_vline(xintercept = mo,          colour = "#2f855a", linewidth = 0.8) +
  geom_vline(xintercept = median(inc), colour = accent,    linewidth = 0.8) +
  geom_vline(xintercept = mean(inc),   colour = warm,      linewidth = 0.8) +
  annotate("text", x = mo,          y = max(dd$y)*1.02, label = "mode",   colour = "#2f855a", hjust = 1.15, size = 3.4) +
  annotate("text", x = median(inc), y = max(dd$y)*0.88, label = "median", colour = accent,    hjust = -0.12, size = 3.4) +
  annotate("text", x = mean(inc),   y = max(dd$y)*0.74, label = "mean",   colour = warm,      hjust = -0.12, size = 3.4) +
  coord_cartesian(xlim = c(0, 160)) +
  labs(x = "Monthly household income (thousand rupees)", y = NULL) +
  theme_rev() + theme(axis.text.y = element_blank())
save_fig("central-tendency", p)

## ---- 2. Same centre, different spread --------------------------------------
g <- seq(20, 80, length.out = 400)
sp <- rbind(
  data.frame(x = g, y = dnorm(g, 50, 4),  s = "Small spread (SD = 4)"),
  data.frame(x = g, y = dnorm(g, 50, 12), s = "Large spread (SD = 12)"))

p <- ggplot(sp, aes(x, y, colour = s)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 50, linetype = "dashed", colour = "grey45") +
  scale_colour_manual(values = c("Small spread (SD = 4)" = accent,
                                 "Large spread (SD = 12)" = warm)) +
  labs(x = "Marks", y = NULL, colour = NULL) +
  theme_rev() + theme(axis.text.y = element_blank(), legend.position = "top")
save_fig("dispersion", p)

## ---- 3. Correlation at five strengths --------------------------------------
set.seed(7)
make_r <- function(r, n = 120) {
  z1 <- rnorm(n); z2 <- rnorm(n)
  data.frame(x = z1, y = r * z1 + sqrt(max(0, 1 - r^2)) * z2)
}
rs <- c(-1, -0.7, 0, 0.7, 1)
cor_df <- do.call(rbind, lapply(rs, function(r) {
  d <- make_r(r); d$lab <- sprintf("r = %.1f", r); d }))
cor_df$lab <- factor(cor_df$lab, levels = sprintf("r = %.1f", rs))

p <- ggplot(cor_df, aes(x, y)) +
  geom_point(alpha = 0.5, size = 0.8, colour = ink) +
  facet_wrap(~ lab, nrow = 1) +
  labs(x = NULL, y = NULL) +
  theme_rev(10) +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        panel.border = element_rect(colour = "grey80", fill = NA))
save_fig("correlation-strengths", p, w = 6.8, h = 1.9)

## ---- 4. Two real scatter plots ---------------------------------------------
income <- c(2583,9166,15749,22322,28915,35498,42081,48664,55247,61830,68413,74996)
educ   <- 1:12
age    <- c(12,16,13,18,19,12,18,19,12,14)
games  <- c(73,67,74,63,73,84,60,62,76,61)

sc <- rbind(
  data.frame(x = educ,  y = income, p = sprintf("Education and income   (r = %.2f)", cor(educ, income))),
  data.frame(x = games, y = age,    p = sprintf("Video games and age   (r = %.2f)",  cor(games, age))))

p <- ggplot(sc, aes(x, y)) +
  geom_point(colour = accent, size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = warm, linewidth = 0.7) +
  facet_wrap(~ p, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_rev(10)
save_fig("scatter-examples", p, w = 6.5, h = 2.8)

## ---- 5. pmf of one die and of the sum of two dice ---------------------------
one <- data.frame(x = 1:6, p = rep(1/6, 6), panel = "One die")
sums <- outer(1:6, 1:6, "+")
two <- data.frame(x = 2:12, p = as.numeric(table(sums)) / 36, panel = "Sum of two dice")

p <- ggplot(rbind(one, two), aes(factor(x), p)) +
  geom_col(fill = fill, colour = accent, width = 0.7) +
  facet_wrap(~ panel, scales = "free_x") +
  labs(x = "x", y = "f(x)") +
  theme_rev(11)
save_fig("pmf-dice", p, w = 6.5, h = 2.8)

## ---- 6. Binomial pmf at three probabilities --------------------------------
bn <- do.call(rbind, lapply(c(0.2, 0.5, 0.8), function(pp)
  data.frame(x = 0:10, p = dbinom(0:10, 10, pp),
             lab = sprintf("n = 10, p = %.1f", pp))))

p <- ggplot(bn, aes(factor(x), p)) +
  geom_col(fill = fill, colour = accent, width = 0.75) +
  facet_wrap(~ lab) +
  labs(x = "Number of successes", y = "f(x)") +
  theme_rev(10)
save_fig("binomial-pmf", p, w = 6.8, h = 2.5)

## ---- 7. Poisson pmf at three rates -----------------------------------------
po <- do.call(rbind, lapply(c(1, 3, 8), function(l)
  data.frame(x = 0:16, p = dpois(0:16, l), lab = sprintf("lambda = %d", l))))
po$lab <- factor(po$lab, levels = sprintf("lambda = %d", c(1, 3, 8)))

p <- ggplot(po, aes(x, p)) +
  geom_col(fill = fill, colour = accent, width = 0.75) +
  facet_wrap(~ lab) +
  labs(x = "Number of events", y = "f(x)") +
  theme_rev(10)
save_fig("poisson-pmf", p, w = 6.8, h = 2.5)

## ---- 8. A pdf, with P(a < X < b) shaded ------------------------------------
xg <- seq(0, 12, length.out = 500)
pdfd <- data.frame(x = xg, y = xg / 72)
shade <- subset(pdfd, x >= 4 & x <= 8)

p <- ggplot(pdfd, aes(x, y)) +
  geom_area(data = shade, fill = fill) +
  geom_line(colour = ink, linewidth = 0.9) +
  annotate("text", x = 6, y = 0.035, label = "P(4 < X < 8)", size = 3.6, colour = ink) +
  annotate("segment", x = 4, xend = 4, y = 0, yend = 4/72, colour = accent, linewidth = 0.4) +
  annotate("segment", x = 8, xend = 8, y = 0, yend = 8/72, colour = accent, linewidth = 0.4) +
  scale_x_continuous(breaks = c(0, 4, 8, 12), labels = c("0", "a", "b", "12")) +
  labs(x = "t (minutes)", y = "f(t)") +
  theme_rev()
save_fig("pdf-area", p, w = 6.0, h = 3.0)

## ---- 9. Uniform distribution ------------------------------------------------
ug <- data.frame(x = c(0, 200, 200, 250, 250, 320),
                 y = c(0, 0, 1/50, 1/50, 0, 0))
p <- ggplot(ug, aes(x, y)) +
  geom_area(data = data.frame(x = c(200, 250), y = c(1/50, 1/50)), fill = fill) +
  geom_path(colour = ink, linewidth = 0.9) +
  annotate("text", x = 225, y = 0.010, label = "area = 1", size = 3.6) +
  annotate("text", x = 225, y = 0.0215, label = "height = 1/(b - a)", size = 3.2, colour = "grey30") +
  scale_x_continuous(breaks = c(200, 250), labels = c("a = 200", "b = 250")) +
  labs(x = "Match duration (minutes)", y = "f(x)") +
  theme_rev()
save_fig("uniform", p, w = 6.0, h = 2.8)

## ---- 10. Normal curves: location and scale ---------------------------------
ng <- seq(40, 110, length.out = 500)
nn <- rbind(
  data.frame(x = ng, y = dnorm(ng, 70, 8),  lab = "N(70, 8²)"),
  data.frame(x = ng, y = dnorm(ng, 82, 6),  lab = "N(82, 6²)"),
  data.frame(x = ng, y = dnorm(ng, 70, 14), lab = "N(70, 14²)"))

p <- ggplot(nn, aes(x, y, colour = lab)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c("N(70, 8²)" = accent, "N(82, 6²)" = "#2f855a",
                                 "N(70, 14²)" = warm)) +
  labs(x = "Marks", y = NULL, colour = NULL) +
  theme_rev() + theme(axis.text.y = element_blank(), legend.position = "top")
save_fig("normal-shapes", p)

## ---- 11. The empirical rule -------------------------------------------------
eg <- seq(-4, 4, length.out = 600)
ed <- data.frame(z = eg, y = dnorm(eg))
band <- function(a, b, alpha) geom_area(data = subset(ed, z >= a & z <= b),
                                        aes(z, y), fill = accent, alpha = alpha)

## Labels sit on brackets below the axis rather than on the curve, where the
## outer two would otherwise overlap the tail.
br <- data.frame(k = 1:3, y = c(-0.045, -0.085, -0.125),
                 lab = c("68%", "95%", "99.7%"))

p <- ggplot(ed, aes(z, y)) +
  band(-3, 3, 0.15) + band(-2, 2, 0.22) + band(-1, 1, 0.32) +
  geom_line(colour = ink, linewidth = 0.9) +
  geom_segment(data = br, aes(x = -k, xend = k, y = y, yend = y),
               colour = "grey35", linewidth = 0.4) +
  geom_segment(data = br, aes(x = -k, xend = -k, y = y, yend = y + 0.012),
               colour = "grey35", linewidth = 0.4) +
  geom_segment(data = br, aes(x = k, xend = k, y = y, yend = y + 0.012),
               colour = "grey35", linewidth = 0.4) +
  geom_text(data = br, aes(x = 0, y = y, label = lab), vjust = -0.35,
            size = 3.4, colour = "grey20") +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  scale_x_continuous(breaks = -3:3,
    labels = c(expression(mu-3*sigma), expression(mu-2*sigma), expression(mu-sigma),
               expression(mu), expression(mu+sigma), expression(mu+2*sigma),
               expression(mu+3*sigma))) +
  coord_cartesian(ylim = c(-0.14, 0.42)) +
  labs(x = NULL, y = NULL) +
  theme_rev() +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
save_fig("empirical-rule", p, w = 6.5, h = 3.4)

## ---- 12. Standard normal: a left-tail area ---------------------------------
zg <- seq(-4, 4, length.out = 600)
zd <- data.frame(z = zg, y = dnorm(zg))
p <- ggplot(zd, aes(z, y)) +
  geom_area(data = subset(zd, z <= 1.22), fill = fill) +
  geom_line(colour = ink, linewidth = 0.9) +
  geom_vline(xintercept = 1.22, colour = warm, linewidth = 0.6) +
  annotate("text", x = -0.4, y = 0.15, label = "P(Z < 1.22) = 0.8888", size = 3.6) +
  annotate("text", x = 1.22, y = 0.42, label = "z = 1.22", size = 3.3,
           colour = warm, hjust = -0.1) +
  labs(x = "Z", y = NULL) +
  theme_rev() + theme(axis.text.y = element_blank())
save_fig("z-tail", p, w = 6.0, h = 3.0)

## ---- 13. The x-to-z transformation -----------------------------------------
mu <- 520; sg <- 94
xs <- seq(mu - 4*sg, mu + 4*sg, length.out = 500)
tr <- rbind(
  data.frame(v = xs, y = dnorm(xs, mu, sg), panel = "Original scale: X ~ N(520, 94²)"),
  data.frame(v = (xs - mu)/sg, y = dnorm((xs - mu)/sg), panel = "Standardised: Z ~ N(0, 1)"))
cuts <- data.frame(v = c(700, (700 - mu)/sg),
                   panel = c("Original scale: X ~ N(520, 94²)", "Standardised: Z ~ N(0, 1)"),
                   lab = c("x = 700", "z = 1.91"))

p <- ggplot(tr, aes(v, y)) +
  geom_line(colour = ink, linewidth = 0.8) +
  geom_vline(data = cuts, aes(xintercept = v), colour = warm, linewidth = 0.6) +
  geom_text(data = cuts, aes(x = v, y = Inf, label = lab), vjust = 1.6, hjust = -0.08,
            colour = warm, size = 3.2) +
  facet_wrap(~ panel, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_rev(10) + theme(axis.text.y = element_blank())
save_fig("standardising", p, w = 6.8, h = 2.6)

message("figures written to figures/")
