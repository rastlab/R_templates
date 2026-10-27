
## this script is meant to be sourced from other scripts ##

###########################################
####### Run code but do not modify ########
###########################################

# Helper script: 2-way APA interaction plot generator
# Sourced from primary analysis scripts

#########################################################################
###### 1. Defensive Fallbacks for Plotting Variables ####################
#########################################################################

if (!exists("dat")) stop("Error: 'dat' object not found in environment.")

# Set default fallbacks if plotting parameters are omitted in parent script
y_label      <- if (exists("y_label")) y_label else "Dependent Variable"
y_axis_low   <- if (exists("y_axis_low")) y_axis_low else floor(min(dat$dv, na.rm = TRUE))
y_axis_high  <- if (exists("y_axis_high")) y_axis_high else ceiling(max(dat$dv, na.rm = TRUE))
y_increment  <- if (exists("y_increment")) y_increment else 1
x_label      <- if (exists("x_label")) x_label else "IV1"
x_values     <- if (exists("x_values")) x_values else levels(factor(dat$iv1))
mod_label    <- if (exists("mod_label")) mod_label else "IV2"
mod_values   <- if (exists("mod_values")) mod_values else levels(factor(dat$iv2))
legend_loc   <- if (exists("legend_loc")) legend_loc else c(0.8, 0.8)

#########################################################################
###### 2. Numerical Parity Summary Calculation ##########################
#########################################################################

# Identical calculation to psych::describeBy(dat$dv, list(dat$iv1, dat$iv2))
dat1 <- dat %>%
  dplyr::drop_na(iv1, iv2, dv) %>%
  dplyr::group_by(iv1, iv2) %>%
  dplyr::summarise(
    n    = dplyr::n(),
    mean = mean(dv, na.rm = TRUE),
    sd   = sd(dv, na.rm = TRUE),
    se   = sd / sqrt(n),
    .groups = "drop"
  )

#########################################################################
###### 3. Visual Parity APA Theme & Layout Defaults #####################
#########################################################################

# Matches legacy apatheme (theme_bw with panel border removed)
apa_theme <- ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border     = ggplot2::element_blank(),
    axis.line        = ggplot2::element_line(color = "black"),
    axis.text        = ggplot2::element_text(color = "black", size = 10),
    axis.title       = ggplot2::element_text(size = 11, face = "bold"),
    legend.position  = legend_loc,
    legend.title     = ggplot2::element_text(size = 10, face = "bold")
  )

dodge_val <- ggplot2::position_dodge(width = 0.9)

#########################################################################
###### 4. Figure 1: IV1 on X-Axis, IV2 as Moderator #####################
#########################################################################

figure_1 <- dat1 %>%
  dplyr::mutate(
    iv1 = forcats::fct_reorder(factor(iv1), desc(iv1)),
    iv2 = forcats::fct_reorder(factor(iv2), desc(iv2))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = iv1, y = mean, fill = iv2, group = iv2)) +
  ggplot2::geom_col(position = dodge_val, width = 0.9) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = mean - (1.96 * se), ymax = mean + (1.96 * se)),
    position = dodge_val,
    width = 0.25
  ) +
  ggplot2::coord_cartesian(ylim = c(y_axis_low, y_axis_high)) +
  ggplot2::scale_y_continuous(breaks = seq(y_axis_low, y_axis_high, by = y_increment)) +
  ggplot2::scale_x_discrete(name = x_label, labels = x_values) +
  ggplot2::scale_fill_grey() +
  ggplot2::labs(y = y_label, fill = mod_label) +
  apa_theme

#########################################################################
###### 5. Figure 2: IV2 on X-Axis, IV1 as Moderator #####################
#########################################################################

figure_2 <- dat1 %>%
  dplyr::mutate(
    iv1 = forcats::fct_reorder(factor(iv1), desc(iv1)),
    iv2 = forcats::fct_reorder(factor(iv2), desc(iv2))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = iv2, y = mean, fill = iv1, group = iv1)) +
  ggplot2::geom_col(position = dodge_val, width = 0.9) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = mean - (1.96 * se), ymax = mean + (1.96 * se)),
    position = dodge_val,
    width = 0.25
  ) +
  ggplot2::coord_cartesian(ylim = c(y_axis_low, y_axis_high)) +
  ggplot2::scale_y_continuous(breaks = seq(y_axis_low, y_axis_high, by = y_increment)) +
  ggplot2::scale_x_discrete(name = mod_label, labels = mod_values) +
  ggplot2::scale_fill_grey() +
  ggplot2::labs(y = y_label, fill = x_label) +
  apa_theme