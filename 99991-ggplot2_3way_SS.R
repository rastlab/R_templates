## this script is meant to be sourced from other scripts ##

###########################################
####### Run code but do not modify ########
###########################################

# Fit 3-way regression model from dat (expects dat$y, dat$z, dat$x, dat$w)
step3 <- lm(y ~ z * x * w, data = dat)

# Extract 3-way interaction probe data
data_plot <- interactions::probe_interaction(
  step3,
  pred = z,
  modx = x,
  mod2 = w,
  modx.values = "plus-minus",
  mod2.values = "plus-minus")

# APA Publication Theme
apatheme <- theme_bw(base_family = "Times") +
                theme(
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  axis.line = element_line(),
                  legend.position = legend_loc,
                  legend.background = element_rect(fill = "transparent", color = NA))

# Extract underlying plot data frame
plot_df <- data_plot$interactplot$data

# Panel A: Low Level of Secondary Moderator (-1 SD)
panel_a <- plot_df %>%
              dplyr::filter(stringr::str_detect(mod2_group, "- 1 SD|-1 SD")) %>%
              ggplot(aes(x = z, y = y, group = modx_group)) +
              geom_line(
                aes(color = modx_group, linetype = modx_group),
                linewidth = 1.25) +
              scale_y_continuous(
                limits = c(y_axis_low, y_axis_high),
                breaks = seq(y_axis_low, y_axis_high, by = y_increment)) +
              # Restricts x-axis limits and tick marks strictly to -1SD and +1SD
              scale_x_continuous(
                limits = z_range,
                breaks = z_range,
                labels = z_values) +
              scale_linetype_manual(
                name = modx_label,
                labels = modx_values,
                values = line_types,
                guide = guide_legend(reverse = TRUE)) +
              scale_colour_manual(
                name = modx_label,
                labels = modx_values,
                values = line_colors,
                guide = guide_legend(reverse = TRUE)) +
              labs(x = z_label, y = y_label, subtitle = panel_a_label) +
              apatheme

# Panel B: High Level of Secondary Moderator (+1 SD)
panel_b <- plot_df %>%
              dplyr::filter(stringr::str_detect(mod2_group, "\\+ 1 SD|\\+1 SD")) %>%
              ggplot(aes(x = z, y = y, group = modx_group)) +
              geom_line(
                aes(color = modx_group, linetype = modx_group),
                linewidth = 1.25) +
              scale_y_continuous(
                limits = c(y_axis_low, y_axis_high),
                breaks = seq(y_axis_low, y_axis_high, by = y_increment)) +
              # Restricts x-axis limits and tick marks strictly to -1SD and +1SD
              scale_x_continuous(
                limits = z_range,
                breaks = z_range,
                labels = z_values) +
              scale_linetype_manual(
                name = modx_label,
                labels = modx_values,
                values = line_types,
                guide = guide_legend(reverse = TRUE)) +
              scale_colour_manual(
                name = modx_label,
                labels = modx_values,
                values = line_colors,
                guide = guide_legend(reverse = TRUE)) +
              labs(x = z_label, y = y_label, subtitle = panel_b_label) +
              apatheme

# Combine panels into a stacked 2-panel figure via patchwork
panel_a_b <- panel_a / panel_b