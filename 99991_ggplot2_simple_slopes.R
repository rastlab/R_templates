#####################################
### Import data & load libraries ####
#####################################

# load packages
# pacman::p_load(rio, pequod, tidyverse, janitor, QuantPsyc, lmSupport, jtools, interactions, ggstance, patchwork)

# import data
# dat <- import(file.choose())

# #####################################################################################
# ######################## Simple Slope Testing Automatically #########################
# #####################################################################################
# 
# ### create x, z, w, and y columns by renaming IVs
# dat$z <- NON-MODERATOR_HERE # x-axis variable here
# dat$x <- MODERATOR_HERE     # moderator_1 variable here
# dat$w <- PANEL_HERE         # moderator_2 variable here
# dat$y <- DV_HERE            # outcome variable here
# 
# 
# ## create labels for figure
# # must use quotes for labels
# # change labels in quotes to be what you want them to be
# 
# panel_a_label  <- "Panel A: YYY"         # panel A = low moderator
# panel_b_label  <- "Panel B: ZZZ"         # panel B = high moderator
# y_label        <- "dv_name"
# y_range        <- c(-1.0, 1.0)           # desired numeric range of y-axis
# y_axis_high    <- 1.0                    # high descrete numeric value displayed on y-axis
# y_axis_low     <- -1.0                    # low descrete numeric value displayed on y-axis
# z_label        <- "z_non-moderator_name" # X-axis variable (non-moderator)
# z_values       <- c("low", "high")       # non-moderator values
# z_range        <- c(-1.0, 1.0)           # non-moderator numerical values
# modx_label     <- "x_moderator_name"     # figure legend (moderator)
# modx_values    <- c("low", "high")       # moderator values
# 
# # run but only modify these if needed
# # line type and color are set for APA 7ed
# line_types    <- c("longdash", "solid")   # can be “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”
# line_colors   <- c("#9E9E9E", "#000000")  # these are colorblind friendly color schemes, don't change unless needed
# 
# # change legend location if needed
# # in the format of (x,y), can be any number between 0 and 1
# # (0.8, 0.8) is upper-right corner, (0.2, 0.8) is the top left of the figure
# legend_loc    <- c(0.2, 0.8)

###########################################
####### Run code but do not modify ########
###########################################

# regression
step3 <- lm(y ~ z * x * w, data=dat)
summary(step3)

# create figure
data_plot <- probe_interaction(step3, 
                               pred = z, 
                               modx = x, 
                               mod2 = w,
                               modx.values = "plus-minus",
                               mod2.values = "plus-minus")

### tells R to provide a plot in APA style, figure is MS ready
apatheme = theme_bw() +
           theme(panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),
                 panel.border=element_blank(),
                 axis.line=element_line(),
                 text=element_text(family='Times'))

# create Panel A
panel_a <- data_plot$interactplot$data %>%
                filter(mod2_group == "Mean of w - 1 SD") %>%
                ggplot(aes(x = z, y = y, group = modx_group)) +
                geom_line(aes(color = modx_group,
                              linetype = modx_group),
                          size = 1.25) +
                scale_y_continuous(limits = y_range,
                                   breaks = seq(y_axis_low, y_axis_high)) + 
                labs(subtitle = panel_a_label) +
                ylab(y_label) +
                xlab(z_label) +
                scale_x_discrete(limits = z_range,
                                 labels = z_values) +  
                scale_linetype_manual(name = modx_label,
                                      labels = modx_values,
                                      values = line_types,
                                      guide = guide_legend(reverse = TRUE)) +
                scale_colour_manual(name = modx_label,
                                    labels = modx_values,
                                    values = line_colors,
                                    guide = guide_legend(reverse = TRUE)) +
                apatheme +
                theme(legend.position = legend_loc) +
                scale_fill_grey()

ggsave('figure1_panel_a.png', width=8, height=6, unit='in', dpi=300)

# create Panel B
panel_b <- data_plot$interactplot$data %>%
                filter(mod2_group == "Mean of w + 1 SD") %>%
                ggplot(aes(x = z, y = y, group = modx_group)) +
                geom_line(aes(color = modx_group,
                              linetype = modx_group),
                          size = 1.25) +
                scale_y_continuous(limits = y_range,
                                   breaks = seq(y_axis_low, y_axis_high)) + 
                labs(subtitle = panel_b_label) +
                ylab(y_label) +
                xlab(z_label) +
                scale_x_discrete(limits = z_range,
                                 labels = z_values) +  
                scale_linetype_manual(name = modx_label,
                                      labels = modx_values,
                                      values = line_types,
                                      guide = guide_legend(reverse = TRUE)) +
                scale_colour_manual(name = modx_label,
                                    labels = modx_values,
                                    values = line_colors,
                                    guide = guide_legend(reverse = TRUE)) +
                apatheme +
                theme(legend.position = legend_loc) +
                scale_fill_grey()

ggsave('figure1_panel_b.png', width=8, height=6, unit='in', dpi=300)

######################################
####### Here are your figures ########
######################################

# panel_a
# panel_b 

figure_1 <- panel_a / panel_b

ggsave('figure1.png', width=8, height=6, unit='in', dpi=300)




