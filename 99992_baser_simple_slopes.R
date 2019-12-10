#####################################
### Import data & load libraries ####
#####################################

# load packages
pacman::p_load(rio, pequod, magick)

# import data
dat <- import(file.choose())

#####################################################################################
######################## Simple Slope Testing Automatically #########################
#####################################################################################

### create x, z, w, and y columns by renaming IVs
dat$z <- NON-MODERATOR_HERE # x-axis variable here
dat$x <- MODERATOR_HERE     # moderator_1 variable here
dat$w <- PANEL_HERE         # moderator_2 variable here
dat$y <- DV_HERE            # outcome variable here

## create labels for figure
# must use quotes for labels
# change labels to be what you want them to be

panel_a_label    <- "Panel A: YYY"  # panel A = low moderator
panel_b_label    <- "Panel B: ZZZ"  # panel B = high moderator
y_label          <- "dv_name"
z_label          <- "non-moderator_name"
z_values         <- c("low (-1SD)", "high (+1SD)")  # non-moderator values
modx_label       <- "moderator_name"
modx_values      <- c("high", "low")       # moderator values
legend_placement <- "topleft" # can be: "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center"
y_range = c(-1,1)  # this is to change y-axis range. The first number is the y-axis low point, the second number is the y-axis high point


###########################################
####### Run code but do not modify ########
###########################################

# create simple slopes using 'pequod'
# z as slope, x as moderator
model1 <- na.omit(lmres(y ~ z * x * w, data=dat))
s_slopes1 <- na.omit(simpleSlope(model1, pred = "z",mod1 = "x", mod2 = "w"))


#### Plot iv1 as slope and iv2 as moderator and iv3 (low -1SD) as panel
yrange = y_range  # modify the y-axis range
xrange = c(-1.5,1.5)
png(file="./figures/figure_1_br.png", width = 8, height = 6, units = "in", res = 800)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes1$Points[3, 1]), (s_slopes1$Points[3, 2])), type ='b', lty = 1, pch = 15, axes = F, xlab = "", ylab = "", ylim = yrange, xlim = xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes1$Points[1, 1]), (s_slopes1$Points[1, 2])), type ='b', lty = 2, pch = 17, axes = F, xlab = z_label, ylab = y_label , ylim = yrange, xlim = xrange)
axis(1, at = c(-1, 1), labels = z_values )
axis(2, at = c(-1, -0.5, 0, 0.5, 1))
legend(legend_placement, title = modx_label, c(modx_values), lty=1:2, cex=.90)
title(main=panel_a_label, adj = 0, font.main = 1, cex.main = 1)
box()
dev.off()
graphics.off()

### Plot iv1 as slope and iv2 as moderator and iv3 (high +1SD) as panel
yrange = y_range # modify the y-axis range
xrange = c(-1.5,1.5)
ylim(-1,1)
png(file="./figures/figure_2_br.png", width=8, height=6, units="in", res = 800)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes1$Points[4, 1]), (s_slopes1$Points[4, 2])), type ='b', lty = 1, pch = 15, axes = F, xlab = "", ylab = "", ylim = yrange, xlim = xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes1$Points[2, 1]), (s_slopes1$Points[2, 2])), type ='b', lty = 2, pch = 17, axes = F, xlab = z_label, ylab = y_label , ylim = yrange, xlim = xrange)
axis(1, at = c(-1, 1), labels = z_values )
axis(2, at = c(-1, -0.5, 0, 0.5, 1))
legend(legend_placement, title = modx_label, c(modx_values), lty=1:2, cex=.90)
title(main=panel_b_label, adj = 0, font.main = 1, cex.main = 1)
box()
dev.off()
graphics.off()


# now combine plots into a single figure
panel_a <- image_read('./figures/figure_1_br.png')
panel_b <- image_read("./figures/figure_2_br.png")
img <- c(panel_a, panel_b)

# save combined figure as single file
image_write(image_append(img, stack = TRUE), "./figures/figure_br.png")

