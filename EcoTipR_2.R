#Libraries
library(ggplot2)
library(gganimate)
library(gifski)


# -------------------------------------------------------------------------
# The original parame-terization of the VBGF from von Bertalanffy (1938)is
# L_t = Linf - (Linf - L0)*e^-(K*t)
# or equivalently,
# L_t = L0 + (Linf - Lr)*[1-e^-K(t-tr)]


# -------------------------------------------------------------------------
# Inputs given parameters -------------------------------------------------
# -------------------------------------------------------------------------

# Common parameters
L0 <- 432
K  <- 0.17

#Female
Linf_f <- 671
tr_f   <- 3.87 

#Male
Linf_m <- 585
tr_m   <- 4.76

#ages
t <- seq(from = 0, to = 13, by = 0.1)


# -------------------------------------------------------------------------
# von Bertalanffy growth model --------------------------------------------
# -------------------------------------------------------------------------

# Fish female
Lt_f <- L0 + (Linf_f-L0)*(1-exp(-K*(t-tr_f)))

# Fish male
Lt_m <- L0 + (Linf_m-L0)*(1-exp(-K*(t-tr_m)))



#Plot base
plot(t, Lt_f, xlim = c(0, 13), ylim = c(100, 700), type = "l", lwd = 4, col = "red", 
     ylab = "Total length (mm)", xlab = "Ages", axes = FALSE)
axis(1, seq(0, 13, 1))
axis(2, las = 2)
lines(t, Lt_m, xlim = c(0, 13), ylim = c(100, 700), type = "l", lwd = 4, col = "blue")
box()
legend("topleft", c("Female", "Male"), 
       col = c("red", "blue"), lwd = 3, bty = "n")


# -------------------------------------------------------------------------
# Animation plot ----------------------------------------------------------
# -------------------------------------------------------------------------

# Build dataframe
growth_data <- data.frame(len = c(Lt_f, Lt_m), 
                          age = rep(seq(0, 13, 0.1), 2),
                          group = rep(c(1,2), each = length(Lt_f)))

#Plot 
p <- ggplot(growth_data,
            aes(age, len, group = group, color = factor(group)))+
  geom_line(size = 3) +
  theme_bw() +
  labs(x = "Ages", y = "Total length (mm)") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15)) +
  scale_x_continuous(breaks = c(0:15)) +
  scale_y_continuous(breaks = seq(100, 700, 50)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95", colour = "grey95")) +
  theme(legend.position = c(0.1, 0.9), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15), 
        legend.background = element_blank(),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  scale_color_manual(labels = c("Female", "Male"), 
                     values = c("red", "blue"))
p

#Animation
figure <- p + transition_reveal(along = age, range = c(0, 13))
figure

#Parameters for size and resolution
options(gganimate.dev_args = list(width = 10, height = 7, units = 'in', res=320))
#Save animation
anim_save("growth_animation.gif", figure)
