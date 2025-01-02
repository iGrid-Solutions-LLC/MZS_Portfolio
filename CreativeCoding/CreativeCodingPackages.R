#Packages from github
# devtools::install_github("djnavarro/jasmines")
# devtools::install_github("cutterkom/generativeart")
# devtools::install_github("marcusvolz/mathart")

#Repositories
# https://github.com/marcusvolz?tab=repositories


#Generativeart----
library(generativeart) # devtools::install_github("cutterkom/generativeart")
library(ambient)
library(dplyr)
# set the paths
IMG_DIR <- "img/"
IMG_SUBDIR <- "everything/"
IMG_SUBDIR2 <- "handpicked/"
IMG_PATH <- paste0(IMG_DIR, 
                   IMG_SUBDIR)
LOGFILE_DIR <- "logfile/"
LOGFILE <- "logfile.csv"
LOGFILE_PATH <- paste0(LOGFILE_DIR, 
                       LOGFILE)
# create the directory structure
generativeart::setup_directories(IMG_DIR, 
                                 IMG_SUBDIR, 
                                 IMG_SUBDIR2, 
                                 LOGFILE_DIR)
# include a specific formula, for example:
my_formula <- list(
  x = quote(runif(1, -1, 10) * x_i^2 - sin(y_i^3)),
  y = quote(runif(1, -1, 10) * y_i^3 - cos(x_i^2) * y_i^4)
)
# call the main function to create five images with a polar coordinate system
generativeart::generate_img(formula = my_formula, 
                            nr_of_img = 5, # set the number of iterations
                            polar = TRUE, 
                            filetype = "png", 
                            color = "#33E6FF", 
                            background_color = "#1a3657")


#Another Example
my_formula <- list(
  x = quote(runif(1, -1, 1) * x_i^2 - cos(y_i^2)*sin(y_i^2)),
  y = quote(runif(1, -1, 1) * y_i^2 - cos(x_i^2)*y_i^2)
)

generativeart::generate_img(formula = my_formula, nr_of_img = 5, polar = TRUE, color = "black", background_color = "white")


#Jasmines----
library(ggplot2)
library(dplyr) # or install.packages("dplyr") first
library(jasmines) # or devtools::install_github("djnavarro/jasmines")
p1 <- use_seed(100) %>% # Set the seed of R‘s random number generator, which is useful for creating simulations or random objects that can be reproduced.
  scene_discs(
    rings = 20, 
    points = 50000, 
    size = 100
  ) %>%
  mutate(ind = 1:n()) %>%
  unfold_warp(
    iterations = 10,
    scale = .5, 
    output = "layer" 
  ) %>%
  unfold_tempest(
    iterations = 5,
    scale = .01
  ) %>%
  style_ribbon(
    color = "black",
    colour = "ind",
    alpha = c(1,1),
    background = "white"
  )
ggsave("p1.png", p1, width = 20, height = 20, units = "in")


#mathart Very Memory Intensive (marcusvolz)----
library(mathart) # devtools::install_github("marcusvolz/mathart")
library(ggforce)
library(Rcpp)
library(tidyverse)

#mollusc
df <- mollusc()
df1 <- df %>% mutate(id = 1)
df2 <- df %>% mutate(id = 2)
df3 <- df %>% mutate(id = 3)

p <- ggplot() +
  geom_point(aes(x, y), df1, size = 0.03, alpha = 0.03) +
  geom_path( aes(x, y), df1, size = 0.03, alpha = 0.03) +
  geom_point(aes(x, z), df2, size = 0.03, alpha = 0.03) +
  geom_path( aes(x, z), df2, size = 0.03, alpha = 0.03) +
  geom_point(aes(y, z), df3, size = 0.03, alpha = 0.03) +
  geom_path( aes(y, z), df3, size = 0.03, alpha = 0.03) +
  facet_wrap(~id, nrow = 2, scales = "free") +
  theme_blankcanvas(margin_cm = 0.5)

ggsave("mollusc01.png", width = 80, height = 80, units = "cm")


#harmonograph
df1 <- harmonograph(A1 = 1, A2 = 1, A3 = 1, A4 = 1,
                    d1 = 0.004, d2 = 0.0065, d3 = 0.008, d4 = 0.019,
                    f1 = 3.001, f2 = 2, f3 = 3, f4 = 2,
                    p1 = 0, p2 = 0, p3 = pi/2, p4 = 3*pi/2) %>% mutate(id = 1)

df2 <- harmonograph(A1 = 1, A2 = 1, A3 = 1, A4 = 1,
                    d1 = 0.0085, d2 = 0, d3 = 0.065, d4 = 0,
                    f1 = 2.01, f2 = 3, f3 = 3, f4 = 2,
                    p1 = 0, p2 = 7*pi/16, p3 = 0, p4 = 0) %>% mutate(id = 2)

df3 <- harmonograph(A1 = 1, A2 = 1, A3 = 1, A4 = 1,
                    d1 = 0.039, d2 = 0.006, d3 = 0, d4 = 0.0045,
                    f1 = 10, f2 = 3, f3 = 1, f4 = 2,
                    p1 = 0, p2 = 0, p3 = pi/2, p4 = 0) %>% mutate(id = 3)

df4 <- harmonograph(A1 = 1, A2 = 1, A3 = 1, A4 = 1,
                    d1 = 0.02, d2 = 0.0315, d3 = 0.02, d4 = 0.02,
                    f1 = 2, f2 = 6, f3 = 1.002, f4 = 3,
                    p1 = pi/16, p2 = 3*pi/2, p3 = 13*pi/16, p4 = pi) %>% mutate(id = 4)

df <- rbind(df1, df2, df3, df4)

p <- ggplot() +
  geom_path(aes(x, y), df, alpha = 0.25, size = 0.5) +
  coord_equal() +
  facet_wrap(~id, nrow = 2) +
  theme_blankcanvas(margin_cm = 0)

ggsave("harmonograph01.png", p, width = 20, height = 20, units = "cm")
