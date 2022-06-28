library(BradleyTerryScalable)
library(purrr)
library(dplyr)
library(ggplot2)
library(Matrix)
library(BradleyTerry2)
library(EMK)

# create matrices
set.seed(1)
K <- c(100, 500, 1000, 2500, 5000, 7500, 10000, 15000, 20000) # matrix sizes
W <- map(K, ~rsparsematrix(.x, .x, 0.5, rand.x = function(n) rbinom(n, 10, 0.5)))
W_0.5 <- W
W_0.01 <- map(K, ~rsparsematrix(.x, .x, 0.01, rand.x = function(n) rbinom(n, 10, 0.5)))
W_0.1 <- map(K, ~rsparsematrix(.x, .x, 0.1, rand.x = function(n) rbinom(n, 10, 0.5)))
W_0.001 <- map(K, ~rsparsematrix(.x, .x, 0.001, rand.x = function(n) rbinom(n, 10, 0.5)))


# which(summary(W_0.01[[3]])[,"x"] == 0) - BUG
W_btdata_0.001 <- map(W_0.001, ~btdata(.x))
W_btdata_0.01 <- map(W_0.01, ~btdata(.x))
W_btdata_0.1 <- map(W_0.1, ~btdata(.x))
W_btdata_0.5 <- map(W_0.5, ~btdata(.x))

comp_length_0.01 <- map(W_btdata_0.01, "components") %>%
  map_int(length)
comp_length_0.01 == 1

comp_length_0.1 <- map(W_btdata_0.1, "components") %>%
  map_int(length)
comp_length_0.1 == 1

btdata_timings_0.1 <- map(W_0.1, ~system.time(btdata(.x)))
btdata_elapsed_0.1 <- map_dbl(btdata_timings_0.1, "elapsed")
btdata_elapsed_0.1

btdata_timings_0.01 <- map(W_0.01, ~system.time(btdata(.x)))
btdata_elapsed_0.01 <- map_dbl(btdata_timings_0.01, "elapsed")
length(btdata_elapsed_0.01)

btdata_timings_0.001 <- map(W_0.001, ~system.time(btdata(.x)))
btdata_elapsed_0.001 <- map_dbl(btdata_timings_0.001, "elapsed")
btdata_elapsed_0.001

btdata_timings_df_0.001 <- data_frame(time = btdata_elapsed_0.001, K = K, density = 0.001)
btdata_timings_df_0.01 <- data_frame(time = btdata_elapsed_0.01, K = K, density = 0.01)
btdata_timings_df_0.1 <- data_frame(time = btdata_elapsed_0.1, K = K, density = 0.1)
btdata_timings_df_0.5 <- data_frame(time = btdata_elapsed_0.5, K = mid_K, density = 0.5)

btdata_timings_df <- bind_rows(btdata_timings_df_0.001,
                               btdata_timings_df_0.01,
                               btdata_timings_df_0.1,
                               btdata_timings_df_0.5) %>%
  mutate(density = as.factor(density))

save(btdata_timings_df, file = "btdata_timings_2022_df.RData")
rm(btdata_timings_2022_df)
load("btdata_timings_2022_df.RData")
head(btdata_timings_df)

pres_adj <- theme(axis.text=element_text(size=18), axis.title=element_text(size=24), strip.text.x = element_text(size = 18), strip.text.y = element_text(size = 18), legend.text = element_text(size = 18), legend.title=element_text(size=24), title=element_text(size=24))

library(ggplot2)
btdata_timings_plot <- ggplot(btdata_timings_df, aes(K, time, colour = density)) +
  geom_line(size = 1.5) + 
  geom_point(size = 3) +
  xlab("K (number of items)") +
  ylab("time (seconds)") +
  pres_adj +
  ggtitle("Timings for btdata()")

btdata_timings_plot

setwd("/Users/ellakaye/Images")
jpeg("btdata_timings.jpeg")
btdata_timings_plot
dev.off()


# check they're all fully connected
comp_length <- map(W_btdata, "components") %>%
  map_int(length)
all(comp_length == 1)

# fit the model
btfit_timings <- map(W_btdata, ~system.time(btfit(.x, a = 1)))

btfit_timings_0.01 <- map(W_btdata_0.01, ~system.time(btfit(.x, a = 1.1)))
btfit_elapsed_0.01 <- map_dbl(btfit_timings_0.01, "elapsed")
btfit_elapsed_0.01
#btfit_timings_0.1 <- map(W_btdata_0.1, ~system.time(btfit(.x, a = 1.1)))


btfit_timings_0.001 <- map(W_btdata_0.001[4:9], ~system.time(btfit(.x, a = 1.1)))
btfit_elapsed_0.001 <- map_dbl(btfit_timings_0.001, "elapsed")
btfit_elapsed_0.001


# mid K
set.seed(1)
mid_K <- c(100, 500, 1000, 2500, 5000) # matrix sizes
W_0.5 <- map(mid_K, ~rsparsematrix(.x, .x, 0.5, rand.x = function(n) rbinom(n, 10, 0.5)))

W_btdata_0.5 <- map(W_0.5, ~btdata(.x))
btdata_timings_0.5 <- map(W_0.5, ~system.time(btdata(.x)))
btdata_elapsed_0.5 <- map_dbl(btdata_timings_0.5, "elapsed")
btdata_elapsed_0.5

btfit_timings_0.5 <- map(W_btdata_0.5, ~system.time(btfit(.x, a = 1.1)))
btfit_elapsed_0.5 <- map_dbl(btfit_timings_0.5, "elapsed")
btfit_elapsed_0.5

btfit_timings_0.1 <- map(W_btdata_0.1[1:7], ~system.time(btfit(.x, a = 1.1)))
btfit_elapsed_0.1 <- map_dbl(btfit_timings_0.1, "elapsed")
btfit_elapsed_0.1

btfit_timings_df_0.001 <- data_frame(time = btfit_elapsed_0.001, K = K[4:9], density = 0.001)
btfit_timings_df_0.01 <- data_frame(time = btfit_elapsed_0.01, K = K, density = 0.01)
btfit_timings_df_0.1 <- data_frame(time = btfit_elapsed_0.1, K = K[1:7], density = 0.1)
btfit_timings_df_0.5 <- data_frame(time = btfit_elapsed_0.5, K = mid_K, density = 0.5)

btfit_timings_df <- bind_rows(btfit_timings_df_0.001,
                               btfit_timings_df_0.01,
                               btfit_timings_df_0.1,
                               btfit_timings_df_0.5) %>%
  mutate(density = as.factor(density))

save(btfit_timings_df, file = "btfit_timings_2022_df.RData")


pres_adj <- theme(axis.text=element_text(size=18), axis.title=element_text(size=24), strip.text.x = element_text(size = 18), strip.text.y = element_text(size = 18), legend.text = element_text(size = 18), legend.title=element_text(size=24), title=element_text(size=24))

library(ggplot2)
btfit_timings_plot <- ggplot(btfit_timings_df, aes(K, time, colour = density)) +
  geom_line(size = 1.5) + 
  geom_point(size = 3) +
  xlab("K (number of items)") +
  ylab("time (seconds)") +
  pres_adj +
  ggtitle("Timings for btfit()")

btfit_timings_plot


# compare to BradleyTerry2
# convert matrix to cross-tab
library(BradleyTerry2)
small_Mat <- rsparsematrix(4, 4, 0.5, rand.x = function(n) rbinom(n, 10, 0.5))
dimnames(small_Mat) <- list(LETTERS[1:4], LETTERS[1:4])
small_Mat
small_Mat_df <- dplyr::as_data_frame(as.data.frame.table(as.matrix(small_mat), useNA = "no", stringsAsFactors = FALSE)) %>%
  filter(Freq != 0)

small_mat <- as.matrix(small_Mat) 
small_table <- small_mat
class(small_table) <- "table"
small_table

countsToBinomial(small_table)

small_K <- c(2:5)
small_W <- map(small_K, ~rsparsematrix(.x, .x, 0.5, rand.x = function(n) rbinom(n, 10, 0.5)))
small_W_mat <- map(small_W, as.matrix)

four_letter_words <- n_letter_words(4, as_vector = TRUE)

mat_to_table <- function(mat) {
  n <- nrow(mat) 
  rownames(mat) <- colnames(mat) <- four_letter_words[1:n]
  class(mat) <- "table"
  mat
}

small_tables <- map(small_W_mat, mat_to_table)
small_binomials <- map(small_tables, countsToBinomial)

map(small_binomials, ~BTm(cbind(win1, win2), player1, player2, data = .x))

load("btfit_timings_df.RData")

btfit_timings_df |> View()
