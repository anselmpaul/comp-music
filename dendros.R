library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(spotifyr)
library(compmus)

load("data/playlists.RData")

topTens <- rbind(head(topOf2016, 10), head(topOf2017, 10), head(topOf2018, 10),
                 head(topOf2019, 10), head(topOf2020, 10), head(topOf2021, 10))

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit %>% 
    collect_predictions() %>% 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit %>% 
    conf_mat_resampled() %>% 
    group_by(Prediction) %>% mutate(precision = Freq / sum(Freq)) %>% 
    group_by(Truth) %>% mutate(recall = Freq / sum(Freq)) %>% 
    ungroup() %>% filter(Prediction == Truth) %>% 
    select(class = Prediction, precision, recall)
}

stuff <-
  topTens %>%
  slice_head(n = 20) %>%
  add_audio_analysis() %>%
  mutate(
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean"
      )
  ) %>%
  mutate(pitches = map(pitches, compmus_normalise, "clr")) %>%
  mutate_at(vars(pitches, timbre), map, bind_rows) %>%
  unnest(cols = c(pitches, timbre))

stuff_juice <-
  recipe(
    track.name ~
      danceability +
      energy +
      loudness +
      speechiness +
      acousticness +
      instrumentalness +
      liveness +
      valence +
      tempo +
      duration, # +
      #C + `C#|Db` + D + `D#|Eb` +
      #E + `F` + `F#|Gb` + G +
      #`G#|Ab` + A + `A#|Bb` + B +
      #c01 + c02 + c03 + c04 + c05 + c06 +
      #c07 + c08 + c09 + c10 + c11 + c12,
    data = stuff
  ) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>% 
  # step_range(all_predictors()) %>% 
  prep(stuff %>% mutate(track.name = str_trunc(track.name, 20))) %>%
  juice() %>%
  column_to_rownames("track.name")

stuff_dist <- dist(stuff_juice, method = "euclidean")

stuff_dist %>% 
  hclust(method = "complete") %>% # Try single, average, and complete.
  dendro_data() %>%
  ggdendrogram()

# heatmaply(
#   t21_juice,
#   hclustfun = hclust,
#   hclust_method = "complete",  # Change for single, average, or complete linkage.
#   dist_method = "euclidean"
# )
# 
# pop <- 
#   get_playlist_audio_features("spotify", "37i9dQZF1DWWEcRhUVtL8n")
# party <- get_playlist_audio_features("spotify", "37i9dQZF1DWTujiC7wfofZ")
# workout <- get_playlist_audio_features("spotify", "37i9dQZF1DXaRL7xbcDl7X")
# indie <-
#   bind_rows(
#     pop %>% mutate(playlist = "Indie Pop") %>% slice_head(n = 20),
#     party %>% mutate(playlist = "Indie Party") %>% slice_head(n = 20),
#     workout %>% mutate(playlist = "Indie Workout") %>% slice_head(n = 20)
#   ) 
# 
# indie_features <-
#   topOf2021 %>%  # For your portfolio, change this to the name of your corpus.
#   add_audio_analysis() %>% 
#   mutate(
#     playlist = factor(playlist),
#     segments = map2(segments, key, compmus_c_transpose),
#     pitches =
#       map(
#         segments,
#         compmus_summarise, pitches,
#         method = "mean", norm = "manhattan"
#       ),
#     timbre =
#       map(
#         segments,
#         compmus_summarise, timbre,
#         method = "mean",
#       )
#   ) %>%
#   mutate(pitches = map(pitches, compmus_normalise, "clr")) %>%
#   mutate_at(vars(pitches, timbre), map, bind_rows) %>%
#   unnest(cols = c(pitches, timbre))
# 
# indie_recipe <-
#   recipe(
#     playlist ~
#       danceability +
#       energy +
#       loudness +
#       speechiness +
#       acousticness +
#       instrumentalness +
#       liveness +
#       valence +
#       tempo +
#       duration +
#       C + `C#|Db` + D + `D#|Eb` +
#       E + `F` + `F#|Gb` + G +
#       `G#|Ab` + A + `A#|Bb` + B +
#       c01 + c02 + c03 + c04 + c05 + c06 +
#       c07 + c08 + c09 + c10 + c11 + c12,
#     data = indie_features,          # Use the same name as the previous block.
#   ) %>%
#   step_center(all_predictors()) %>%
#   step_scale(all_predictors())      # Converts to z-scores.
# # step_range(all_predictors())    # Sets range to [0, 1].
# 
# 
# indie_cv <- indie_features %>% vfold_cv(5)
# 
# knn_model <-
#   nearest_neighbor(neighbors = 1) %>%
#   set_mode("classification") %>% 
#   set_engine("kknn")
# indie_knn <- 
#   workflow() %>% 
#   add_recipe(indie_recipe) %>% 
#   add_model(knn_model) %>% 
#   fit_resamples(
#     indie_cv, 
#     control = control_resamples(save_pred = TRUE)
#   )
# 
# indie_knn %>% get_conf_mat()
# 
# indie_knn %>% get_conf_mat() %>% autoplot(type = "mosaic")
# 
# indie_knn %>% get_conf_mat() %>% autoplot(type = "heatmap")
# indie_knn %>% get_pr()
# 
# forest_model <-
#   rand_forest() %>%
#   set_mode("classification") %>% 
#   set_engine("ranger", importance = "impurity")
# indie_forest <- 
#   workflow() %>% 
#   add_recipe(indie_recipe) %>% 
#   add_model(forest_model) %>% 
#   fit_resamples(
#     indie_cv, 
#     control = control_resamples(save_pred = TRUE)
#   )
# 
# indie_forest %>% get_pr()
# 
# workflow() %>% 
#   add_recipe(indie_recipe) %>% 
#   add_model(forest_model) %>% 
#   fit(indie_features) %>% 
#   pluck("fit", "fit", "fit") %>%
#   ranger::importance() %>% 
#   enframe() %>% 
#   mutate(name = fct_reorder(name, value)) %>% 
#   ggplot(aes(name, value)) + 
#   geom_col() + 
#   coord_flip() +
#   theme_minimal() +
#   labs(x = NULL, y = "Importance")
# 
# indie_features %>%
#   ggplot(aes(x = c01, y = c02, colour = playlist, size = energy)) +
#   geom_point(alpha = 0.8) +
#   scale_color_viridis_d() +
#   labs(
#     x = "Timbre Component 1",
#     y = "Timbre Component 2",
#     size = "Energy",
#     colour = "Playlist"
#   )
# 
