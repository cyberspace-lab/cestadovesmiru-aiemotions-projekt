library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

json_results <- jsonlite::fromJSON("data/AI Emoce.csv")
df_results <- read.csv("data/results.csv")

df_results <- as.data.frame(json_results$Data)
str(df_results)

colnames(df_results)

# the table is tructured in a way that each emotion has three columns: emotion felt, intesity and realism
# THe columns are named "columnnbame_emotion_image"
# I need to pivot longer, but each row should be the values for the single image - emotion, intensity and realism



# for eadh column which starts with emotion_ encode the values which are a list to a string concatenated with ,
for (col in colnames(df_results)){
  if (startsWith(col, "emotion_")){
    df_results[[col]] <- sapply(df_results[[col]], function(x) paste(x, collapse = ","))
  }
}

df_results_long <- tidyr::pivot_longer(df_results, cols = -c(HappendAt, InstanceId, question1),
                                       values_transform = list(value = as.character))
df_results_long <- select(df_results_long, -c(HappendAt, InstanceId))
df_results_long <- separate(df_results_long, name, into = c("question", "emotion_tested", "image"), sep = "_")

df_results_long <- pivot_wider(df_results_long, id_cols = c(image, question1, emotion_tested),
            names_from = question, values_from = value)

conversion_emotions <- c("sad" = "sadness", "happy" = "joy", "angry" = "anger",
                         "scared" = "fear", "surprised" = "surprise", "neutral" = "neutral")

# replace the emotions with the ones from the conversion_emotions vector
df_results_long$emotion_imagename <- str_glue("{df_results_long$emotion_tested}_{df_results_long$image}")
df_results_long$emotion_tested <- conversion_emotions[df_results_long$emotion_tested]
df_results_long <- mutate(df_results_long,
# does emotion contain emotion_tested
  emotion_seen = str_detect(emotion, emotion_tested),
  proportion_seen = str_count(emotion, ",") + 1,
  intensity = as.numeric(intensity))

df_accuracy <- df_results_long %>%
  group_by(question1) %>%
  count(emotion_seen) %>%
  pivot_wider(names_from = emotion_seen, values_from = n) %>%
  mutate(proportion = `TRUE` / (`TRUE` + `FALSE`)) %>%
  arrange(proportion)

ids_to_drop <- df_accuracy %>%
  filter(proportion < 0.8) %>%
  pull(question1)

df_results_long <- df_results_long %>%
  filter(!question1 %in% ids_to_drop)

well_defined_images <- df_results_long %>%
  group_by(emotion_imagename) %>%
  summarise(mean_proportion_seen = mean(emotion_seen)) %>%
  filter(mean_proportion_seen > 0.75) %>%
  pull(emotion_imagename)

df_results_long %>%
  filter(emotion_imagename %in% well_defined_images) %>%
  distinct(emotion_tested, emotion_imagename) %>%
  count(emotion_tested)

## dropping fear

df_results_long <- df_results_long %>%
  filter(emotion_tested != "fear", emotion_imagename %in% well_defined_images)


df_results_long %>%
  group_by(emotion_imagename) %>%
  summarise(mean_proportion_seen = mean(proportion_seen)) %>%
  ggplot(aes(x = mean_proportion_seen)) +
    geom_histogram(binwidth = 0.05)

df_results_long %>%
  group_by(emotion_tested, emotion_imagename) %>%
  summarise(mean_proportion_seen = mean(proportion_seen)) %>%
  filter(mean_proportion_seen < 1.3) %>%
  count(emotion_tested)

# select the best 34 images for each emotion based on the mean proportion_seen

best_images <- df_results_long %>%
  group_by(emotion_imagename, emotion_tested) %>%
  summarise(mean_proportion_seen = mean(proportion_seen)) %>%
  filter(mean_proportion_seen < 1.3) %>%
  group_by(emotion_tested) %>%
  top_n(34, mean_proportion_seen) %>%
  ungroup() %>%
  pull(emotion_imagename)

df_results_long <- df_results_long %>%
  filter(emotion_imagename %in% best_images)

df_results_long %>%
  ggplot(aes(x = intensity)) +
    geom_histogram(binwidth = 0.1) +
    facet_wrap(~emotion_tested)

final_images <- df_results_long %>%
  group_by(emotion_tested, emotion_imagename) %>%
  summarise(mean_intensity = mean(intensity, na.rm = TRUE)) %>%
  # select be best 9 images from each emotion but below the 0.8 quartile
  group_by(emotion_tested) %>%
  filter(mean_intensity < quantile(mean_intensity, 0.8)) %>%
  arrange(mean_intensity) %>%
  top_n(9, mean_intensity) %>%
  pull(emotion_imagename)

training_images <- best_images[!(best_images %in% final_images)]

# for each final image, go to folder images/emotion/emotion_imagename and copy the image to the folder images/moving/final
# for each training image, go to folder images/emotion/emotion_imagename and copy the image to the folder images/moving/training
for(image in final_images){
  file.copy(from = paste0("images/", image, ".png"),
            to = "images/moving/final")
}

for(image in training_images){
  file.copy(from = paste0("images/", image, ".png"),
            to = "images/moving/training")
}
