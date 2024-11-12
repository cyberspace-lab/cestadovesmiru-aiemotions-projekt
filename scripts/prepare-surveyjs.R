
library(googlesheets4)

SHEET_ID <- "1OPRw3ZRwaYkJnrtwMy_74xaEAYCyt5f1DmGOktfch08"
BASE_URL <- "https://files.hejtmy.com/api/public/dl/W1XGSddr/"

df_images <- read_sheet(SHEET_ID, sheet = "images")
write.csv(df_emotions_list, "data/images_list.csv", row.names = FALSE)
df_images <- read.csv("data/images_list.csv")

source("functions/survey-preparations.R")
set.seed(2028)
survey <- create_survey(df_images[sample(nrow(df_images)),])
#randomize order in dataframe
write(jsonlite::toJSON(survey, auto_unbox = TRUE, pretty = TRUE), file = "output.json", append = FALSE)
