
create_image_path <- function(image_link) {
  path <- paste0(unlist(strsplit(image_link, "_"))[1], "/", image_link, ".png")
  return(paste0(BASE_URL, path))
}

create_survey <- function(df_images) {
  survey <- list(
    title = "AI Emoce",
    description = "Hodnocení emocí v AI generovaných obrázcích",
    logoPosition = "right",
    pages = list()
  )

  images_per_page <- 10
  num_pages <- ceiling(nrow(df_images) / images_per_page)
  pages <- list()

  # Create emotion evaluation pages
  for (page_num in 1:num_pages) {
    i_start <- ((page_num - 1) * images_per_page) + 1
    i_finish <- min(page_num * images_per_page, nrow(df_images))
    page <- create_emotion_page(df_images[i_start:i_finish, ], page_num, i_start)
    pages[[page_num]] <- page
  }

  # Creates AI evaluation pages
  for (page_num in 1:num_pages) {
    new_page_num <- page_num + num_pages
    i_start <- ((page_num - 1) * images_per_page) + 1
    i_finish <- min(page_num * images_per_page, nrow(df_images))
    page <- create_evaluation_page(df_images[i_start:i_finish, ], page_num, i_start)
    pages[[new_page_num]] <- page
  }
  survey$pages <- pages
  return(survey)
}

create_emotion_page <- function(df_page_images, page_num, start_idx) {
  page <- list(
    name = paste0("page_emotion_", page_num),
    title = paste("strana", page_num),
    description = paste("Hodnocení ", start_idx, "až", start_idx + nrow(df_page_images) - 1),
    elements = list()
  )
  ## Create image sections for each image
  for (i in seq_len(nrow(df_page_images))) {
    image_section <- create_image_section(df_page_images[i, ])
    page$elements[[i]] <- image_section
  }
  page$elements <- unlist(page$elements, recursive = FALSE)
  return(page)
}

create_evaluation_page <- function(df_page_images, page_num, start_idx) {
  page <- list(
    name = paste0("page_evaluation_", page_num),
    title = paste("Strana", page_num),
    description = paste("Hodnocení obrázků", start_idx, "až", start_idx + nrow(df_page_images) - 1),
    elements = list()
  )

  for (i in seq_len(nrow(df_page_images))) {
    image_section <- create_believable_image_section(df_page_images[i, ])
    page$elements[[i]] <- image_section
  }
  page$elements <- unlist(page$elements, recursive = FALSE)
  return(page)
}

create_image_section <- function(image_link, page) {
  page_elements <- list()

  page_elements[[1]] <- list(
    type = "image",
    name = paste0("image_emotion_", image_link$file_name),
    imageLink = create_image_path(image_link$file_name),
    contentMode = "image",
    imageFit = "cover",
    imageHeight = "auto",
    imageWidth = "50%"
  )

  page_elements[[2]] <- list(
    type = "checkbox",
    name = paste0("emotion_", image_link$file_name),
    indent = 1,
    title = "Jakou emoci/či neutrální výraz ve fotografii vidíte?",
    isRequired = TRUE,
    description = "Můžete vybrat více možností",
    choices = list(
      list(value = "neutral", text = "neutrální"),
      list(value = "sadness", text = "smutek"),
      list(value = "anger", text = "vztek"),
      list(value = "fear", text = "strach"),
      list(value = "surprise", text = "překvapení"),
      list(value = "joy", text = "radost")
    ),
    showNoneItem = TRUE,
    startWithNewLine = FALSE,
    noneText = "Nic z výše uvedeného",
    colCount = 3
  )

  page_elements[[3]] <- list(
    type = "rating",
    name = paste0("intensity_", image_link$file_name),
    startWithNewLine = FALSE,
    minWidth = "200px",
    title = "Jak silná je podle vás prožívaná emoce?",
    description = "Pokud jste označili více emocí, ohodoťte tu nejsilnější. Nemusíte hodnotit neutrální či žádnou emoci.",
    minRateDescription = "Velice slabá",
    maxRateDescription = "Velice silná",
    requiredIf = paste0("{", {paste0('emotion_', image_link$file_name)}, "} anyof ['sadness', 'anger', 'joy', 'surprise', 'fear']"),
    rateDescriptionLocation = "top"
  )

  return(page_elements)
}

create_believable_image_section <- function(image_link) {
  page_elements <- list()

  page_elements[[1]] <- list(
    type = "image",
    name = paste0("image_believe_", image_link$file_name),
    imageLink = create_image_path(image_link$file_name),
    contentMode = "image",
    imageFit = "cover",
    imageHeight = "auto",
    imageWidth = "30%"
  )

  page_elements[[2]] <- list(
    type = "rating",
    name = paste0("realism_", image_link$file_name),
    startWithNewLine = FALSE,
    title = "Jak moc byste byli schopni uvěřit, že se jedná o skutečnou fotografii?",
    minRateDescription = "Vůbec",
    maxRateDescription = "Velice",
    isRequired = TRUE
  )
  return (page_elements)
}