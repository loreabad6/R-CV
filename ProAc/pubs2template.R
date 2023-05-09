library(bib2df)
library(RefManageR)
library(scholar)
library(here)
library(tidyverse)

id = "vDqPwpUAAAAJ&hl"
name = "Lorena Abad"
l = get_profile(id)
pubs = get_publications(l)
supervisor = "Dirk Tiede"
bib_file = here("bib_files/mypubs.bib")

pubs_edit = pubs |> 
  mutate(
    title = case_when(
      pubid == "d1gkVwhDpl0C" ~ "Calidad del Agua y Variables Ambientales en Hábitats para Anfibios Amenazados en la Zona Urbana de Cuenca", 
      TRUE ~ title
    )
  )
mypubs = bib2df(bib_file)
mypubs_ext = mypubs |> 
  rowwise() |> 
  mutate(
    single_author = case_when(
      length(AUTHOR) > 1 ~ 0,
      TRUE ~ 1
    ),
    first_author = case_when(
      which(map2_lgl(
        name, 
        AUTHOR, 
        `%in%`
      )) == 1 ~ 1,
      TRUE ~ 0
  )) |> 
  ungroup() |> 
  mutate(
    supervisor = case_when(map2_lgl(
      supervisor, 
      AUTHOR, 
      `%in%`
    ) ~ 1,
    TRUE ~ 0
    ),
    link = case_when(
      !is.na(DOI) ~ DOI,
      !is.na(URL) ~ URL,
      TRUE ~ NA_character_
    )
  )

mypubs_ext |> 
  mutate(title = str_remove_all(str_to_lower(TITLE), " ")) |> 
  left_join(
    mutate(pubs_edit, title = str_remove_all(str_to_lower(title), " ")),
    by = "title"
  ) |> View()

proac_template = read_csv(here("ProAc/ScientificOutput-Template.csv"))
glimpse(proac_template)
