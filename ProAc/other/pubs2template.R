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

pubs_ = mypubs_ext |> 
  mutate(title = str_remove_all(str_to_lower(TITLE), " ")) |> 
  left_join(
    mutate(pubs_edit, title = str_remove_all(str_to_lower(title), " ")),
    by = "title"
  ) |> 
  mutate(
    phd_pub = case_when(
      pubid == "QIV2ME_5wuYC" ~ 1,
      TRUE ~ 0
    ),
    supervisor = case_when(
      phd_pub == 0 & supervisor == 1 ~ 0,
      TRUE ~ supervisor
    ),
    publisher_ = case_when(
      !is.na(JOURNAL) ~ JOURNAL,
      !is.na(INSTITUTION) ~ INSTITUTION,
      TRUE ~ NA_character_
    ),
    kind = case_when(
      CATEGORY == "ARTICLE" ~ "Peer-reviewed paper",
      CATEGORY == "INPROCEEDINGS" ~ "Conference proceedings",
      pubid == "W7OEmFMy1HYC" ~ "MSc thesis",
      pubid == "Tyk-4Ss8FVUC" ~ "BSc thesis",
    ),
    open_access = case_when(
      is.na(link) ~ 0,
      TRUE ~ 1
    )
  )

proac_template = read_csv(here("ProAc/ScientificOutput-Template.csv"))
glimpse(proac_template)

pubs_proac = pubs_ |>
  transmute(
    Tag = BIBTEXKEY,
    Year = YEAR,
    Kind = kind, 
    `First authored` = first_author,
    `Single authored` = single_author,
    `PhD publication` = phd_pub,
    `Including PhD supervisor` = supervisor,
    Publisher = publisher_,
    `Open access` = open_access,
    Link = link,
    `Full reference` = NA,
    `Current number of citations` = cites,
    Title = TITLE
  )

write_csv(
  pubs_proac, 
  file = "ProAc/ScientificOutput-auto.csv",
)

pubs_proac = read_csv("ProAc/ScientificOutput-auto.csv")
ext_proac = read_csv("ProAc/ScientificOutput-manual.csv")

proac = pubs_proac |>
  rbind(ext_proac)
write_csv(
  proac, quote = "needed", 
  file = "ProAc/ScientificOutput.csv"
)

