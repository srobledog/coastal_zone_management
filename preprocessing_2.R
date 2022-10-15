library(tidyverse)
source("verbs.R")
library(bibliometrix)
library(tosr)
library(igraph)
library(tidygraph)
library(lubridate)
library(sjrdata)
library(openxlsx)
library(zoo)
library(journalabbr)
library(ggraph)
library(openxlsx)
library(XML)
library(plyr)

wos_scopus <-
  read_csv("https://docs.google.com/spreadsheets/d/1nwszUhhHBzJS1WA0aP5lVMZY_lEVxjT4QO1l3bKiVtE/export?format=csv&gid=0") |> 
  dplyr::filter(!is.na(AU)) |> 
  dplyr::select(-AU_CO) |>
  bibliometrix::metaTagExtraction(Field = "AU_CO")

AU_CO_df <- 
  wos_scopus |>
  select(SR, TI, AU_CO, TC, SO, PY, quartile) |>
  separate_rows(AU_CO, sep = ";")


# wos <-
#   bibliometrix::convert2df("jaime_STI.txt") |>  # create dataframe from wos file
#   bibliometrix::metaTagExtraction(Field = "AU_CO" ) # Adding Country author's affiliation
# 
# scopus <-
#   bibliometrix::convert2df("jaime_STI.bib", # Create dataframe from scopus file
#                            dbsource = "scopus",
#                            format = "bibtex") |> 
#   bibliometrix::metaTagExtraction(Field = "AU_CO" )  # Adding Country author's affiliation

CR_links <-
  get_references(wos_scopus)

write_csv(CR_links, "CR_links_camilo_MCZ.csv")

citation_network <- 
  get_citation_network(scopus_df = wos_scopus, references_df = CR_links) |> 
  get_citation_network_tos() |> 
  tidygraph::activate(nodes) |> 
  dplyr::mutate(in_degree = tidygraph::centrality_degree(mode = "in"),
                out_degree = tidygraph::centrality_degree(mode = "out"),
                bet = tidygraph::centrality_betweenness()) 

citation_network_undirected <- 
  citation_network |> 
  tidygraph::to_undirected() |> 
  tidygraph::activate(nodes) |> 
  dplyr::mutate(cluster = tidygraph::group_louvain()) |> 
  tidygraph::as_tibble() |> 
  dplyr::select(name, cluster)

ToS <- 
  citation_network |> 
  tidygraph::activate(nodes) |> 
  dplyr::left_join(citation_network_undirected) |> 
  tidygraph::as_tibble() |> 
  dplyr::select(name, TI, PY, in_degree, out_degree, bet, cluster)

write_csv(CR_links, "ToS_camilo_MCZ.csv")

# country_df <-
#   get_country()

SO_links <-
  get_journals(wos_scopus, CR_links)

write_csv(SO_links, "SO_links_camilo_MCZ.csv")

AU_links <-
  get_authors(wos_scopus, CR_links)

write_csv(AU_links, "AU_links_camilo_MCZ.csv")

#### Figure 1 ####

# wos_anual_production <-
#   wos |>
#   dplyr::select(PY) |>
#   dplyr::count(PY, sort = TRUE) |>
#   na.omit() |>
#   dplyr::filter(PY >= 2000,
#                 PY < year(today())) |>
#   dplyr::mutate(ref_type = "wos")
# 
# scopus_anual_production  <-
#   scopus |>
#   dplyr::select(PY) |>
#   dplyr::count(PY, sort = TRUE) |>
#   na.omit() |>
#   dplyr::filter(PY >= 2000,
#                 PY < year(today())) |>
#   dplyr::mutate(ref_type = "scopus")
# 
# total_anual_production <-
#   wos_scopus_tos$df |>
#   dplyr::select(PY) |>
#   dplyr::count(PY, sort = TRUE) |>
#   na.omit() |>
#   dplyr::filter(PY >= 2000,
#                 PY < year(today())) |>
#   dplyr::mutate(ref_type = "total") |>
#   dplyr::arrange(desc(PY))
# 
# wos_scopus_total_annual_production <-
#   wos_anual_production |>
#   bind_rows(scopus_anual_production,
#             total_anual_production)
# 
# # Checking results of total
# wos_scopus_total_annual_production_dummy <-
#   total_anual_production |>
#   dplyr::rename(n_total = n,
#                 ref_type_total = ref_type) |>
#   left_join(wos_anual_production |>
#               dplyr::rename(n_wos = n,
#                             ref_type_wos = ref_type) ) |>
#   left_join(scopus_anual_production |>
#               dplyr::rename(n_scopus = n,
#                             ref_type_scopus = ref_type)) |>
#   # mutate(total = if_else(n_total < n_wos | n_total < n_scopus,
#   #                        max(tibble(n_wos = n_wos, n_scopus = n_scopus)), # it could be improved
#   #                        n_total)) |>
#   replace_na(list(n_wos = 0,
#                   n_scopus = 0,
#                   n_total = 0,
#                   ref_type_wos = "wos",
#                   ref_type_scopus = "scopus"))
# # select(-total)
# 
# wos_scopus_total_annual_production_total <-
#   wos_scopus_total_annual_production_dummy |>
#   select(PY,
#          n = n_total,
#          ref_type = ref_type_total)
# 
# wos_scopus_total_annual_production_scopus <-
#   wos_scopus_total_annual_production_dummy |>
#   select(PY,
#          n = n_scopus,
#          ref_type = ref_type_scopus)
# 
# wos_scopus_total_annual_production_wos <-
#   wos_scopus_total_annual_production_dummy |>
#   select(PY,
#          n = n_wos,
#          ref_type = ref_type_wos)
# 
# wos_scopus_total_annual_production <-
#   wos_scopus_total_annual_production_total |>
#   bind_rows(wos_scopus_total_annual_production_scopus,
#             wos_scopus_total_annual_production_wos)
# 
# figure_1_data <-
#   wos_scopus_total_annual_production |>
#   mutate(PY = replace_na(PY, replace = 0)) |>
#   pivot_wider(names_from = ref_type,
#               values_from = n) |>
#   arrange(desc(PY))
# 
# TC_wos <-
#   wos |>
#   dplyr::select(PY, TC) |>
#   dplyr::group_by(PY) |>
#   dplyr::summarise(TC_sum = sum(TC)) |>
#   arrange(desc(PY)) |>
#   na.omit()
# 
# 
# TC_scopus <-
#   scopus |>
#   dplyr::select(PY, TC) |>
#   dplyr::group_by(PY) |>
#   dplyr::summarise(TC_sum = sum(TC)) |>
#   arrange(desc(PY)) |>
#   na.omit()
# 
# TC_all <-
#   TC_scopus |>
#   left_join(TC_wos,
#             by = "PY",
#             suffix = c("_wos",
#                        "_scopus")) |>
#   replace_na(replace = list(TC_sum_scopus = 0)) |>
#   mutate(TC_sum_all = TC_sum_wos + TC_sum_scopus,
#          TC_total = sum(TC_sum_all),
#          TC_percentage = round(TC_sum_all/TC_total, digits = 2)) |>
#   select(PY, TC_sum_all, TC_percentage) |>
#   filter(PY != 2022) |>
#   filter(PY >= 2000) |>
#   arrange(desc(PY))

#### Table 2 - Countries ####

wos_scopus_countries <-
  wos_scopus |>
  select(SR, AU_CO, TC) |>
  separate_rows(AU_CO, sep = ";") |>
  unique() |>
  drop_na()

wos_scopus_countries_journals <-
  wos_scopus_countries |>
  left_join(wos_scopus |>
              select(SR, SO, PY),
            by = "SR")

# scimago_2020 <-
#   read_csv2("scimago2020.csv", show_col_types = FALSE) |>
#   select(SO = Title,
#          quartile = "SJR Best Quartile") |>
#   mutate(PY = 2020)

scimago <- 
  read_csv("https://docs.google.com/spreadsheets/d/1K_3QqjcD8Hab2ehXwE_1cm-6yhSoet13Q7qsULlnN1Y/export?format=csv&gid=1875866918") |> 
  # select(-1) |> 
  select(PY = ano,
         SO = revista,
         quartile = categoria) |> 
  mutate(SO = str_to_upper(SO)) |> 
  unique()

# scimago_2021 <-
#   read_csv2("scimago2020.csv", show_col_types = FALSE) |>
#   select(SO = Title,
#          quartile = "SJR Best Quartile") |>
#   mutate(PY = 2021)

# scimago_2020_2021 <-
#   scimago_2020 |>
#   bind_rows(scimago_2021) |>
#   select(PY, SO, quartile)

# scimago_1 <-
#   sjr_journals |>
#   select(PY = year,
#          SO = title,
#          quartile = sjr_best_quartile) |>
#   mutate(SO = str_to_upper(SO),
#          PY = as.numeric(PY)) |>
#   bind_rows(scimago_2020_2021)

AU_CO_links <- 
  wos_scopus_countries_journals |>
  left_join(scimago, by = c("PY", "SO")) |> 
  group_by(AU_CO) |> 
  filter(!duplicated(SR))

wos_scopus_countries_journals_scimago <-
  wos_scopus_countries_journals |>
  left_join(scimago, by = c("PY", "SO")) |>
  group_by(AU_CO) |> 
  filter(!duplicated(SR)) |> 
  # drop_na() |>
  # filter(n() != 1) |>
  select(SO, AU_CO, TC, quartile)  
# filter(quartile != "-")

# df_dummy <- 
#  wos_scopus_tos$df |> 
#  left_join(scimago, by = c("PY", "SO")) |> 
#  filter(!duplicated(SR))

table_2a_production <- 
  wos_scopus_countries |> 
  dplyr::select(AU_CO) |> 
  dplyr::group_by(AU_CO) |> 
  dplyr::summarise(count_co = n()) |> 
  dplyr::mutate(percentage_co = count_co / sum(count_co) * 100,
                percentage_co = round(percentage_co, digits = 2)) |> 
  dplyr::arrange(desc(count_co))

table_2b_citation <- 
  wos_scopus_countries_journals_scimago |> 
  dplyr::select(AU_CO, TC) |> 
  dplyr::group_by(AU_CO) |> 
  dplyr::summarise(citation = sum(TC)) |> 
  dplyr::mutate(percentage_ci = citation / sum(citation) * 100) |> 
  dplyr::arrange(desc(citation))

table_2c_quality <- 
  wos_scopus_countries_journals_scimago |> 
  dplyr::select(AU_CO, quartile) |> 
  dplyr::group_by(AU_CO) |> 
  dplyr::count(quartile, sort = TRUE) |> 
  pivot_wider(names_from = quartile, 
              values_from = n) |> 
  dplyr::select(AU_CO, Q1, Q2, Q3, Q4) |> 
  dplyr::mutate(Q1 = replace_na(Q1, 0),
                Q2 = replace_na(Q2, 0),
                Q3 = replace_na(Q3, 0),
                Q4 = replace_na(Q4, 0))

table_2 <- 
  table_2a |> 
  left_join(table_2b_citation, by = "AU_CO") |> 
  left_join(table_2c_quality, by = "AU_CO") |> 
  mutate(percentage_ci = round(percentage_ci, digits = 2),
         no_category = count_co - (Q1 + Q2 + Q3 + Q4)) |> 
  slice(1:11)

#### Figure 2 - Countries ####

df_dummy_3 <-
  wos_scopus |>
  select(SR, AU_CO) |>
  separate_rows(AU_CO, sep = ";") |>
  unique() |> 
  tidyr::drop_na()

# df_dummy_1 <- 
#   wos |> 
#   select(SR, AU_CO) |> 
#   separate_rows(AU_CO, sep = ";") |> 
#   unique()
# 
# df_dummy_2 <- 
#   scopus |> 
#   select(SR, AU_CO) |> 
#   separate_rows(AU_CO, sep = ";")
# 
# df_dummy_3 <- 
#   df_dummy_1 |> 
#   bind_rows(df_dummy_2) |> 
#   unique() |> 
#   na.omit()

edgelist_wos_countries <- data.frame(from = character(), 
                                     to = character(),
                                     SR = character(),
                                     # year = as.numeric(),
                                     stringsAsFactors = FALSE
)
# table_ids <- table(author_1$doi)
# table_ids_0 <- data.frame(table_ids)
# table_ids_1 <- table_ids_0[table_ids_0$Freq >= 2,]
list_ids_1 <- 
  df_dummy_3 |> 
  select(SR) |> 
  group_by(SR) |> 
  filter(n() > 1) |> 
  unique()

for (i in list_ids_1$SR) {
  df_1 = df_dummy_3[df_dummy_3$SR == i,]
  df_2 = combn(df_1$AU_CO, 2, simplify = FALSE)
  df_3 = data.frame((t(data.frame(df_2))), i)
  colnames(df_3) = c("from", "to", "SR")
  # df_4 <- df_3 |> bind_cols(df_1 |> select(year) |> unique())
  edgelist_wos_countries = rbind(edgelist_wos_countries, df_3)
}

edgelist_wos_countries_weighted <- 
  edgelist_wos_countries |> 
  dplyr::select(from, to) |> 
  dplyr::group_by(from, to) |> 
  dplyr::count(from, to) |> 
  filter(from != to)

# edgelist_scopus_countries <- data.frame(from = character(), 
#                                         to = character(),
#                                         SR = character(),
#                                         # year = as.numeric(),
#                                         stringsAsFactors = FALSE
# )
# # table_ids <- table(author_1$doi)
# # table_ids_0 <- data.frame(table_ids)
# # table_ids_1 <- table_ids_0[table_ids_0$Freq >= 2,]
# list_ids_2 <- 
#   df_dummy_2 |> 
#   select(SR) |> 
#   group_by(SR) |> 
#   filter(n() > 1) |> 
#   unique()
# 
# for (i in list_ids_2$SR) {
#   df_1 = df_dummy_2[df_dummy_2$SR == i,]
#   df_2 = combn(df_1$AU_CO, 2, simplify = FALSE)
#   df_3 = data.frame((t(data.frame(df_2))), i)
#   colnames(df_3) = c("from", "to", "SR")
#   # df_4 <- df_3 |> bind_cols(df_1 |> select(year) |> unique())
#   edgelist_scopus_countries = rbind(edgelist_scopus_countries, df_3)
# }
# 
# edgelist_scopus_countries_weighted <- 
#   edgelist_scopus_countries |> 
#   dplyr::select(from, to) |> 
#   dplyr::group_by(from, to) |> 
#   dplyr::count(from, to) |> 
#   filter(from != to)

# Merging both datasets 

edgelist_wos_scopus_countries <- 
  edgelist_wos_countries |> 
  # bind_rows(edgelist_scopus_countries) |> 
  unique() |> 
  mutate(PY = str_extract(SR, "[0-9]{4}"))

# edgelist_wos_scopus_countries_weighted <- 
#   edgelist_wos_scopus_countries |> 
#   dplyr::select(from, to) |> 
#   dplyr::group_by(from, to) |> 
#   dplyr::count(from, to) |> 
#   filter(from != to)

edgelist_wos_scopus_countries_weighted_properties <- 
  edgelist_wos_countries_weighted |> 
  tidygraph::as_tbl_graph(directed = FALSE) |> 
  tidygraph::activate(edges) |> 
  tidygraph::rename(weight = n) |> 
  activate(nodes) |> 
  dplyr::mutate(community = tidygraph::group_louvain(),
                degree = tidygraph::centrality_degree(),
                community = as.factor(community))
# Exporting data as igraph object

# edgelist_wos_scopus_countries_weighted_properties_nodes <-
#   edgelist_countries_weighted |>
#   activate(nodes) |>
#   tidygraph::as_tibble() |>
#   dplyr::rename(author = name) |>
#   tibble::rownames_to_column("name")
# 
# edgelist_wos_scopus_countries_weighted_properties_edges <-
#   edgelist_countries_weighted |>
#   tidygraph::activate(edges) |>
#   tidygraph::as_tibble()
# 
# edgelist_wos_scopus_countries_weighted_properties_nodes_igraph <-
#   graph_from_data_frame(d = edgelist_wos_scopus_countries_weighted_properties_edges,
#                         directed = FALSE,
#                         vertices = edgelist_wos_scopus_countries_weighted_properties_nodes)
# 
# write_graph(edgelist_wos_scopus_countries_weighted_properties_nodes_igraph,
#             "data/edgelist_wos_scopus_countries_weighted_properties_nodes_igraph.graphml",
#             "graphml") # Export




# net_dummy_wos <- 
#   df_dummy_1 |> 
#   group_by(SR) |> 
#   filter(n() > 1)
#   tidyr::expand_grid(from = df_dummy_1$SR,
#                      to = df_dummy_1$AU_CO) |> 
#   unique() |> 
#   filter( from != to)
#     
#   
# net_dummy_wos_1 <- 
#   wos |> 
#   biblioNetwork(analysis = "coupling",
#                 network = "countries") |>
#   graph_from_adjacency_matrix(mode = "undirected",
#                               weighted = TRUE) |>
#   simplify() |>
#   as_tbl_graph() |>
#   activate(nodes) |>
#   mutate(communities = group_components(type = "weak")) |>
#   filter(communities == 1)

#### Table 3 - Journals ####

table_1 <- 
  tibble(
    # wos = length(wos$SR), # Create a dataframe with the values.
    #      scopus = length(scopus$SR), 
         total = length(wos_scopus$SR))

wos_journal <- 
  wos_scopus |> 
  dplyr::select(journal = SO) |> 
  na.omit() |> 
  dplyr::group_by(journal) |> 
  dplyr::count(journal, sort = TRUE) |> 
  dplyr::slice(1:20) |>
  dplyr::rename(publications = n) 
  # dplyr::mutate(database = "wos")

# wos_journal <- 
#   wos |> 
#   dplyr::select(journal = SO) |> 
#   na.omit() |> 
#   dplyr::group_by(journal) |> 
#   dplyr::count(journal, sort = TRUE) |> 
#   dplyr::slice(1:20) |>
#   dplyr::rename(publications = n) |> 
#   dplyr::mutate(database = "wos")

# scopus_journal <- 
#   scopus |> 
#   dplyr::select(journal = SO) |> 
#   na.omit() |> 
#   dplyr::count(journal, sort = TRUE) |> 
#   dplyr::slice(1:20) |>
#   dplyr::rename(publications = n) |> 
#   dplyr::mutate(database = "scopus")

# total_journal <- 
#   wos_scopus |> 
#   dplyr::select(journal = SO) |> 
#   na.omit() |> 
#   dplyr::count(journal, sort = TRUE) |> 
#   dplyr::slice(1:20) |>
#   dplyr::rename(publications = n) |> 
#   dplyr::mutate(database = "total")

wos_scopus_total_journal <- 
  wos_journal |> 
  # dplyr::bind_rows(scopus_journal, 
  #                  total_journal) |> 
  # pivot_wider(names_from = database, 
  #             values_from = publications) |> 
  dplyr::arrange(desc(publications)) |> 
  # dplyr::slice(1:10) |> 
  # dplyr::mutate_all(~replace_na(., 0)) |> 
  dplyr::mutate(percentage = publications / (table_1 |> 
                  dplyr::pull(total)),
                percentage = round(percentage, digits = 2)) |> 
  dplyr::arrange(desc(publications))

#### Table 4 - Authors ####

data_biblio_wos <- biblioAnalysis(wos)

wos_authors <- 
  data_biblio_wos$Authors |> 
  data.frame() |> 
  dplyr::rename(authors_wos = AU, papers_wos = Freq) |> 
  dplyr::arrange(desc(papers_wos)) |> 
  dplyr::slice(1:10) |> 
  dplyr::mutate(database_wos = "wos")

data_biblio_scopus <- biblioAnalysis(scopus)

scopus_authors <- 
  data_biblio_scopus$Authors |> 
  data.frame() |> 
  dplyr::rename(authors_scopus = AU, papers_scopus = Freq) |> 
  dplyr::arrange(desc(papers_scopus)) |> 
  dplyr::slice(1:10) |> 
  dplyr::mutate(database_scopus = "scopus")

data_biblio_total <- biblioAnalysis(wos_scopus_tos$df)

total_authors <- 
  data_biblio_total$Authors |> 
  data.frame() |> 
  dplyr::rename(authors_total = AU, 
                papers_total = Freq) |> 
  dplyr::arrange(desc(papers_total)) |> 
  dplyr::slice(1:10) |> 
  dplyr::mutate(database_total = "total")

wos_scopus_authors <- 
  wos_authors |> 
  dplyr::bind_cols(scopus_authors,
                   total_authors)

#### Exporting data ####

list_of_files <- list(wos_scopus_tos = wos_scopus_tos$df,
                      wos = wos,
                      scopus = scopus,
                      reference_df = reference_df,
                      journal_df  = journal_df,
                      author_df = author_df,
                      TC_all = TC_all,
                      figure_1_data = figure_1_data,
                      au_co_quartile = au_co_quartile,
                      table_2_country = table_2,
                      figure_2_country_wos_scopus = edgelist_wos_scopus_countries,
                      figure_2_country_wos_scopus_1 = edgelist_wos_scopus_countries_weighted,
                      table_3_journal = wos_scopus_total_journal,
                      table_4_authors = wos_scopus_authors,
                      AU_links = AU_links
                      
)


write.xlsx(list_of_files, file = "all_data_jaime_STI.xlsx")
