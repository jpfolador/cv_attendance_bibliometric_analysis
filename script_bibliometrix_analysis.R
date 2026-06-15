# Author: João Paulo Folador
# Note: the script can be executed all at once.
# You have to adjust the variable "workPath" to your dataset path.

#load or/and install all packages needed
if (!requireNamespace("bibliometrix", quietly = TRUE)) {
  install.packages("bibliometrix")
}else{
  library(bibliometrix)
}
if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  install.packages("rstudioapi")
}else{
  library(rstudioapi)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}else{
  library(ggplot2)
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}else{
  library(dplyr)
}
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}else{
  library(patchwork)
}
if (!requireNamespace("cowplot", quietly = TRUE)) {
  install.packages("cowplot")
}else{
  library(cowplot)
}
if (!requireNamespace("magick", quietly = TRUE)) {
  install.packages("magick")
}else{
  library(magick)
}
if (!requireNamespace("grid", quietly = TRUE)) {
  install.packages("grid")
}else{
  library(grid)
}
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}else{
  library(stringr)
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}else{
  library(tidyr)
}
if (!requireNamespace("igraph", quietly = TRUE)) {
  install.packages("igraph")
}else{
  library(igraph)
}


###
# Step 01:
# load, conversion, removal duplicated data and file junction
##
workPath = "data/"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

fileScopus <- c("scopus_string_01.csv", 
                "scopus_string_02.csv", 
                "scopus_string_03.csv", 
                "scopus_string_04.csv",
                "scopus_string_05.csv")

fileWos <- c("wos_string_01.txt",
             "wos_string_02.txt",
             "wos_string_03.txt",
             "wos_string_04.txt",
             "wos_string_05.txt")

dataScopus <- list()
totalRegistrosScopus <- list()
dataWos <- list()
totalRegistrosWos <- list()

for (i in 1:5) {
  fileTemp <- file.path(getwd(), fileScopus[i])
  dataScopus[[i]] <- convert2df(file = fileScopus[i], dbsource = "scopus", format = "csv")
  totalRegistrosScopus[[i]] <- nrow(dataScopus[[i]])
  
  fileTemp <- file.path(getwd(), fileWos[i])
  dataWos[[i]] <- convert2df(file = fileWos[i], dbsource = "wos", format = "plaintext")
  totalRegistrosWos[[i]] <- nrow(dataWos[[i]])
}

sum(unlist(totalRegistrosScopus))
dataScopusAll <- rbind(dataScopus[[1]], 
                       dataScopus[[2]], 
                       dataScopus[[3]], 
                       dataScopus[[4]],
                       dataScopus[[5]])

# Find common columns between all data frames
common_columns <- Reduce(intersect, list(colnames(dataWos[[1]]), 
                                         colnames(dataWos[[2]]), 
                                         colnames(dataWos[[3]]), 
                                         colnames(dataWos[[4]]),
                                         colnames(dataWos[[5]])))

dataWos[[1]] <- dataWos[[1]][, common_columns]
dataWos[[2]] <- dataWos[[2]][, common_columns]
dataWos[[3]] <- dataWos[[3]][, common_columns]
dataWos[[4]] <- dataWos[[4]][, common_columns]
dataWos[[5]] <- dataWos[[5]][, common_columns]

sum(unlist(totalRegistrosWos))
dataWosAll <- rbind(dataWos[[1]], 
                    dataWos[[2]], 
                    dataWos[[3]], 
                    dataWos[[4]], 
                    dataWos[[5]])

data <- mergeDbSources(dataScopusAll, dataWosAll, remove.duplicated = TRUE)

# ajusta o bug do nome da coluna CR_raw
colnames(data)[colnames(data) == "CR_raw"] <- "CR"

data <- data[, !duplicated(colnames(data))]

# save to disk if needed
#write.table(data, file.path(getwd(), "finalDataSet.csv"), sep = ";", row.names = FALSE)

# load dataset if salved before
#data <- read.csv(file.path(getwd(), "finalDataSet.csv"), sep = ";", header = TRUE)


#############################################
# Step 02: summarizes and explores the data #
#############################################

com <- missingData(data)
com$mandatoryTags

# make the bibliometrix analysis
results <- biblioAnalysis(data, sep = ";")

# sumaryze the data
S <- summary(object = results, k = 12, pause = FALSE)

# autores mais produtivos, paises mais produtivos, produção anual, média de 
# artigos citação por ano, e média total de citações anuais
plot(x = results, k = 12, pause = FALSE, remove.watermark = TRUE)


#####################################################
# Step 3: Build extra data visualization - Figure 2 #
#####################################################

### Annual production
paper_per_year <- as.data.frame(S$AnnualProduction)

# Adjust column names
colnames(paper_per_year) <- c("Year", "Articles")

citations_per_year <- data.frame(
  Year = results$Years, 
  Citations = results$TotalCitation
)
# group by year
citations_per_year <- citations_per_year %>%
  group_by(Year) %>%
  summarise(
    Citations = sum(Citations, na.rm = TRUE)
  )

# Ordenando o data frame por ano
#citations_per_year <- citations_per_year[order(citations_per_year$Year), ]

paper_citations_per_year <- merge(paper_per_year, citations_per_year, by = "Year", all = TRUE)
paper_citations_per_year <- paper_citations_per_year[order(paper_citations_per_year$Year), ]

plotCitArtAno <- ggplot(paper_citations_per_year, aes(x = Year)) +
  # Barras para citações
  geom_bar(aes(y = Citations, fill = "Citations"), stat = "identity", width = 0.7, alpha = 0.7) +
  
  # Linha para artigos publicados
  geom_line(aes(y = Articles, color = "Articles", group = 2), size = 1, linetype = 1) +
  geom_point(aes(y = Articles, color = "Articles"), shape = 20, size = 3) +
  
  # Ajustar eixos
  scale_y_continuous(
    name = "Total of citations", 
    sec.axis = sec_axis(~ ., name = "Total of published articles"),
    expand = c(0.1, 0.1)
  ) +
  
  # Escalas de cores e preenchimentos
  scale_color_manual(
    name = NULL,
    values = c("Articles" = "#642454")
  ) +
  scale_fill_manual(
    name = NULL, 
    values = c("Citation" = "#bfb5af")
  ) +

  # Personalização do tema e rótulos
  theme_classic() +
  labs(
    title = "A",
    x = "Year"
  ) +
  theme(
    text = element_text(size = 14, family = "serif"),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15), 
    axis.title.y.right = element_text(size = 15, angle = 90),
    panel.grid.major = element_line(color = "#EDE6D8"),
    legend.position = c(.03, .98),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.box.background = element_rect(color = "#EDE6D8", linewidth = 1),
    legend.margin = margin(4, 4, 4, 4),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
    plot.title = element_text(size = 16, color = "black", hjust = 0),
  )


###
# Build the graph of countries' production
###

# extract "mostProdCountries"
countries_df <- as.data.frame(S$MostProdCountries)

# rename the columns
colnames(countries_df) <- c("Country", "Articles")
countries_df$Articles <- as.numeric(countries_df$Articles) 
countries_df$Country <- as.character(trimws(countries_df$Country))

# build the graph
plotProdCountry <- ggplot(countries_df, aes(x = reorder(Country, Articles), y = Articles)) +
  geom_bar(stat = "identity", fill = "#027b7f", width = 0.7, alpha = 0.7) +
  #geom_text(aes(label = Articles), color = "#FFF", hjust = 1.1, vjust = 0.4, size = 4, family = "serif", fontface = "bold") +
  coord_flip() +  # Barras horizontais
  theme_classic() +
  labs(
    title = "B", #"Países Mais Produtivos",
    x = "Countries",
    y = "Number of documents"
  ) +
  scale_y_continuous(expand = c(0.01, 0.1)) +
  theme(
    text = element_text(size = 14, family = "serif"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 16, color = "black", hjust = 0),
    panel.grid.major = element_line(color = "#d1e5dd"),
    legend.position = "none",
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

###
# Most productive authors
###

# Extract "mostProdCountries"
authors_df <- as.data.frame(S$MostProdAuthors)

# Rename the columns
colnames(authors_df) <- c("Authors", "Articles")
authors_df$Articles <- as.numeric(authors_df$Articles)  
authors_df$Authors <- as.character(trimws(authors_df$Authors))

# the graph
plotAutorProd <- ggplot(authors_df, aes(x = reorder(Authors, Articles), y = Articles)) +
  geom_bar(stat = "identity", fill = "#5c564a", width = 0.7, alpha = 0.7) +
  #geom_text(aes(label = Articles), color = "#FFF", hjust = 1.1, vjust = 0.4, size = 4, family = "serif", fontface = "bold") +
  coord_flip() +  # Barras horizontais
  theme_classic() +
  labs(
    title = "C",
    x = "Authors",
    y = "Number of documents"
  ) +
  scale_y_continuous(expand = c(0.01, 0.1)) +
  theme(
    text = element_text(size = 14, family = "serif"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, hjust = 1),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 16, color = "black", hjust = 0),
    panel.grid.major = element_line(color = "#EDE6D8"),
    legend.position = "none",
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )


###
# Key-words
###

keywords_df <- as.data.frame(S$MostRelKeywords)
colnames(keywords_df) <- trimws(colnames(keywords_df))
keywords_df$`Author Keywords (DE)` <- trimws(keywords_df$`Author Keywords (DE)`)

keywords_df <- data.frame(
  Keyword = keywords_df$`Author Keywords (DE)`,  # Coluna limpa
  Frequency = as.numeric(keywords_df$Articles)  # Converter Articles para numérico
)

plotKeyWords <- ggplot(keywords_df, aes(x = reorder(Keyword, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "#642454", size = 1, width = 0.7, alpha = 0.7) +
  #geom_text(aes(label = Frequency), color = "#FFF", hjust = 1.1, vjust = 0.4, size = 4, family = "serif", fontface = "bold") +
  theme_classic() +
  coord_flip() + # Barra horizontal
  labs(
    title = "D", #"Palavras-Chave mais Relevantes",
    x = "Keywords",
    y = "Number of occurrences"
  ) + 
  scale_y_continuous(expand = c(0.01, 0.1)) +
  theme(
    text = element_text(size = 14, family = "serif"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, hjust = 1),
    axis.title.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 16, color = "black", hjust = 0),
    panel.grid.major = element_line(color = "#EDE6D8"),
    legend.position = "none",
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5) 
  )

# Build the four graphs in a 2x2 matrix
final_plot <- (plotCitArtAno | plotProdCountry) / (plotAutorProd | plotKeyWords)

# show the graphs and save in disk
final_plot
ggsave("figure_2.png", final_plot, width = 16, height = 9)


########################################
# Step 04: Country Network - Figure 03 #
########################################
# function toLowercase
fmt_country <- function(x) {
  stringr::str_to_title(stringr::str_squish(x))
}

# function to adjust the author's names (ex.: "DE FAUW J." / "KUMAR A.")
fmt_author <- function(x) {
  x <- toupper(str_squish(x))
  parts <- unlist(strsplit(x, "[, ]+"))
  parts <- parts[nchar(parts) > 0]
  if (length(parts) == 0) return(x)
  
  is_init <- nchar(parts) == 1
  if (any(is_init)) {
    last  <- paste(parts[!is_init], collapse = " ")
    inits <- paste0(parts[is_init], collapse = "")
  } else {
    last  <- parts[1]
    inits <- paste0(substr(parts[-1], 1, 1), collapse = "")
  }
  inits <- if (nzchar(inits)) paste0(paste0(strsplit(inits, "")[[1]], ".", collapse = ""), "") else ""
  str_squish(paste(last, inits))
}

###
# Extract the Countries Collaboration
###
n = 30
M <- metaTagExtraction(data, Field = "AU_CO", sep = ";")
netMatrixCountryCollab <- biblioNetwork(
  M, 
  analysis = "collaboration", 
  network = "countries", 
  sep = ";", 
  n = n
)

countryNames <- fmt_country(rownames(netMatrixCountryCollab))
dimnames(netMatrixCountryCollab) <- list(countryNames, countryNames)

png("plot1.png", width = 2000, height = 2000, res = 300)
netContryCollab = networkPlot(
  netMatrixCountryCollab, 
  Title = "",
  type = "fruchterman", 
  normalize = "equivalence", 
  weighted = TRUE, 
  size = TRUE,
  edgesize = 4, 
  edges.min = 0,
  labelsize = 1.1,
  label.color = TRUE,
  label = TRUE,
  cluster = "optimal",
  remove.multiple = TRUE,
  remove.isolates = TRUE,
  community.repulsion = 0.009,
  curved = 0.6,
  alpha = 0.6,
)

# Adjust the graph labels
g <- netContryCollab$graph
V(g)$label <- fmt_country(V(g)$name)
plot(g, vertex.label = V(g)$label)

mtext("A", side = 3, line = 0.3, adj = 0, col = "black", font = 1, family = "serif", cex = 1.5)
par(mar = c(4, 0.5, 2, 0))
box(lwd = 1, col = "grey")
dev.off()

###
# Extract authors collaboration
###
netMatrixAuthorCollab <- biblioNetwork(
  M,
  analysis = "collaboration",
  network = "authors",
  n = n,
  sep = ";"
)

authorNames <- sapply(rownames(netMatrixAuthorCollab), fmt_author, USE.NAMES = FALSE)
dimnames(netMatrixAuthorCollab) <- list(authorNames, authorNames)

png("plot2.png", width = 2000, height = 2000, res = 300)
netAuthorCollab <- networkPlot(
  netMatrixAuthorCollab,
  Title = "",
  type = "kamada", 
  normalize = "association", 
  weighted = TRUE, 
  size = TRUE,
  edgesize = 4, 
  edges.min = 0,
  labelsize = 1.2,
  label.color = TRUE,
  label = TRUE,
  cluster = "optimal",
  remove.multiple = TRUE,
  remove.isolates = TRUE,
  community.repulsion = 0.003,
  curved = 0.6,
  alpha = 0.6,
)

# adjust the graph labels
g2 <- netAuthorCollab$graph
V(g2)$label <- sapply(V(g2)$name, fmt_author)
plot(g2, vertex.label = V(g2)$label)

mtext("B", side = 3, line = 0.3, adj = 0, col = "black", font = 1, family = "serif", cex = 1.5)
par(mar = c(4, 1, 2, 0.2))
box(lwd = 1, col = "grey")
dev.off()

# bind the two plot, build the figure 03, show and save to disk
p1 <- ggdraw() + draw_image("plot1.png")
p2 <- ggdraw() + draw_image("plot2.png")
final_plot <- plot_grid(p1, p2, ncol = 2, nrow = 1)

ggsave("figure_3_collab_countries_authors.png", 
       final_plot, width = 16, height = 9, dpi = 300)


######################################################
# Steop 5: keyword co-occurrences network - Figure 4 #
######################################################

fmt_keyword <- function(x) {
  stringr::str_to_title(stringr::str_squish(x))
}

###
# Extract Keywords co-occurrence
###
n = 30 
M <- metaTagExtraction(data, Field = "DE", sep = ";")
NetMatrixKeywordCooc <- biblioNetwork(
  M, 
  analysis = "co-occurrences", 
  network = "keywords", 
  sep = ";", 
  n = n
)

keywordNames <- fmt_keyword(rownames(NetMatrixKeywordCooc))
dimnames(NetMatrixKeywordCooc) <- list(keywordNames, keywordNames)

png("plot3.png", width = 2000, height = 2000, res = 300)
netKeywordCooc <- networkPlot(
  NetMatrixKeywordCooc, 
  normalize = "equivalence",  
  weighted = TRUE, 
  Title = "", 
  type = "kamada", 
  size = TRUE,
  edgesize = 4, 
  edges.min = 0,
  labelsize = 1.1,
  cluster = "optimal",
  remove.multiple = TRUE,
  remove.isolates = TRUE,
  community.repulsion = 0.0092,
  curved = 0.6,
  alpha = 0.6
)

# to uppercase
g3 <- netKeywordCooc$graph
V(g3)$label <- fmt_keyword(V(g3)$name)
plot(g3, vertex.label = V(g3)$label)

mtext("A", side = 3, line = 0.3, adj = 0, col = "black", font = 1, family = "serif", cex = 1.5)
par(mar = c(4, 0.5, 2, 0))
box(lwd = 1, col = "grey")
dev.off()

###
# Extract Co-citation (references)
###
fmt_ref <- function(x) toupper(str_squish(x))

M_ref <- metaTagExtraction(data, Field = "CR", sep = ";")
NetMatrixCitationRef <- biblioNetwork(
  M_ref, 
  analysis = "co-citation", 
  network = "references", 
  sep = ";",
  n = n
)

# Remove números e hífens
listaAutoresComAno <- NetMatrixCitationRef@Dimnames[[1]]
lista_sem_numeros <- lapply(listaAutoresComAno, function(x) gsub("[0-9-]", "", x))
resultado <- unlist(lista_sem_numeros)
resultado_unico <- make.unique(fmt_ref(resultado))

NetMatrixCitationRef@Dimnames[[1]] <- resultado_unico
NetMatrixCitationRef@Dimnames[[2]] <- resultado_unico

png("plot4.png", width = 2000, height = 2000, res = 300)
netCitationRef <- networkPlot(
  NetMatrixCitationRef, 
  Title = "",
  normalize = "equivalence", 
  weighted = TRUE, 
  type = "fruchterman", 
  size = TRUE,
  edgesize = 4, 
  edges.min = 0,
  labelsize = 1.0,
  cluster = "optimal",
  remove.multiple = TRUE,
  remove.isolates = TRUE,
  community.repulsion = 0,
  curved = 0.6,
  alpha = 0.6
)

g4 <- netCitationRef$graph
V(g4)$label <- fmt_ref(V(g4)$name)
plot(g4, vertex.label = V(g4)$label)

mtext("B", side = 3, line = 0.3, adj = 0, col = "black", font = 1, family = "serif", cex = 1.5)
par(mar = c(4, 1, 2, 0.2))
box(lwd = 1, col = "grey")
dev.off()

# build the graph A and B, show it and save to disk
p3 <- ggdraw() + draw_image("plot3.png")
p4 <- ggdraw() + draw_image("plot4.png")
final_plot2 <- plot_grid(p3, p4, ncol = 2, nrow = 1)

ggsave("figure_4_cocolab.png", final_plot2, width = 16, height = 9, dpi = 300)


#################################################
# Final step: Build the most cited papers table #
#################################################
mostImportantPapers <- data.frame(
  "autor" = data$AU,
  "titulo" = data$TI,
  "ano" = data$PY,
  "doi" = data$DI,
  "citacao" = data$TC
)
mostImportantPapers <- mostImportantPapers %>%
  arrange(desc(citacao)) %>%
  distinct(autor, .keep_all = TRUE) %>%
  slice_head(n = 30)

write.csv(mostImportantPapers, "top30_most_cited_papers.csv", row.names = FALSE)

# Note: some missing DOIs came from the scientific database and 
#       need to be found manually
