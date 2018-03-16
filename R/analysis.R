library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(viridis)
library(jkmisc)
library(RColorBrewer)
library(treemap)
library(grid)

blooms_key <- c("Remember", "Understand","Apply", "Analyze", "Evaluate", "Create")
solo_key <- c("Pre-structural", "Unistructural", "Multistructural", "Relational", "Extended Abstract")

lo <- dir(here('data'), full.names = TRUE, pattern = "taxonomy") %>% 
  read_excel(sheet = 1) %>% 
  clean_names() %>% 
  set_names(c("course", "outcome", "verb")) %>% 
  mutate_all(tolower)

blooms <- dir(here('data'), full.names = TRUE, pattern = "taxonomy") %>% 
  read_excel(sheet = 2) %>% 
  clean_names() %>% 
  set_names(c("verb", "blooms")) %>% 
  mutate_all(tolower)

solo <- dir(here('data'), full.names = TRUE, pattern = "taxonomy") %>% 
  read_excel(sheet = 3) %>% 
  clean_names() %>% 
  set_names(c("verb", "solo")) %>% 
  mutate_all(tolower)

lo_data <- lo %>% 
  left_join(blooms) %>% 
  inner_join(solo)


# Course Level Heat Maps ----

course_data <- lo_data  %>% 
  count(course, verb, blooms, solo) %>%
  ungroup() %>% 
  mutate(blooms = factor(blooms, 1:6, blooms_key),
         solo = factor(solo, 1:5, solo_key)) %>% 
  group_by(course) %>%  
  mutate(prop = n/sum(n)) %>% 
  ungroup() %>% 
  complete(course, blooms, solo, fill = list(prop = NA))

course_heatmap <- ggplot(course_data, aes(x = blooms, y = solo, fill = prop)) +
  geom_tile(color = "grey20", size = 0.1) +
  coord_equal() +
  facet_wrap(~course) +
  scale_fill_distiller(palette = "Reds", na.value = "white", direction = 1) +
  theme_jk() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave(plot = course_heatmap, here("plots/course heatmap.png"), width = 10, height = 5)


# Program Level Heat Map ----
prog_data <- lo_data %>% 
  count(verb, blooms, solo) %>%
  ungroup() %>% 
  mutate(blooms = factor(blooms, 1:6, blooms_key),
         solo = factor(solo, 1:5, solo_key)) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup() %>% 
  complete(blooms, solo, fill = list(prop = NA))


prog_heatmap <- ggplot(prog_data, aes(x = blooms, y = solo, fill = prop)) +
  geom_tile(color = "grey20", size = 0.1) +
  coord_equal() +
  scale_fill_distiller(palette = "Reds", na.value = "white", direction = 1) +
  theme_jk()
  

ggsave(plot = prog_heatmap, here("plots/Program heatmap.png"))

# Program Treemap function to produce final graphics ----
tree_data <- lo_data %>% 
  count(course, verb, blooms, solo) %>%
  ungroup() %>% 
  mutate(blooms = factor(blooms, 1:6, blooms_key),
         solo = factor(solo, 1:5, solo_key))

vplayout <- function(x, y) viewport(width = 25, height = 14, layout.pos.row = x, layout.pos.col = y)

pdf(here("plots/Framework Maps.pdf"), width = 11, height = 8.5)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
par(mai = c(0,0,0,0))


treemap(tree_data,
          index = c("blooms", "verb", "course"),
          vSize = "n",
          vColor = "blooms",
          type = "categorical",
          palette = tol6qualitative,
          title = "Program Cognitive Classifcation (Blooms)",
          title.legend = "",
          fontsize.labels = c(0, 30, 20),
          fontcolor.labels = "#f0f0f0",
          inflate.labels = FALSE,
          lowerbound.cex.labels = 1,
          bg.labels = 0,
          position.legend = "bottom",
          border.col = "white",
          vp = vplayout(1,1),
          border.lwds = c(2, 2, 1),
          align.labels = list(c("left", "top"), c("left", "top"), c("right", "bottom")),
          drop.unused.levels = FALSE,
          aspRatio = NA)

treemap(tree_data,
        index = c("solo", "verb", "course"),
        vSize = "n",
        vColor = "solo",
        palette = tol6qualitative,
        type = "categorical",
        title = "Program Outcome Structure (SOLO)",
        title.legend = "",
        fontsize.labels = c(0, 30, 20),
        fontcolor.labels = "#f0f0f0",
        inflate.labels = FALSE,
        lowerbound.cex.labels = 1,
        bg.labels = 0,
        position.legend = "bottom",
        border.col = "white",
        border.lwds = c(2, 2, 1),
        vp = vplayout(2,1),
        align.labels = list(c("left", "top"), c("left", "top"), c("right", "bottom")),
        drop.unused.levels = FALSE,
        aspRatio = NA)
dev.off()
  
