---
title: "Org_Net_Ex_1"
output:
  pdf_document: default
  html_document: default
date: "2024-03-14"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
options(warn = -1)

connections <- read.csv("E:/Users/pc/Downloads/Connections_Sohail.csv", skip = 2)

Connections_Sohail = connections

library(tidygraph)
library(tidyverse)
library(igraph)
library(ggplot2)
library(vroom)
library(scales)
library(ggraph)
library(ggtext)
library(ggrepel)
library(ggforce)
library(ggthemes)
library(patchwork)
library(qualpalr)
library(viridis)
library(oaqc)

#Before Removing missing values

# Count the number of contacts by their current employer
counts_by_employer <- Connections_Sohail %>%
  group_by(Company) %>%
  summarise(count = n())

# Calculate the total count of contacts
total_count <- sum(counts_by_employer$count)

# Print the counts by employer
print(counts_by_employer)

# Print the total count
print(paste("Total count of contacts:", total_count))

```

```{r}
Connections_Sohail <- select(Connections_Sohail, -'Email.Address')
```

```{r}
Connections_Sohail <- Connections_Sohail %>%
  mutate(
    label = str_c(
      First.Name,                            
      str_sub(Last.Name, 1, 1),              
      as.character(row_number())             
    )
  ) %>%
  select(label, Company) %>%
  filter(!is.na(Company) & Company != "")   # Filter out NA and empty strings
```

#After removing missing values

```{r}
# Count the number of contacts by their current employer
counts_by_employer <- Connections_Sohail %>%
  group_by(Company) %>%
  summarise(count = n())

# Calculate the total count of contacts
total_count <- sum(counts_by_employer$count)

# Print the counts by employer
print(counts_by_employer)

# Print the total count
print(paste("Total count of contacts:", total_count))
```

#Summarising

```{r}
companies_with_more_than_5 <- Connections_Sohail %>%
  group_by(Company) %>%
  count(sort = TRUE) %>%
  filter(n > 5)

companies_with_more_than_5
```

#Histogram

```{r}
ggplot(companies_with_more_than_5, aes(x = reorder(Company, n), y = n, fill = Company)) +
  geom_bar(stat = "identity",width=0.8) +
  coord_flip() +
  labs(
    x = "Count of Contacts",
    y = "Company",
    title = "Companies with More Than 5 Contacts",
    subtitle = "Horizontal bar chart showing companies with more than 5 contacts"
  ) +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 8),  # Adjust font size of y-axis labels
    plot.subtitle = element_text(size = 12)
  )
```



```{r}
# Preparing a dataframe for graph creation
df_connections <- Connections_Sohail %>%
  filter(Company %in% companies_with_more_than_5$Company) %>%
  group_by(Company) %>%
  summarise(label_combinations = list(combn(label, 2, simplify = FALSE))) %>%
  unnest(label_combinations) %>%
  transmute(
    from = map_chr(label_combinations, 1),
    to = map_chr(label_combinations, 2),
    company = Company
  )

# View the resulting dataframe
print(df_connections)
```


```{r}
# Setting seed for reproducibility
set.seed(180)

# Sampling 30% of the connections and creating a graph
graph_data <- df_connections %>%
  slice_sample(prop = 0.30) %>%
  as_tbl_graph(directed = FALSE)

# Print the graph object to view its summary
print(graph_data)

# Generating a color palette
color_palette <- qualpal(
  n = graph_data %>%
    activate(edges) %>%
    pull(company) %>%
    unique() %>%
    length(), # Number of unique companies in the graph's edges
  colorspace = "pretty" # Specifies the color space for the palette
)
```

```{r}
# Create a data frame for plotting
palette_df <- data.frame(colors = color_palette$hex)

# Create a plot of the color palette with custom text sizes
ggplot(palette_df, aes(x = 1, y = colors, fill = colors, label = colors)) +
  geom_tile() + 
  geom_text(color = "white", size = 3) + # Adjust text size here
  scale_fill_identity() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8), # Adjust axis text size here
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
```

```{r}
#Example seeing connections in McGill University - Desautels Faculty of Management and Desauetls Capital #Management
# Extract names associated with McGill University
mcgill_names <- df_connections %>%
  filter(company %in% c(
    "McGill University - Desautels Faculty of Management",
    "Desautels Capital Management"
  )) %>%
  select(from, to) %>%
  pivot_longer(cols = c(from, to)) %>%
  distinct() %>%
  pull(value)
```


```{r}
# Create a graph layout
graph_layout <- create_layout(graph_data, layout = "backbone", keep = 0.7)

# Visualize the graph
graph_vis <- ggraph(graph_layout) +
  geom_node_point(
    size = 4,
    color = ifelse(
      graph_data %>%
        activate(nodes) %>%
        pull(name) %in% mcgill_names,
      "brown",
      "black"
    )
  ) +
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    max.overlaps = 5,
    check_overlap = TRUE,
    size = 2 # Adjust text size here if needed
  ) +
  geom_edge_link0(
    aes(color = company),
    show.legend = TRUE,
    width = 1
  ) +
  scale_edge_color_manual(values = color_palette$hex) +
  theme_void() +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(fill = NA)
  ) +
  labs(
    title = "LinkedIn Connections Network",
    subtitle = "Edges are colored based on the organization (sample of 30% of connections, organizations with >5 connections only)",
    caption = "Linkedin Connections Data by Ammad Sohail"
  )

# Preview the graph in the R plotting window
print(graph_vis)

# Saving the graph to a PNG file
ggsave("E:/Linkedin_Connections_Networks_Graph.png",
  graph_vis,
  width = 20,   # Set the width of the saved image
  height = 15,  # Set the height of the saved image
  dpi = 300,    # Set the resolution in dots per inch
  device = "jpg"  # Specify the type of graphics device
)

options(warn = 0)  # Reset to default behavior
```


