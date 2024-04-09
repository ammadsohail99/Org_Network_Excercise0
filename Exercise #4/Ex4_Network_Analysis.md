---
title: "Exercise 4"
output:
  pdf_document: 
    toc: true
    number_sections: true
    latex_engine: xelatex
  always_allow_html: true
date: "2024-04-09"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Load necessary libraries for data manipulation, visualization, and analysis

```{r}
# Loading necessary libraries
library(arrow)       # For reading/writing Apache Parquet files
library(gender)      # For predicting gender based on first names
library(dplyr)       # For data manipulation
library(tidyr)       # For tidying data
library(wru)         # For predicting race
library(lubridate)   # For working with dates
library(ggplot2)     # For data visualization
library(ggthemes)    # For additional ggplot themes
library(lattice)     # For additional plotting options
library(tidyverse)   # For a cohesive data analysis workflow
library(ggridges)    # For creating ridge plots
library(tidygraph)   # For graph data structures
library(ggraph)      # For graph visualization
library(webshot2)     # For taking html widgets snapshots to knit in PDF
```

## Load Data

### To begin the analysis, we load our primary datasets: patent applications stored in a Parquet file and a CSV file containing edges that represent relationships between patent examiners. This step sets the foundation for our examination of the USPTO's patent examination process.

```{r}
# Define the path to the data directory
data_path <- "E:/Users/pc/Downloads/672_project_data/"

# Load the application data from a Parquet file
applications <- read_parquet(paste0(data_path,"app_data_sample.parquet"))

# Load the edges data from a CSV file
edges <- read.csv(paste0(data_path,"edges_sample.csv"))
```

## Reveiwing the parquet file

```{r}
head(applications)
```

## Reviewing the csv file

```{r}
head(edges)
```

## Get gender for examiners

### We’ll get gender based on the first name of the examiner, which is recorded in the field examiner_name_first. We’ll use library gender for that, relying on a modified version of their own example.

###Note that there are over 2 million records in the applications table – that’s because there are many records for each examiner, as many as the number of applications that examiner worked on during this time frame. Our first step therefore is to get all unique names in a separate list examiner_names. We will then guess gender for each one and will join this table back to the original dataset. So, let’s get names without repetition:

```{r}
# get a list of first names without repetitions
examiner_names <- applications %>% distinct(examiner_name_first)

head(examiner_names)
```

### Now let’s use function gender() as shown in the example for the package to attach a gender and probability to each name and put the results into the table examiner_names_gender

```{r}
# Use the gender package to estimate gender based on first names
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>%
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  ) %>% 
  # Filter out rows where any of the specified columns are NA
  filter(!is.na(gender))

print(head(examiner_names_gender))
```

### Finally, let’s join that table back to our original applications data and discard the temporary tables we have just created to reduce clutter in our environment.

```{r}
# remove extra colums from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)

# joining gender back to the dataset
applications <- applications %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")

# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
```

## Guess the examiner’s race

### We’ll now use package wru to estimate likely race of an examiner. Just like with gender, we’ll get a list of unique names first, only now we are using surnames.

```{r}
# Isolate unique last names for race prediction
examiner_surnames <- applications %>% 
  select(surname = examiner_name_last) %>% 
  distinct()

head(examiner_surnames)
```

### We’ll follow the instructions for the package outlined here https://github.com/kosukeimai/wru.

```{r}
# Use the wru package to estimate race based on surnames
examiner_race <- examiner_surnames %>%
  # Ensure we're working with clean, non-NA surnames
  filter(!is.na(surname)) %>%
  # Apply the race prediction
  predict_race(voter.file = ., surname.only = TRUE) %>% 
  as_tibble()
```

### Exploring examiner_race

```{r}
head(examiner_race)
```

### As you can see, we get probabilities across five broad US Census categories: white, black, Hispanic, Asian and other. (Some of you may correctly point out that Hispanic is not a race category in the US Census, but these are the limitations of this package.)

### Our final step here is to pick the race category that has the highest probability for each last name and then join the table back to the main applications table. See this example for comparing values across columns: https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-rowwise/. And this one for case_when() function: https://dplyr.tidyverse.org/reference/case_when.html.

```{r}
# Determine the most likely race category for each surname
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))

head(examiner_race)
```

### Let’s join the data back to the applications table.

```{r}
# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)

# Join the race predictions back to the main applications dataset
applications <- applications %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))

# Again, clean up the workspace by removing temporary variables
rm(examiner_race)
rm(examiner_surnames)
gc()
```
## Examiner’s tenure

### To figure out the timespan for which we observe each examiner in the applications data, let’s find the first and the last observed date for each examiner. We’ll first get examiner IDs and application dates in a separate table, for ease of manipulation. We’ll keep examiner ID (the field examiner_id), and earliest and latest dates for each application (filing_date and appl_status_date respectively). We’ll use functions in package lubridate to work with date and time values.

```{r}
# Extract relevant date information for each application
examiner_dates <- applications %>% 
  select(examiner_id, filing_date, appl_status_date) 

head(examiner_dates)
```

### The dates look inconsistent in terms of formatting. Let’s make them consistent. We’ll create new variables start_date and end_date.

```{r}
# Standardize date formats and calculate tenure
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))
```

### Let’s now identify the earliest and the latest date for each examiner and calculate the difference in days, which is their tenure in the organization.

```{r}
# Calculate the tenure for each examiner based on the earliest and latest dates observed
examiner_tenure <- examiner_dates %>%
  # Remove rows with NA in start_date or end_date before grouping and summarising
  filter(!is.na(start_date) & !is.na(end_date)) %>%
  group_by(examiner_id) %>%
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1),
    .groups = 'drop' # Automatically drop the grouping
  ) %>% 
  # Keep records with a latest_date before 2018
  filter(year(latest_date) < 2018)

# Assuming you want to check the result
head(examiner_tenure)
```

### Joining back to the applications data.

```{r}
applications <- applications %>% 
  left_join(examiner_tenure, by = "examiner_id")

rm(examiner_tenure)
gc()
```
### Review the applications dataframe after merging examiner_tenure

```{r}
head(applications)
```

## Processing Time Calculation

### A crucial aspect of our analysis is the calculation of application processing time, the duration from filing to the final decision. This step is vital for understanding efficiency within the patent examination process.

```{r}
# Dropping applications with "Pending" status to focus on completed cases
applications <- applications %>%
  filter(disposal_type != "PEND")

# Calculating application processing time
applications <- applications %>%
  mutate(app_proc_time = interval(
    ymd(filing_date),
    dmy_hms(appl_status_date)
  ) %/% days(1))

# Final cleanup
gc()

# Previewing the updated dataset
head(applications)
```


## Preparing Edge Data

### This code block transforms the edges dataframe, which contains relationships between patent examiners, by ensuring the examiner IDs (both ego_examiner_id and alter_examiner_id) are character strings. This is crucial for graph analysis as it ensures consistency in node identification. We also drop any rows with missing values to maintain data integrity.

```{r}
edges <- edges %>%
  mutate(
    from = as.character(ego_examiner_id), # Convert IDs to character for graph compatibility
    to = as.character(alter_examiner_id)
  ) %>%
  drop_na() # Remove rows with missing values
```


## Creating a Network Graph

### We then prepare the applications dataframe for integration into the network graph. This includes relocating examiner_id for easier access, converting IDs to character strings for consistency with edge data, and renaming examiner_id to name for clarity. A directed graph is created from the edges dataframe, incorporating examiner data from applications.


```{r}
# Preparing applications data for graph creation
applications <- applications %>%
  relocate(examiner_id, .before = application_number) %>%
  mutate(examiner_id = as.character(examiner_id)) %>%
  drop_na(examiner_id) %>%
  rename(name = examiner_id)

# Creating a directed graph from the edges data
graph <- tbl_graph(
  edges = (edges %>% relocate(from, to)),
  directed = TRUE
)

# Enriching graph nodes with examiner data from applications
graph <- graph %>%
  activate(nodes) %>%
  inner_join(
    (applications %>% distinct(name, .keep_all = TRUE)),
    by = "name"
  )

# Display the graph structure
graph
```

The network comprises 2,489 nodes and 17,720 edges, characterizing a directed multigraph with significant interactions among patent examiners at the USPTO. The designation as a "directed multigraph" is particularly telling, indicating not only the directional nature of these interactions (suggesting a flow of communication or influence from one examiner to another) but also the presence of multiple connections between the same pairs of examiners. 

This could reflect recurring collaborations or consultations on various patent applications, highlighting a complex web of professional relationships. The existence of 127 distinct components within this network suggests a segmented operational structure, where clusters of examiners may work more closely with each other, potentially aligned by specialization areas or other organizational divisions. This fragmentation could mirror the diverse technical fields covered by patent applications, indicating a natural grouping of examiners based on expertise or departmental organization.


## Calculating Centrality Measures

### After constructing the network graph, we calculate centrality measures (degree, betweenness, closeness) for each node (examiner) to assess their influence within the network. These measures are then arranged by degree to highlight the most central examiners. The result is converted to a tibble for easy viewing and manipulation.

```{r}
node_data <- graph %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),
    betweenness = centrality_betweenness(),
    closeness = centrality_closeness()
  ) %>%
  arrange(-degree) %>%
  as_tibble() %>%
  mutate(tc = as.factor(tc))

node_data
```

## Regression Analysis with Plotting

### Finally, the run_regression function is designed to perform linear regression analysis based on specified predictor (x) and response (y) variables. If desired, it also generates a scatter plot with a regression line and saves it as a PNG file. This function simplifies the process of examining relationships between variables, such as the effect of centrality measures on application processing times.


```{r}
# Function to run regression and optionally generate and save a plot
run_regression <- function(data, x, y, plot = TRUE) {
  # Construct the regression formula
  formula <- as.formula(paste(y, "~", x))
  
  # Fitting the linear model
  model <- lm(formula, data = data)
  
  # Conditionally generate and save the plot
  if (plot) {
    # Prepare and display the plot
    plot_data <- ggplot(data, aes_string(x, y)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("Regression of", y, "on", x),
           subtitle = paste("R-squared:", round(summary(model)$r.squared, 4)),
           x = x, y = y) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 14)) +
      labs(caption = "Source: USPTO Data")
    
    # Save the plot to a specified path
    ggsave(paste0("E:/", y, "_on_", x, ".png"), plot_data, width = 16, height = 9)
    
    # Display the plot
    print(plot_data)
  }
  
  # Extract model summary statistics
  tidy_model <- broom::tidy(model)
  glance_model <- broom::glance(model)
  
  # Enhance the summary dataframe with R-squared and centrality measure
  tidy_model <- tidy_model %>%
    mutate(r_squared = glance_model$r.squared,
           centrality_measure = x)
  
  # Return the enhanced summary dataframe
  return(tidy_model)
}

```

##Running Regression Models on Centrality Measures


### Initiating a regression analysis exploring how the degree centrality of examiners correlates with application processing times. Degree centrality represents the number of direct connections an examiner has within the network, serving as a proxy for their involvement and potential influence in the patent examination process. The expectation is that examiners with higher degree centrality might expedite or delay the processing due to their central role in the collaborative network.

```{r}
# Running regression with Degree Centrality as the predictor for Application Processing Time
run_regression(node_data, "degree", "app_proc_time")
```
Observation:
The R-squared value of 0.0021 suggests that only a very small portion of the variability in application processing times can be explained by degree centrality alone. This indicates that degree centrality is not a strong predictor of processing times in this context.
The regression line has a positive slope, as indicated by the estimate for degree (approximately 4.087). This suggests that, on average, an increase in degree centrality is associated with a slight increase in processing time. However, the small coefficient means that the effect is quite minimal.
The wide spread of points and the broad confidence interval band signal that there is a high degree of variance in processing times that is not captured by degree centrality alone. This implies that there are likely other factors at play influencing the processing time which are not included in this model.

The p-value for degree centrality is approximately 0.023, which is below the conventional threshold of 0.05 for statistical significance. This indicates that there is a statistically significant relationship between degree centrality and processing time, albeit the actual impact is small as reflected in the R-squared value.
The intercept value (around 2468.65) represents the expected processing time when degree centrality is zero. This can be interpreted as the base processing time for an examiner with no direct connections in the network.

The findings suggest that while there is a statistically significant relationship between an examiner's degree centrality and the processing time of applications, the effect size is very small. It implies that other unaccounted-for variables might have a more substantial impact on processing times. These could include the complexity of the patent application, the examiner's expertise, workload, or even external factors like the technological area of the patent application.

The results also prompt a deeper reflection on the structure and dynamics of the USPTO's patent examination process. For instance, it may be that examiners with higher degree centrality face a more complex and demanding caseload due to their central position within the examiner network, which could slightly increase processing times. Alternatively, it might be that the degree centrality captures only a fraction of the social capital or influence an examiner wields, which could have nuanced effects on their efficiency and the resources they can marshal.


### Shifting focus to betweenness centrality, measuring an examiner's capacity to act as a bridge within the network. This analysis seeks to understand if examiners who frequently connect disparate groups impact the efficiency of patent processing differently, potentially by facilitating knowledge transfer or creating bottlenecks.

```{r}
# Running regression with Betweenness Centrality as the predictor for Application Processing Time
run_regression(node_data, "betweenness", "app_proc_time")

```

Observations:
The R-squared value is exceedingly low (1e-04), indicating that betweenness centrality has virtually no explanatory power in predicting application processing times. The ability of an examiner to act as a bridge between different parts of the network (as measured by betweenness centrality) does not seem to influence the speed at which they process patent applications.
The regression line appears almost horizontal, suggesting no significant trend between betweenness centrality and processing time. The confidence interval is broad and spans the y-axis, reinforcing the lack of a clear predictive relationship.
The insignificant relationship suggests that while betweenness might be important for communication, it does not translate into processing efficiency. This could be due to a variety of reasons, such as the nature of tasks performed by central individuals, which may involve more complex decision-making or mentoring responsibilities that do not directly affect processing times.

Given the lack of a significant relationship, it would be prudent to investigate other factors that might influence processing times. This could include workload, the complexity of applications, or the efficiency of support systems within the USPTO.
It's also possible that betweenness centrality needs to be combined with other network measures or job-related factors to have a noticeable impact on processing times. This could point to a more complex interplay of factors that determine how quickly applications are processed.
It may be valuable to examine the network structure more closely, potentially identifying subnetworks or roles within the network that correlate more strongly with processing time. Understanding the microstructures within the larger network could yield more nuanced insights.


### Closeness centrality assesses how close an examiner is to all other nodes in the network, indicative of their accessibility. This regression model probes whether examiners who can more readily access or be accessed by others influence application processing times, perhaps by being more efficient in information dissemination or coordination.

```{r}
# Running regression with Closeness Centrality as the predictor for Application Processing Time
run_regression(node_data, "closeness", "app_proc_time")
```

Observations:
The R-squared value rounds to zero, suggesting that closeness centrality does not explain any variance in application processing times. This indicates that the closeness of an examiner to all others in the network does not impact how quickly they process applications.
The regression line is almost perfectly horizontal, reinforcing the interpretation that there's no discernible trend between closeness centrality and processing time within this dataset.

With a p-value well above the conventional 0.05 threshold (p-value ≈ 0.857), the relationship between closeness centrality and processing time is not statistically significant. Thus, any observed effect is likely due to random variation rather than a systematic influence of closeness centrality on processing times.
The intercept, approximately 2595.93, suggests the average processing time when closeness centrality is zero. Given the context, this may reflect a baseline processing time independent of the examiner's network position.

The lack of a significant relationship in this context suggests that while closeness may be advantageous for information dissemination and communication, it doesn't necessarily correlate with faster administrative processing. This could indicate that USPTO examiners’ efficiency is influenced more by other factors, such as individual work methods, the complexity of the applications they review, or even the specific procedures and protocols they must follow, which are not captured by network centrality.

This analysis suggests a need to examine additional factors beyond the scope of traditional network centrality measures to fully understand the dynamics of application processing times. Other considerations might include the technical complexity of applications, examiner workload and experience, and organizational support structures. It also suggests that the role of individual examiners within the network may be multifaceted, with centrality capturing only a narrow aspect of their professional interactions and contributions to the patent examination process.


## Advanced Regression Analysis Incorporating Demographic Interactions

### Extending the analysis by introducing interaction terms between centrality measures and demographic variables (gender and race). It systematically examines how the relationship between centrality and processing times may vary across different demographic groups, leveraging the map_dfr function for efficient iteration across centrality types. This approach acknowledges the complexity of the examiner network, considering the multifaceted influences on patent processing times.

```{r}
# Conducting regression analyses to explore the interactions between centrality measures, gender, and race on Application Processing Time
centrality_measures <- c("degree", "betweenness", "closeness")

results_df <- map_dfr(
  centrality_measures,
  ~ run_regression(node_data,
    paste0(.x, " * gender * race"),
    "app_proc_time",
    plot = FALSE
  )
)

# Extracting and displaying the R-squared values for each model to assess their explanatory power
results_df %>%
  select(centrality_measure, r_squared) %>%
  distinct()
```

Observations:
All three R-squared values are very low (ranging from 0.0046 to 0.0085), indicating that the models explain only a small fraction of the variance in application processing times. This suggests that the combined effect of centrality measures, gender, and race does not strongly predict how long it takes for applications to be processed.
The interaction of degree centrality with gender and race yields the highest R-squared value (0.0085) among the three models, although it remains quite low. This may hint that the direct connections an examiner has within the network, along with their gender and race, could have a slightly more pronounced effect on processing times than the other centrality measures when considered together.
The model incorporating betweenness centrality with gender and race interaction terms has the lowest explanatory power (R-squared of 0.0046), which implies that an examiner's role as a bridge within the network, when combined with their gender and race, has little impact on the processing time variance.
With closeness centrality, the R-squared value is somewhat higher than betweenness but lower than degree centrality, at 0.0078. This indicates a minor increase in explanatory power when closeness centrality is considered along with gender and race, compared to betweenness.

The combined interactions of centrality measures with demographic factors indicate minimal impact on application processing times. In the context of the USPTO, this suggests that other factors, potentially beyond the examiner's network position or demographics, play a more significant role in influencing processing times. Such factors may include the specific nature of the patent applications, organizational workflows, resources available to the examiners, or the individual expertise and efficiency of the examiners themselves.
Furthermore, the low R-squared values also imply that there's a large amount of variability in processing times that is not captured by these models, pointing to a complex interplay of factors that could be explored in further studies. For instance, understanding how organizational policies, individual workload, or the technical complexity of patent applications affect processing times could provide a more complete picture.


### Refining the regression models by incorporating disposal_type and technology center (tc) variables, considering the outcome of patent applications and the specific areas of technological expertise. These additions aim to capture a more nuanced view of the factors influencing processing times, recognizing the role of content-specific knowledge and the final disposition of applications.


```{r}
# Enhancing regression models to include disposal type and technology center, alongside centrality, gender, and race interactions
results_df_2 <- map_dfr(
  centrality_measures,
  ~ run_regression(node_data,
    paste0(.x, " * gender * race + disposal_type + tc"),
    "app_proc_time",
    plot = FALSE
  )
)

# Summarizing the enhanced models by showcasing the R-squared values, offering insights into their improved explanatory capabilities
results_df_2 %>%
  select(centrality_measure, r_squared) %>%
  distinct()
```

Observations:
Including disposal type and technology center, along with the interaction terms, has increased the R-squared values compared to the models with only centrality measures and demographic interactions. This suggests that these additional variables contribute to explaining the variance in application processing times.
The model including closeness centrality alongside gender, race, disposal type, and technology center shows the highest R-squared value (0.1768940). This indicates that closeness centrality, when considered with these variables, has a relatively more substantial relationship with application processing times.
The degree centrality model also shows an improved R-squared value (0.1348058) compared to the model with centrality and demographic factors alone, reinforcing the idea that the context in which examiners work, represented by the disposal type and technology center, plays a significant role in processing times.
The betweenness centrality model's R-squared value (0.1317740) is the lowest among the three but still shows that adding context variables improves its ability to explain processing time variance.

These models suggest that the centrality of an examiner in the USPTO's network, their demographic background, the type of disposal (issued, abandoned, etc.), and the technology center they work in collectively provide a more robust understanding of processing times. While centrality alone had minimal explanatory power, its influence becomes more pronounced when combined with these additional factors, indicating that the social structure of the workplace, along with the workflow and technical context, shapes productivity and efficiency.

The type of patent application outcome (disposal type) and the specific area of technological expertise (technology center) are influential in the time it takes to process applications. This aligns with the expectation that more complex technologies or the intricate nature of some patent decisions could lengthen processing times. The varied R-squared values across the centrality measures suggest that the role of an examiner in the network is nuanced. While they might be centrally located or act as bridges, the way this affects their work appears to be contextual, shaped by the type of patents they deal with and their specialized knowledge area.


## Visualizing Significant Coefficients from the Best Model

```{r}
# Identifying and visualizing significant coefficients from the best regression model focused on Degree Centrality
best_model <- results_df_2 %>%
  filter(str_starts(centrality_measure, "degree"))

model_coeffs <- ggplot(
  best_model %>% filter(term != "(Intercept)") %>% filter(p.value < 0.05),
  aes(
    x = reorder(term, estimate), # Order terms by estimate for clarity
    y = estimate,
    ymin = estimate - std.error,
    ymax = estimate + std.error
  )
) +
  geom_point(color = "dodgerblue", size = 3) + # More vibrant point color
  geom_errorbar(aes(color = p.value < 0.01), width = 0.2, size = 0.7) + # Color code significance
  scale_color_manual(values = c("TRUE" = "red", "none" = "dodgerblue"), guide = FALSE) + # Highlight highly significant estimates
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", lwd = 1) +
  coord_flip() + # Flip coordinates for horizontal layout
  geom_text(aes(label = sprintf("%.2f", estimate)), # Add estimate values as text labels
            hjust = -0.2, size = 3.5, color = "gray25") +
  labs(
    title = "Significant Coefficients of Degree Centrality Model",
    subtitle = "Application Processing Time Regression Analysis",
    x = "Model Terms",
    y = "Coefficient Estimate",
    caption = "Data Source: USPTO"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    plot.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "#e5e5e5"),
    panel.grid.minor = element_blank()
  )

# Display the plot
model_coeffs

# Save the plot
ggsave("E:/model_coeffs_enhanced.png", model_coeffs, width = 16, height = 9, dpi = 300)

```

Observations:
With a coefficient estimate far to the right and a large error bar, it is significantly different from zero and positively associated with processing time. This implies that applications resulting in issued patents, on average, take longer to process. The lengthy bar indicates some uncertainty around the exact size of the effect but confirms its positive nature.
The coefficients for technology centers, particularly tc2400 and tc1700, show a significant negative association with processing time, meaning these technology centers, on average, process applications more quickly than the baseline technology center. However, tc2100 has a positive coefficient, indicating slower processing times. These findings may reflect differences in the complexity of technologies reviewed in different centers or their operational efficiency.
The point estimate for degree centrality is positive, suggesting a very slight increase in processing time with greater examiner centrality. However, the magnitude is modest and the error bars indicate low precision for this estimate.
The negative coefficient for the interaction between degree centrality and male gender implies that the influence of degree centrality on processing times differs by gender. Specifically, male examiners with higher centrality may process applications more quickly than their female counterparts or the base case, which could be female if the data is coded with female as the reference category.

The error bars, representing the standard errors of estimates, indicate the precision of the coefficients. Larger error bars for disposal_typeISS, for instance, suggest greater uncertainty about the true impact of this factor on processing times.The direction and size of the coefficients provide a nuanced understanding of how various factors contribute to processing times, with positive values indicating an increase in time and negative values a decrease.


### Printing the best model formula here

```{r}
# Defining the regression formula
model_formula <- app_proc_time ~ degree + gender + race + 
  disposal_type + tc + 
  degree:gender + 
  degree:race + 
  gender:race + 
  degree:gender:race

# Printing the formula
cat("Regression model formula:\n")
print(model_formula)
```

### Using Diagrammer to print the predictors assocaited with target variable y (application prcoessing time) in th best model

```{r}
library(DiagrammeR)

# Example using DiagrammeR
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  DiagrammeR::grViz("
digraph model {
  node [shape=box]
  rankdir=LR
  
  // Defining nodes
  DegreeCentrality -> y
  Gender -> y
  Race -> y
  DisposalType -> y
  TechnologyCenter -> y
  'DegreeCentrality:Gender' -> y
  'DegreeCentrality:Race' -> y
  'Gender:Race' -> y
  'DegreeCentrality:Gender:Race' -> y
  
  // Adding labels for interaction terms
  'DegreeCentrality:Gender' [label='Degree*Gender']
  'DegreeCentrality:Race' [label='Degree*Race']
  'Gender:Race' [label='Gender*Race']
  'DegreeCentrality:Gender:Race' [label='Degree*Gender*Race']
}
  ")
}
```


# Q. Does the relationship between centrality and application processing time differ by examiner gender?

The analysis indicates a gender disparity in how centrality impacts processing time. Specifically, the interaction term between degree centrality and male gender was significant, suggesting that the effect of an examiner's centrality within the network varies with gender. For male examiners, increased centrality is associated with a slight decrease in processing times compared to female examiners, suggesting that male examiners may leverage their network position more effectively to process applications. This could be due to various factors, such as differing work styles, collaboration patterns, or even the nature of the applications handled by male versus female examiners.


# Discussion of Findings:

The linear regression models used to evaluate the USPTO patent examiner data have illuminated several key points:

Centrality's Minor Role: Centrality measures alone (degree, betweenness, closeness) have a very minor role in explaining the variance in application processing times, as indicated by low R-squared values.

Importance of Contextual Factors: When adding contextual factors such as the disposal type and the examiner's technological center, the explanatory power of the models increases. This suggests that the specifics of patent applications and the examiner's area of expertise are critical factors influencing processing times.

Gender Differences: The regression models reveal gender differences in the influence of centrality on processing times, with male examiners' centrality being a slightly more significant factor in predicting application processing times than female examiners'.

Technological Centers and Disposal Types: Certain technological centers process applications faster than others, and applications that result in issued patents take longer to process. These findings point to the complexities of patent examination, where different technologies and outcomes require varying amounts of time.


# Implications for the USPTO:

The insights from this analysis suggest several implications for the USPTO:

Resource Allocation: Understanding the role of centrality and its interaction with demographic variables could guide more effective allocation of resources and support to examiners.

Gender Dynamics: Addressing any potential underlying causes of gender disparities in processing times could lead to a more equitable and efficient examination process.

Training and Support: Tailored training and support programs for examiners in technological centers that process applications slower might improve efficiency.

Policy Implications: The relationship between disposal types and processing times could inform policy adjustments to streamline the patent examination process, especially for complex technologies that lead to issued patents.


# Conclusion

In conclusion, the relationship between an examiner's network position and their performance, as measured by application processing times, is nuanced and influenced by multiple factors including gender, technological center, and the type of patent application disposal. While centrality plays a role, its impact is significantly moderated by these contextual factors, suggesting that a multifaceted approach is required to fully understand and enhance efficiency within the USPTO's patent examination process.
