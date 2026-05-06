# Bazaar Case: Branded Keyword Search Experiment
# DiD Analysis of Google vs Bing traffic

library(ggplot2)
library(data.table)
library(tidyverse)

# ---- Load data ----
g <- fread("Google_Weekly_Traffic__Exhibit_2_-_Table_1_.csv")
b <- fread("Bing_Weekly_Traffic__Exhibit_2_-_Table_2_.csv")

treat_week <- 10

# Reshape data to long format for ggplot
df_long1 <- pivot_longer(g, cols = c(Sponsored, Organic),
                         names_to = "Source", values_to = "Traffic")
df_long2 <- pivot_longer(b, cols = c(Sponsored, Organic),
                         names_to = "Source", values_to = "Traffic")

summary(g)
summary(b)

# ---- Plot Google ----
ggplot(df_long1, aes(x = Week, y = Traffic, color = Source)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Sponsored" = "#E64B35", "Organic" = "#4DBBD5")) +
  labs(
    title = "Weekly Traffic from Google (Branded Keyword Searches Only)",
    x = "Week",
    y = "Traffic Volume",
    color = "Click Origin"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )

# ---- Plot Bing ----
ggplot(df_long2, aes(x = Week, y = Traffic, color = Source)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Sponsored" = "#E64B35", "Organic" = "#4DBBD5")) +
  labs(
    title = "Weekly Traffic from Bing (Branded Keyword Searches Only)",
    x = "Week",
    y = "Traffic Volume",
    color = "Click Origin"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )

# ---- Combine data ----
# Control group: Bing, Treated group: Google
# Treatment (change in ad policy) occurs at week 10
df <- bind_rows(
  b %>% mutate(Engine = "Bing"),
  g %>% mutate(Engine = "Google")
) %>%
  mutate(
    Total = Sponsored + Organic,
    Post  = Week >= treat_week
  )

head(df, 6)

# ---- Compute mean total by engine and period ----
pre_means <- df %>%
  filter(Post == FALSE) %>%
  group_by(Engine) %>%
  summarise(Pre = mean(Total), .groups = "drop")

post_means <- df %>%
  filter(Post == TRUE) %>%
  group_by(Engine) %>%
  summarise(Post = mean(Total), .groups = "drop")

# ---- Diff 1: percent change within each engine ----
summary_tbl <- pre_means %>%
  left_join(post_means, by = "Engine") %>%
  mutate(
    Reduction_Diff1 = (Post - Pre) / Pre
  ) %>%
  arrange(Engine)

# ---- Diff 2: Google minus Bing (DiD) ----
bing_red   <- summary_tbl %>% filter(Engine == "Bing")   %>% pull(Reduction_Diff1)
google_red <- summary_tbl %>% filter(Engine == "Google") %>% pull(Reduction_Diff1)
diff2      <- google_red - bing_red

# ---- Summary table ----
summary_pretty <- summary_tbl %>%
  mutate(
    Pre  = round(Pre, 0),
    Post = round(Post, 0),
    `Reduction (Diff 1)` = paste0(sprintf("%.2f", 100 * Reduction_Diff1), "%"),
    `Normalized reduction (Diff 2)` = paste0(sprintf("%.2f", 100 * diff2), "%")
  ) %>%
  select(Engine, Pre, Post, `Reduction (Diff 1)`, `Normalized reduction (Diff 2)`)

print(summary_pretty)

# ---- DiD via pooled OLS (fixest) ----
library(fixest)

df <- df %>%
  mutate(
    Treated = as.numeric(Engine == "Google"),
    Post    = as.numeric(Post)
  )

did_total <- feols(Total ~ Treated * Post, data = df)

did_total_log <- feols(log(Total) ~ Treated * Post, data = df)
summary(did_total)
summary (did_total_log) #level regression is partly picking up the mechanical fact that          
#proportionally similar declines generate much bigger absolute numbers when the baseline is 8x larger. The log spec controls for that and says: the extra      
#proportional decline attributable to the treatment is small and noisy.
