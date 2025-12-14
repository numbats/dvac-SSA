library(tidyverse)

## Example 1

## Before
ggplot(data = mtcars) +
  geom_point(aes(x = wt, y = mpg))

## After 
ggplot(data = mtcars) +
  geom_point(aes(x = wt, y = mpg)) + 
geom_label(x = 4, y = 30, 
          label = "As the car weight increases \n the fuel economy gets worse", 
          color = "darkred", 
          size = 5) + 
  theme_bw() + 
  ylab("Miles Per Gallon") + 
  xlab("Weight (by 1000 lbs)")

## Another after 
most_efficient = mtcars |>
  arrange(desc(mpg)) |>
  slice(1)

ggplot(data = mtcars) +
  annotate(geom = "point", x = most_efficient$wt, y = most_efficient$mpg, 
           colour = "orange", size = 3) + 
  annotate(geom = "label",  x = most_efficient$wt + 0.1, y = most_efficient$mpg, 
           label = paste("Most Efficient:", rownames(most_efficient)), 
           hjust = "left", colour = "orange") +
  geom_point(aes(x = wt, y = mpg)) + 
  theme_bw() + 
  ylab("Miles Per Gallon") + 
  xlab("Weight (by 1000 lbs)") + 
  ggtitle("As the car weight increases the fuel economy gets worse") 

## =============================================================================

library(gghighlight)

big_tech_data = read_csv("week10/data/big-tech-stock-price.csv")

## Mess 
ggplot(big_tech_data) +
    geom_line(aes(x= date, y = high, colour = stock_symbol))

## Highlight top stocks 
p <- ggplot(big_tech_data) +
  geom_line(aes(x= date, y = high, colour = stock_symbol)) + 
  gghighlight(max(high) > 600)
p
  
## Facet warp
p + theme_minimal() + facet_wrap(~ stock_symbol)

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_point() + 
  gghighlight::gghighlight() + 
  facet_wrap(vars(cyl))

## =============================================================================

## Example 1 

## Before: 
ggplot(economics, aes(date, unemploy)) + 
  geom_line()

presidential_processed <- presidential |>
  filter(start > economics$date[1])

## After 
min_date = min(economics$date)
ggplot(economics) + 
  geom_rect(
    aes(xmin = start, xmax = end, fill = party), 
    ymin = -Inf, ymax = Inf, alpha = 0.2, 
    data = presidential
  ) + 
  geom_vline(
    aes(xintercept = as.numeric(start)), 
    data = presidential,
    colour = "gray50", alpha = 0.5, linetype = "dashed"
  ) + 
  geom_text(
    aes(x = start, y = 2500, label = name), 
    data = presidential, 
    size = 2.5, vjust = 0, hjust = 0, nudge_x = 50
  ) + 
  geom_line(aes(date, unemploy)) + 
  scale_fill_manual(values = c("blue", "red")) +
  xlab("Date") + 
  ylab("Unemployed (in 1000s)") + 
  ggtitle("How does unemployment vary by Presidency?") + 
  theme_bw() + 
  theme(
    legend.position = c(0.2, 0.9),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = "white",
      colour = "grey80",
      linewidth = 0.5
    ),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.6, "lines"),
    legend.spacing.x = unit(0.3, "lines")
  )

## AI Prompts 
# https://chatgpt.com/share/693e5967-8574-8011-b36a-bc9920ffb1b3