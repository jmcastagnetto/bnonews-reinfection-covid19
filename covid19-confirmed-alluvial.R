library(tidyverse)
library(ggalluvial)

confirmed <- read_csv(
  "data/covid19_confirmed_reinfections.csv",
  na = c("", "-", "NA", "N/A")
) %>%
  mutate(
    age_range = cut(age,
                    breaks = seq(0, 100, by = 10),
                    right = FALSE,
                    ordered_result = TRUE),
    gender = factor(
      replace_na(gender, "(Missing)"),
      levels = c("F", "M", "(Missing)"),
      ordered = FALSE
    ),
    symptoms_1st_case = replace_na(symptoms_1st_case, "(Missing)"),
    symptoms_2nd_case = replace_na(symptoms_2nd_case, "(Missing)"),
    recovered = factor(
       replace_na(recovered, "(Missing)"),
       levels = c("Yes", "No", "(Missing)"),
       ordered = FALSE
    )
  ) %>%
  rename(
    Recovered = recovered
  )

p1 <- ggplot(
  confirmed,
  aes(
    axis1 = age_range,
    axis2 = symptoms_1st_case,
    axis3 = symptoms_2nd_case,
    axis4 = gender,
    fill = Recovered
  )
) +
  scale_x_discrete(
    limits = c("Age Range", "Symptoms of\n1st case", "Symptoms of\n2nd Case", "Sex"),
    expand = c(.2, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  geom_alluvium(aes(fill = Recovered), width = .4) +
  geom_stratum(width = .4) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  labs(
    title = "Characteristics of confirmed COVID-19 reinfections",
    subtitle = "Source: BNO News (https://bit.ly/confirmedcovid19reinfections)",
    caption = "2020-12-15 // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  theme_minimal(18) +
  theme(
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(family = "Inconsolata")
  )
p1
ggsave(
  plot = p1,
  filename = "20201215-characteristics-covid19-reinfections.png",
  width = 16,
  height = 8
)
