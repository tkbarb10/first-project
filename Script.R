library(tidyverse)
library(caret)
library(scales)
library(ROCR)
library(pROC)
library(PRROC)
library(e1071)


# Import Data -------------------------------------------------------------

star_wars <- read.csv("C:/Users/Taylor/OneDrive/Desktop/R Projects/Star Wars/StarWars.csv", 
                        header = F)

#Import without designating a seperator.  Empty rows show up as multiple comma's in the raw data,
#and read.csv will take care of those

dim(star_wars)


# Changing Column Names ---------------------------------------------------

v1_v3 <- as.vector(star_wars[1, 1:3])

v4_v9 <- c('Seen_E1', 'Seen_E2', 'Seen_E3', 'Seen_E4', 'Seen_E5', 'Seen_E6')

v10_v15 <- c('E1_Rank', 'E2_Rank', 'E3_Rank', 'E4_Rank', 'E5_Rank', 'E6_Rank')

v16_V29 <- as.vector(star_wars[2, 16:29])

V30_V38 <- as.vector(star_wars[1, 30:38])

colnames(star_wars) <- c(v1_v3, v4_v9, v10_v15, v16_V29, V30_V38)

View(star_wars)

#Error in column 32 for unrecognized characters so changing that

colnames(star_wars)[32] <- "Do you consider yourself to be a fan of the Expanded Universe?"


# Delete First 2 Rows -----------------------------------------------------

star_wars_prac <- star_wars

star_wars_prac <- star_wars_prac[3:555, ]

View(star_wars_prac)

star_wars <- star_wars[-c(1, 2), ]


# Removing all empty rows -------------------------------------------------


subset <- subset.data.frame(star_wars[, 3:38])

 ' -Get the indices of rows where all columns are empty strings'
 
indexes <- which(apply(subset, 1, function(row) all(row == "")))


star_wars <- star_wars[-indexes, ]

# Changing Movie Name Columns to Yes or No --------------------------------

star_wars[4:9] <- lapply(star_wars[4:9], function(x) ifelse(grepl("Star Wars", x), 'Yes', 'No'))


# If Column 2 is No, changing all row values to N/A -----------------------

       
#indicies <- which(star_wars[, 2] == 'No')

 '- Mistake since some of those rows contain relevant demographic data'


# Changing 'Neither Favorably..." to "Neutral" ----------------------------

star_wars[16:29] <- lapply(star_wars[16:29], function (x) ifelse(x == "Neither favorably nor unfavorably (neutral)", "Neutral", x))



# Change DF to factor and then fill in all blanks as NA ----------------------------------

' - Change to NA first, then change to factor, otherwise the empty cells will become a level'

star_wars <- star_wars |> 
  mutate(
    across(c(3:38), ~ if_else(. == "", NA, .))
    )|> 
  mutate_at(
    vars(2:38), as.factor
  )




# Favorite character ------------------------------------------------------

' - pull out the "Very favorable" level and plot the characters against each other'

favorable <- sapply(characters, function(x) sum(x == 'Very favorably'))

counts_df <- data.frame(Variable = names(favorable), Count = favorable)

ggplot(counts_df, aes(x = Count, y = reorder(Variable, Count), fill = Variable)) + 
  geom_col(orientation = 'y', just = 0) +
  geom_text(aes(label = Count), vjust = -.6, hjust = 0) +
  labs(title = "Who's the favorite character?",
       x = 'Number of very favorable votes',
       y = 'Character') +
  scale_x_continuous(breaks = seq(100, 600, 100)) +
  theme_minimal() +
  theme(axis.text.y = element_text(vjust = 0))



# Least Favorite ----------------------------------------------------------

unfavorable <- sapply(characters, function(x) sum(x == 'Very unfavorably'))

uncounts_df <- data.frame(Variable = names(unfavorable), Count = unfavorable)


ggplot(uncounts_df, aes(x = Count, y = reorder(Variable, Count), fill = Variable)) + 
  geom_col(orientation = 'y', just = 0) +
  geom_text(aes(label = Count), vjust = -.6, hjust = 0) +
  labs(title = "Who's the least favorite character?",
       x = 'Number of very unfavorable votes',
       y = 'Character') +
  scale_fill_viridis_d(option = 'inferno') +
  scale_x_continuous(breaks = seq(0, 200, 25)) +
  theme_minimal() +
  theme(axis.text.y = element_text(vjust = 0))


# Visualize Columns and make comparisons ----------------------------------


' - Directly filtering NA values since they arent recognized automatically by ggplot'

ggplot(star_wars |> filter(!is.na(`Household Income`)), aes(`Have you seen any of the 6 films in the Star Wars franchise?`, fill = `Household Income`)) +
  geom_bar() +
  theme_minimal()

income <- table(star_wars$`Household Income`)
prop.table(income)

movie <- table(star_wars$`Household Income`, star_wars$`Have you seen any of the 6 films in the Star Wars franchise?`)
prop.table(movie, margin = 2)

ggplot(star_wars, aes(x = `Luke Skywalker`, fill = characters)) +
  geom_bar() +
  theme_minimal()




# Filter groups of data with no NA ----------------------------------------

sw_no_na <- star_wars |> 
  filter(if_all(everything(), ~ !is.na(.)))

characters1 <- star_wars |> 
  select(16:29) |> 
  filter(if_all(everything(), ~ !is.na(.)))


demographics <- star_wars |> 
  select(34:38) |> 
  filter(if_all(everything(), ~ !is.na(.)))

View(star_wars)

ggplot(characters, aes(x = `Han Solo`)) +
  geom_bar(fill = 'forestgreen') +
  scale_x_discrete(labels = label_wrap(5)) +
  theme_minimal()



# Average Rank of Each Movie ----------------------------------------------

star_wars |> 
  select(E1_Rank:E6_Rank) |> 
  filter(if_all(everything(), ~ !is.na(.))) |> 
  reframe(across(everything(), as.numeric)) |> 
  summarize(across(everything(), mean))



# Average Rank by Region --------------------------------------------------

star_wars |> 
  select(E1_Rank:E6_Rank, `Location (Census Region)`) |> 
  group_by(`Location (Census Region)`) |> 
  filter(if_all(everything(), ~ !is.na(.))) |> 
  mutate(across(E1_Rank:E6_Rank, as.numeric)) |> 
  summarise(across(E1_Rank:E6_Rank, mean))



# Average Rank by Gender --------------------------------------------------

' - Put grouping after filter to ensure NA values were removed from all columns'

star_wars |> 
  select(E1_Rank:E6_Rank, Gender) |> 
  filter(if_all(everything(), ~ !is.na(.))) |> 
  group_by(Gender) |> 
  mutate(across(E1_Rank:E6_Rank, as.numeric)) |> 
  summarise(across(E1_Rank:E6_Rank, mean))



# Average ranking of each character ---------------------------------------

characters <- apply(characters, MARGIN = 2, 
                    function(.) ifelse(. == 'Very favorably', 5, 
                                       ifelse(. == 'Somewhat favorably', 4,
                                              ifelse(. == 'Neutral', 3,
                                                     ifelse(. == 'Somewhat unfavorably', 2,
                                                            ifelse(. == 'Very unfavorably', 1, 0))))))

characters <- as.data.frame(characters)

characters |> 
  summarise(across(everything(), mean))



# Average Ranking with Unfamiliar values removed --------------------------

characters1 <- star_wars |> 
  select(16:29)


mapping <- c(
  'Very favorably' = 5,
  'Somewhat favorably' = 4,
  'Neutral' = 3,
  'Somewhat unfavorably' = 2,
  'Very unfavorably' = 1
)            

characters_no_zero <- characters1 |> 
  mutate(across(everything(), ~ recode(., !!!mapping, .default = 0))) |>
  filter(if_all(everything(), ~ . != 0))
  #The !!! is to ensure the mapping vector is spliced and each element considered individually

characters_no_zero |>
  summarise(across(everything(), mean)) |> 
  pivot_longer(
    cols = everything(),
    names_to = 'Names',
    values_to = 'Average Ranking'
  )



# Logit model using just episode rankings ---------------------------------


episodes <- star_wars |> 
  select(E1_Rank:E6_Rank, Gender) |> 
  mutate_at(vars(1:6), as.numeric)


episodes <- na.omit(episodes)

model <- glm(Gender ~ ., data = episodes, family = binomial(link = 'logit'))

epi_pred <- predict(model, type = 'response')

roc_curve <- roc(episodes$Gender, epi_pred)
plot(roc_curve)

roc_curve

actual_labels <- as.numeric(episodes$Gender)

actual_labels <- ifelse(actual_labels == 2, 1, 0)

pr_curve <- pr.curve(scores.class0 = epi_pred, weights.class0 = actual_labels, curve = T)

plot(pr_curve)

pr_curve
