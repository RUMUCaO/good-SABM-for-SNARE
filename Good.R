# Full pipeline example: Data preparation + Creating Siena data + Running model + Extracting results

library(RSiena)

# --- 1. Data Preparation ---
# Suppose you have four sets of data, each containing friendship networks and behavior variables
# Here are example data placeholders, replace with your real data
# Friendship network data: 3D array, nodes × nodes × time points
# Behavior variables: matrix or vector, time points × nodes

# For example:
# friendshipSchool1Cohort1 <- array(dim = c(n, n, 3))
# selfControlSchool1Cohort1 <- matrix(nrow = n, ncol = 3)
# externalizingBehaviorSchool1Cohort1 <- matrix(nrow = n, ncol = 3)

# Replace with your actual data:
# Example assuming 20 nodes
load("friend.RData")
ls()

set.seed(123)
n <- 20
timePoints <- 3


# --- 2. Create Siena Data Object ---
createSienaData <- function(friendship, selfControl, externalizing) {
  friendshipArray <- sienaDependent(friendship)
  selfControlVar <- coCovar(rowMeans(selfControl)) # Use mean values as example
  externalizingVar <- coCovar(rowMeans(externalizing))
  
  # You can also use behaviorDependent() to create dynamic behavior variables
  # selfControlVar <- behaviorDependent(selfControl)
  # externalizingVar <- behaviorDependent(externalizing)
  
  sienaDataCreate(friendshipArray, selfControlVar, externalizingVar)
}

sienaDataSchool1Cohort1 <- createSienaData(friendshipSchool1Cohort1, selfControlSchool1Cohort1, externalizingSchool1Cohort1)
sienaDataSchool1Cohort2 <- createSienaData(friendshipSchool1Cohort2, selfControlSchool1Cohort2, externalizingSchool1Cohort2)
sienaDataSchool2Cohort1 <- createSienaData(friendshipSchool2Cohort1, selfControlSchool2Cohort1, externalizingSchool2Cohort1)
sienaDataSchool2Cohort2 <- createSienaData(friendshipSchool2Cohort2, selfControlSchool2Cohort2, externalizingSchool2Cohort2)


# --- 3. Define Effects for the Model ---
createEffectsModel <- function(sienaData) {
  eff <- getEffects(sienaData)
  
  # 1. Basic network structure
  eff <- includeEffects(eff, density, recip)
  
  # 2. Node degree correlation
  eff <- includeEffects(eff, outdegree, indegree)
  # If desired, you can also enable the following:
  eff <- includeEffects(eff, inPopSqrt)
  eff <- includeEffects(eff, outPopSqrt)
  eff <- includeEffects(eff, inActSqrt)
  eff <- includeEffects(eff, outActSqrt)
  
  # 3. Triangle structure
  eff <- includeEffects(eff, transTrip, cycle3)
  # Add transitive and reciprocal triplets if necessary
   eff <- includeEffects(eff, transRecTrip)
   
   eff <- includeTimeHeterogeneity(eff, effects = c(density, indegreePopularity))
   
   # 4. Effects of behavioral variables (if any)
   # eff <- includeEffects(eff, linear, quad, behaviorChange, name = "externalizing")
   # eff <- includeEffects(eff, avAlt, name = "externalizing")
    eff <- includeEffects(eff, egoX, altX, simX, interaction1 = "selfControl")
   
   #simplify the effect model for the whole cohort
   #effectsDocumentation(eff)
   print(eff)  # 
   return(eff)
}

effSchool1Cohort1 <- createEffectsModel(sienaDataSchool1Cohort1)
effSchool1Cohort2 <- createEffectsModel(sienaDataSchool1Cohort2)
effSchool2Cohort1 <- createEffectsModel(sienaDataSchool2Cohort1)
effSchool2Cohort2 <- createEffectsModel(sienaDataSchool2Cohort2)



# --- 4. Run the Models ---
alg <- sienaAlgorithmCreate(projname = "FriendshipModel", useStdInits = FALSE, n3 = 5000)

ansSchool1Cohort1 <- siena07(alg, data = sienaDataSchool1Cohort1, effects = effSchool1Cohort1, batch = TRUE)
ansSchool1Cohort2 <- siena07(alg, data = sienaDataSchool1Cohort2, effects = effSchool1Cohort2, batch = TRUE)
ansSchool2Cohort1 <- siena07(alg, data = sienaDataSchool2Cohort1, effects = effSchool2Cohort1, batch = TRUE)
ansSchool2Cohort2 <- siena07(alg, data = sienaDataSchool2Cohort2, effects = effSchool2Cohort2, batch = TRUE)

summary(ansSchool1Cohort1)$tconv.max  # Maximum convergence value
summary(ansSchool1Cohort1)$tconv.max > 0.25  # Whether the convergence threshold is exceeded

print(summary(ansSchool1Cohort1)$effects)
s<-ansSchool1Cohort1
print(s$theta)
print(s$se)
print(s$tconv)
print(rownames(s$covparms))



# --- 5. Extract Results ---
extractResultsFixed <- function(ans) {
  s <- summary(ans)
  effects <- s$effects
  
  data.frame(
    Effect = effects$effectName,
    Estimate = round(s$theta, 3),
    SE = round(s$se, 3),
    tRatio = round(s$tconv, 3)
  )
}

res1 <- extractResultsFixed(ansSchool1Cohort1)
res2 <- extractResultsFixed(ansSchool1Cohort2)
res3 <- extractResultsFixed(ansSchool2Cohort1)
res4 <- extractResultsFixed(ansSchool2Cohort2)

# --- 6. Combine Results to Create Table 2 Format ---
library(dplyr)
if (!exists("res4") || is.null(res4)) {
  res4 <- data.frame(Effect = character(0),
                     Estimate = numeric(0),
                     SE = numeric(0),
                     tRatio = numeric(0),
                     Display = character(0))
}


table2 <- full_join(res1, res2, by = "Effect", suffix = c("_S1C1", "_S1C2")) %>%
  full_join(res3, by = "Effect") %>%
  full_join(res4, by = "Effect", suffix = c("_S2C1", "_S2C2"))

# Select and rename display columns
print(table2)


# Optionally save the table as CSV
write.csv(table2, "Table2_Estimates.csv", row.names = FALSE)


library(tidyverse)


df <- tribble(
  ~Effect, ~Estimate_S1C1, ~SE_S1C1, ~Estimate_S1C2, ~SE_S1C2,
  "outdegree (density)", -2.251, 0.292, -1.843, 0.173,
  "reciprocity", -3.484, 6.549, -0.177, 0.788,
  "transitive triplets", 0.646, 1.307, -0.153, 0.968,
  "3-cycles", 0.854, 0.786, 0.917, 0.425
)


df_long <- df %>%
  pivot_longer(cols = starts_with("Estimate"), names_to = "Group", values_to = "Estimate") %>%
  mutate(SE = case_when(
    Group == "Estimate_S1C1" ~ SE_S1C1,
    Group == "Estimate_S1C2" ~ SE_S1C2
  )) %>%
  mutate(Cohort = recode(Group,
                         "Estimate_S1C1" = "School 1 - Cohort 1",
                         "Estimate_S1C2" = "School 1 - Cohort 2"))

library(ggplot2)

ggplot(df_long, aes(x = Effect, y = Estimate, color = Cohort)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Effect Estimates for School 1 - Cohort 1 and 2",
    x = "Effect",
    y = "Estimate (with SE)",
    color = "Cohort"
  )


