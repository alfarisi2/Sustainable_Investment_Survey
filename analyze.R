library(readxl)
library(tidyverse)
library(skimr)
library(RColorBrewer)
library(gtsummary)
library(gt)
library(patchwork)
library(likert)
library(xtable)
library(kableExtra)
setwd("D:/dataset/mendeley/inveschar/pm6ks8k59x-1/pm6ks8k59x-1")

rawdata = read_xls("Sustainable Data.xls")
glimpse(rawdata)
skim(rawdata)

cat_data <- rawdata |> 
  mutate_if(is.numeric, as.factor)

glimpse(cat_data)                          

cat_data$Area
cat_data$SIMN1

cat_data <- 
  cat_data |> 
  mutate(Area = fct_recode(Area, 
                           "Rural" = "1",
                           "Urban" = "2"),
        Gender = fct_recode(Gender,
                             "Male" = "1",
                             "Female" = "2"),
         Age = fct_recode(Age,
                          "< 20" = "1",
                          "21-29" = "2",
                          "30-39" = "3",
                          "40-49" = "4",
                          "50 >" = "5"),
         Educationqualification = fct_recode(Educationqualification,
                                             "School Level" = "1",
                                             "Diploma/ITI" = "2",
                                             "Undergraduate" = "3",
                                             "Postgraduate" = "4",
                                             "Professional Degree" = "5"),
         Occupation = fct_recode(Occupation,
                                 "Business" = "1",
                                 "Government Employee" = "2",
                                 "Private Employee" = "3",
                                 "Farmer" = "4",
                                 "Retired" = "5"),
         MonthlyIncome = fct_recode(MonthlyIncome,
                                    "Up to 10,000" = "1",
                                    "10,001-20,000" = "2",
                                    "20,001-30,000" = "3",
                                    "30,001-40,000" = "4",
                                    "40,001-50,000" = "5",
                                    "Above 50,000" = "6"),
         HouseholdIncome = fct_recode(HouseholdIncome,
                                      "Up to 10,000" = "1",
                                      "10,001-20,000" = "2",
                                      "20,001-30,000" = "3",
                                      "30,001-40,000" = "4",
                                      "40,001-50,000" = "5",
                                      "Above 50,000" = "6"),
         Natureoffamily = fct_recode(Natureoffamily,
                                     "Nuclear" = "1",
                                     "Joint" = "2"),
         Monthlysavingpercentage = fct_recode(MonthlyIncome,
                                              "1%-5%" = "1",
                                              "6%-10%" = "2",
                                              "11%-15%" = "3",
                                              "16%-20%" = "4",
                                              "More than 20%" = "5"))


fct_unique(cat_data$Educationqualification)

gender <- cat_data |> group_by(Gender) |> 
  summarise(Participants = n()) |> 
  mutate(Percentage = round((Participants / sum(Participants)) * 100, 2))

gender |>
  kbl(booktabs = TRUE, caption = "Participants by Gender") |> 
  kable_styling(full_width = FALSE, 
                bootstrap_options = c("striped", "bordered", "hover"), 
                font_size = 14) |>
  row_spec(0, bold = TRUE)
  
age <- cat_data |> group_by(Age) |> 
  summarise(N = n()) |> 
  mutate(Percentage = round((N / sum(N)) * 100, 2))

age |>
  kbl(booktabs = TRUE, caption = "Participants by Age") |> 
  kable_styling(full_width = FALSE, 
                bootstrap_options = c("striped", "bordered", "hover"), 
                font_size = 14) |>
  row_spec(0, bold = TRUE)

edu <- 
  cat_data |> group_by(Educationqualification) |> 
  summarise(N = n()) |> 
  mutate(Percentage = round((N / sum(N)) * 100, 2))

edu |>
  kbl(booktabs = TRUE, caption = "Participants by Education") |> 
  kable_styling(full_width = FALSE, 
                bootstrap_options = c("striped", "bordered", "hover"), 
                font_size = 14) |>
  row_spec(0, bold = TRUE)

occup  <- 
  cat_data |> group_by(Occupation) |> 
  summarise(N = n()) |> 
  mutate(Percentage = round((N / sum(N)) * 100, 2))

occup |>
  kbl(booktabs = TRUE, caption = "Participants by Occupation") |> 
  kable_styling(full_width = FALSE, 
                bootstrap_options = c("striped", "bordered", "hover"), 
                font_size = 14) |>
  row_spec(0, bold = TRUE)

HouseIn <- 
  cat_data |> group_by(HouseholdIncome) |> 
  summarise(N = n()) |> 
  mutate(Percentage = round((N / sum(N)) * 100, 2))

HouseIn |>
  kbl(booktabs = TRUE, caption = "Participants by Household Income") |> 
  kable_styling(full_width = FALSE, 
                bootstrap_options = c("striped", "bordered", "hover"), 
                font_size = 14) |>
  row_spec(0, bold = TRUE)

Monthlysave <- 
  cat_data |> group_by(Monthlysavingpercentage) |> 
  summarise(N = n()) |> 
  mutate(Percentage = round((N / sum(N)) * 100, 2))

Monthlysave |>
  kbl(booktabs = TRUE, caption = "Participants by Monthly Saving") |> 
  kable_styling(full_width = FALSE, 
                bootstrap_options = c("striped", "bordered", "hover"), 
                font_size = 14) |>
  row_spec(0, bold = TRUE)


cat_data |> 
  select(Gender, Age, Area) |> 
  tbl_summary() |> 
  bold_labels() 

partic_education <- 
  cat_data|> 
  ggplot(aes(x = Educationqualification,
             fill = Educationqualification)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2")+
  ggtitle("Participants by Education") +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL) +
  coord_flip()

partic_occupation <- 
  cat_data |> 
  ggplot(aes(x = Occupation, fill = Occupation)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3")+
  ggtitle("Participants by Occupation") +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL) +
  coord_flip()

partic_age <- 
  cat_data |> 
  ggplot(aes(x = Age, fill = Age)) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Participants by Age") +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none")

partic_gender <- 
  cat_data |> 
  ggplot(aes(x = Gender, fill = Gender)) +
  geom_bar() +
  scale_fill_brewer(palette = "Accent")+
  ggtitle("Participants by Gender") +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none")

partic_area <- 
  cat_data |> 
  ggplot(aes(x = Area, fill = Area)) +
  geom_bar() +
  scale_fill_brewer(palette = "Pastel1")+
  ggtitle("Participants by Area") +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none")

partic_houseincome <- 
  cat_data |> 
  ggplot(aes(x = HouseholdIncome, fill = HouseholdIncome)) +
  geom_bar() +
  scale_fill_brewer(palette = "BrBG")+
  ggtitle("Participants by Household Income") +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL) +
  coord_flip()

partic_saving <- 
  cat_data |> 
  ggplot(aes(x = Monthlysavingpercentage, fill = Monthlysavingpercentage)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1")+
  ggtitle("Participants by Monthy Saving Percentage") + 
  theme(legend.position = "none") + 
  labs(x = NULL, y = NULL) + 
  coord_flip()


print(partic_age)
print(partic_gender)
print(partic_area)
print(partic_education)
print(partic_houseincome)
print(partic_saving)
print(partic_occupation)



(partic_gender | partic_area) / partic_age

partic_education +  partic_occupation + partic_houseincome + partic_saving + plot_layout(ncol = 2)

display.brewer.all()


levels(cat_data$SIDMB1)

likert_labels <- c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")

likert_columns <- 10:ncol(cat_data)

for (col in likert_columns) {
  levels(cat_data[[col]]) <- likert_labels
}



questions <- cat_data[, 10:39]


allquest <- tbl_summary(
  data = questions,
  label = list(SIDMB1 ~ "1. I consider social responsibility aspects, whenever I am choosing an investment", 
               SIDMB2 ~ "2. Investing socially responsible is something that I have done", 
               SIDMB3 ~ "3. I prefer investing in funds/shares that comply with sustainable development, which are consistent with ethics, social, economic development",
               SIDMB4 ~ "4. I am always satisfied with invest in formal investment as well as sustainable investment",
               SIDMB5 ~ "5. I believe that my investment does have a positive impact on the economic development",
               SIInt1 ~ "6. I am happy to invest in socially responsible investments",
               SIInt2 ~ "7. Sustainable investment develop my family status",
               SIInt3 ~ "8. It will help my child’s future",
               SIInt4 ~ "9. It is responsible and sustainable and hence I invest",
               SIInt5 ~ "10. It will help my retirement",
               SIAtt1 ~ "11. Investments are good for the economy",
               SIAtt2 ~ "12. Sustainable investment would allow me to gain, without unnecessarily harming the society or economy",
               SIAtt3 ~ "13. It gives more benefit to our country as well as my family",
               SIAtt4 ~ "14. Financial inclusion schemes give opportunity to access all the financial services",
               SIAtt5 ~ "15. I promote economic development through my investment decisions",
               SISN1 ~ "16. Family members and friends motivate me to invest in sustainable investments",
               SISN2 ~ "17. Financial experts and agents of financial institutions provide the required information about traditional investment and sustainable investments",
               SISN3 ~ "18. Financial literacy programmes of various investment avenues provide sufficient knowledge about particular investment",
               SISN4 ~ "19. Media like, news paper, magazine, TV, ratio and mobile provided the investment knowledge",
               SISN5 ~ "20. Co-workers encouraged me to invest in sustainable investments ",
               SIPBC1 ~ "21. I believe I have the ability to invest in socially responsible investment schemes",
               SIPBC2 ~ "22. I have plenty of opportunity to invest in socially responsible investment schemes",
               SIPBC3 ~ "23. All the financial inclusion schemes will help to improve my financial status",
               SIPBC4 ~ "24. If it were entirely up to me, I am confident that I will invest in any investment",
               SIPBC5 ~ "25. I have resources, time and willingness to invest in various investments",
               SIMN1 ~ "26. I have some personal principles towards investing my money",
               SIMN2 ~ "27. My investments are responsible to all",
               SIMN3 ~ "28. All the investments are created by the rules and regulations of particular authorities and hence I invest",
               SIMN4 ~ "29. There are no speculations in the financial inclusion schemes",
               SIMN5 ~ "30. Financial inclusion schemes economically help to develop our country ")
)

print(allquest)




allquest <- allquest |> 
  modify_table_styling(
    columns = label,
    rows = !is.na(label),
    text_format = "bold"
  )


allquest |> as_gt()

Q1 = cat_data[, "SIDMB1", drop = FALSE]

Q1table <-  
  tbl_summary(data = Q1 ,
              label = list(SIDMB1 ~ "1. I consider social responsibility aspects, whenever I am choosing an investment"))

print(Q1table)








questions <- as.data.frame(questions)
likertdata <- likert(questions)

plot(likert(questions)) +
  scale_fill_manual(values = brewer.pal(n=5, "Dark2"))

cat_data2 <- as.data.frame(cat_data)

plot(likert(cat_data2[, c(10:14)], grouping = cat_data2[, 1]), legend.position = "right")

cat_data3 <- cat_data2 |> 
  rename("1. I consider social responsibility aspects, whenever I am choosing an investment" = SIDMB1, 
         "2. Investing socially responsible is something that I have done" = SIDMB2, 
         "3. I prefer investing in funds/shares that comply with sustainable development, which are consistent with ethics, social, economic development" = SIDMB3,
         "4. I am always satisfied with invest in formal investment as well as sustainable investment" = SIDMB4,
         "5. I believe that my investment does have a positive impact on the economic development" = SIDMB5,
         "6. I am happy to invest in socially responsible investments" = SIInt1,
         "7. Sustainable investment develop my family status" = SIInt2,
         "8. It will help my child’s future" = SIInt3,
         "9. It is responsible and sustainable and hence I invest" = SIInt4,
         "10. It will help my retirement" = SIInt5,
         "11. Investments are good for the economy" = SIAtt1,
         "12. Sustainable investment would allow me to gain, without unnecessarily harming the society or economy" = SIAtt2,
         "13. It gives more benefit to our country as well as my family" = SIAtt3,
         "14. Financial inclusion schemes give opportunity to access all the financial services" = SIAtt4,
         "15. I promote economic development through my investment decisions" = SIAtt5,
         "16. Family members and friends motivate me to invest in sustainable investments" = SISN1,
         "17. Financial experts and agents of financial institutions provide the required information about traditional investment and sustainable investments" = SISN2,
         "18. Financial literacy programmes of various investment avenues provide sufficient knowledge about particular investment" = SISN3,
         "19. Media like, news paper, magazine, TV, ratio and mobile provided the investment knowledge" = SISN4,
         "20. Co-workers encouraged me to invest in sustainable investments " = SISN5,
         "21. I believe I have the ability to invest in socially responsible investment schemes" = SIPBC1,
         "22. I have plenty of opportunity to invest in socially responsible investment schemes" = SIPBC2,
         "23. All the financial inclusion schemes will help to improve my financial status" = SIPBC3,
         "24. If it were entirely up to me, I am confident that I will invest in any investment" = SIPBC4,
         "25. I have resources, time and willingness to invest in various investments" = SIPBC5,
         "26. I have some personal principles towards investing my money" = SIMN1,
         "27. My investments are responsible to all" = SIMN2,
         "28. All the investments are created by the rules and regulations of particular authorities and hence I invest" = SIMN3,
         "29. There are no speculations in the financial inclusion schemes" = SIMN4,
         "30. Financial inclusion schemes economically help to develop our country " = SIMN5)

plot(likert(cat_data3[, c(10:14)],
            grouping = cat_data3[, 1]),
     centered = FALSE,
     plot.percent.low = FALSE, plot.percent.high = FALSE, plot.percent.neutral = FALSE,
     legend.position = "right") +
     scale_fill_manual(values = brewer.pal(n=5, "Accent")) +
     guides(fill = guide_legend(title = "Responses")) +
    theme(
    legend.position = "right",
    axis.text = element_text(face = "bold"),      # Bold axis labels
    axis.text.x = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),    # Bold axis titles
    plot.title = element_text(face = "bold"),    # Bold plot title
    legend.text = element_text(face = "bold")    # Bold legend text
    )

occu_answer <- 
  plot(likert(cat_data3[, c(10:14)],
            grouping = cat_data3[, 5]),
     centered = FALSE,
     plot.percent.low = FALSE, plot.percent.high = FALSE, plot.percent.neutral = FALSE,
     legend.position = "right") +
  scale_fill_manual(values = brewer.pal(n=5, "Paired")) +
  guides(fill = guide_legend(title = "Responses")) +
  ggtitle("Indicate your opinion regarding your investment behaviour towards sustainable investment") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(occu_answer)

occu_answer1 <- 
  plot(likert(cat_data3[, c(15:19)],
              grouping = cat_data3[, 5]),
       centered = FALSE,
       plot.percent.low = FALSE, plot.percent.high = FALSE, plot.percent.neutral = FALSE,
       legend.position = "right") +
  scale_fill_manual(values = brewer.pal(n=5, "Paired")) +
  guides(fill = guide_legend(title = "Responses")) +
  ggtitle("Rate your opinion regarding your intention towards sustainable investment") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(occu_answer1)

occu_answer2 <- 
  plot(likert(cat_data3[, c(20:24)],
              grouping = cat_data3[, 5]),
       centered = FALSE,
       plot.percent.low = FALSE, plot.percent.high = FALSE, plot.percent.neutral = FALSE,
       legend.position = "right") +
  scale_fill_manual(values = brewer.pal(n=5, "Paired")) +
  guides(fill = guide_legend(title = "Responses")) +
  ggtitle("State your opinion regarding your attitude towards sustainable investment") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(occu_answer2)

occu_answer3 <- 
  plot(likert(cat_data3[, c(25:29)],
              grouping = cat_data3[, 5]),
       centered = FALSE,
       plot.percent.low = FALSE, plot.percent.high = FALSE, plot.percent.neutral = FALSE,
       legend.position = "right") +
  scale_fill_manual(values = brewer.pal(n=5, "Paired")) +
  guides(fill = guide_legend(title = "Responses")) +
  ggtitle("Indicate your opinion regarding your subjective norms towards sustainable investment") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(occu_answer3)

occu_answer4 <- 
  plot(likert(cat_data3[, c(30:34)],
              grouping = cat_data3[, 5]),
       centered = FALSE,
       plot.percent.low = FALSE, plot.percent.high = FALSE, plot.percent.neutral = FALSE,
       legend.position = "right") +
  scale_fill_manual(values = brewer.pal(n=5, "Paired")) +
  guides(fill = guide_legend(title = "Responses")) +
  ggtitle("Rate your opinion regarding your perceived behavioural control towards sustainable investment") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(occu_answer4)

occu_answer5 <- 
  plot(likert(cat_data3[, c(35:39)],
              grouping = cat_data3[, 5]),
       centered = FALSE,
       plot.percent.low = FALSE, plot.percent.high = FALSE, plot.percent.neutral = FALSE,
       legend.position = "right") +
  scale_fill_manual(values = brewer.pal(n=5, "Paired")) +
  guides(fill = guide_legend(title = "Responses")) +
  ggtitle("Indicate your opinion regarding your moral norms towards sustainable investment") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(occu_answer5)

