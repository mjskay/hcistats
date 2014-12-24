### analyze the data used in chapter 5
library(coin)		#median_test, wilcox_test

sink("analyses/chapter_5.txt")

#load data tables
monthly = read.csv("data/chapter_5_monthly_data.csv")
annual = read.csv("data/chapter_5_annual_data.csv")


#PART iv
cat("\n\n# PART iv\n")
cat("\n## Test for normality in each group\n") 
d_ply(annual, ~ teamName, function(df) {
        cat("### ", as.character(df$teamName)[1], "\n")
        print(shapiro.test(df$annualIncome))
    })

cat("\n## Median test for difference in median salary between Team A and B\n")
print(median_test(annualIncome ~ teamName, data=annual))

cat("\n## Wilcox test for shift in location of salary between Team A and B\n")
print(wilcox_test(annualIncome ~ teamName, data=annual))

cat("\n## Permutation test for difference in salary between Team A and B\n")
print(oneway_test(annualIncome ~ teamName, data=annual))



sink()