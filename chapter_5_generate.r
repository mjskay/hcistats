### generate the data used in chapter 5. might also be useful for others
source("generate.R")
source("chapter_5_functions.R")

#put all monthly data in this table
monthly = email



#PART IV
# add monthly number of sales to the email response time data. This is 
# inversely proportional to response time, with some added noise
monthly$monthlySales = floor(10 * pmax(monthly$responseTime + rnorm(nrow(monthly)), .05)^-0.5)

# generate salary based on base salary plus some commission from above sales
averageOrderValue = 5000
commission = .2
baseSalary = 30000 / 12 #base monthly salary
monthly$monthlyIncome = baseSalary + monthly$monthlySales * averageOrderValue * commission

#annual salary will be the same as the sum of each person's monthly salaries
#over the 4 month period times 12/4
annual = ddply(monthly, ~ uid + team + teamName, summarize, annualIncome=sum(monthlyIncome) * 12/4)



#SAVE DATA TABLES
write.csv(monthly, "data/chapter_5_monthly_data.csv")
write.csv(annual, "data/chapter_5_annual_data.csv")



#REFERENCE PLOTS
pdf("plots/chapter_5.pdf")
tryCatch({
    # monthly sales versus email response time
    print(ggplot(monthly, aes(x=responseTime, y=monthlySales, color=teamName)) + geom_point() + geom_hline(yintercept=0)) 
    
    # monthly income of each team
    print(ggplot(monthly, aes(x=monthlyIncome)) + geom_histogram() + facet_wrap(~teamName))
    
    # annual income of each team
    print(ggplot(annual, aes(x=annualIncome)) + geom_histogram() + facet_wrap(~teamName))
}, finally={
    dev.off()  
})
