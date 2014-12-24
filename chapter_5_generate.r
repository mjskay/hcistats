### generate the data used in chapter 5. might also be useful for others
source("generate.R")
source("chapter_5_functions.R")

# add monthly number of sales to the email response time data. This is 
# inversely proportional to response time, with some added noise
email$monthlySales = floor(10 * pmax(email$responseTime + rnorm(nrow(email)), .05)^-0.5)


# generate salary based on base salary plus some commission from above sales
averageOrderValue = 500
commission = .2
baseSalary = 30000
email$annualIncome = baseSalary * email$monthlySales * 12 * averageOrderValue * commission




#make some reference plots
pdf("chapter_5_plots.pdf")
tryCatch({
    # monthly sales versus email response time
    print(ggplot(email, aes(x=responseTime, y=monthlySales, color=teamName)) + geom_point() + geom_hline(yintercept=0)) 
    
    # annual income of each team
    print(ggplot(email, aes(x=annualIncome)) + geom_histogram() + facet_wrap(~teamName))
}, finally={
    dev.off()  
})
