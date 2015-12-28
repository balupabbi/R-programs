#Author: Bhargav Pabbisetti
#ID:W1167792

#problem 2
data2 <- read.csv(file = "censusData.csv.xls", header = TRUE)
total_rows = nrow(data2)
total_columns = ncol(data2)
head(data2[2])
count_age <- 0 
count_work <- 0
count_edu <- 0 
count_marital <- 0
count_occupation <- 0 
count_race <- 0 
count_sex <- 0
count_hrsperweek <- 0
count_income <-0

for(i in 1:total_rows){
  if(data2[i,"age"] == '?'){
    count_age<-count_age +1
  }
}

for(i in 1:total_rows){
  if(data2[i,"work"] == '?'){
    count_work<-count_work +1
  }
}

for(i in 1:total_rows){
  if(data2[i,"edu"] == '?'){
    count_edu<-count_edu +1
  }
}

for(i in 1:total_rows){
  if(data2[i,"marital"] == '?'){
    count_marital<-count_marital +1
  }
}

for(i in 1:total_rows){
  if(data2[i,"occupation"] == '?'){
    count_occupation<-count_occupation +1
  }
}

for(i in 1:total_rows){
  if(data2[i,"race"] == '?'){
    count_race<-count_race +1
  }
}

for(i in 1:total_rows){
  if(data2[i,"sex"] == '?'){
    count_sex<-count_sex +1
  }
}

for(i in 1:total_rows){
  if(data2[i,"hrs_per_week"] == '?'){
    count_hrsperweek<-count_hrsperweek +1
  }
}

for(i in 1:total_rows){
  if(data2[i,"income"] == '?'){
    count_income<-income +1
  }
}

p_age = (count_age/total_rows)*100
p_work = (count_work/total_rows)*100
p_edu = (count_edu/total_rows)*100
p_marital = (count_marital/total_rows)*100
p_occupation = (count_occupation/total_rows)*100
p_race = (count_race/total_rows)*100
p_sex = (count_sex/total_rows)*100
p_hrsperweek = (count_hrsperweek/total_rows)*100
p_income = (count_income/total_rows)*100

cat(sprintf("Percentage of missing values for age: %0.2f", p_age))
cat(sprintf("Percentage of missing values for work: %0.2f", p_work))
cat(sprintf("Percentage of missing values for edu: %0.2f", p_edu))
cat(sprintf("Percentage of missing values for marital: %0.2f", p_marital))
cat(sprintf("Percentage of missing values for occupation: %0.2f", p_occupation))
cat(sprintf("Percentage of missing values for race: %0.2f", p_race))
cat(sprintf("Percentage of missing values for sex: %0.2f", p_sex))
cat(sprintf("Percentage of missing values for hrs_per_week: %0.2f", p_hrsperweek))
cat(sprintf("Percentage of missing values for income: %0.2f", p_income))


#problem 2b

#x_pt <- list(),x_pt$sent <- list(count)
require(ggplot2)
missing_values = c()
count_miss<-0
for(i in 1:total_rows){
     for(j in 1:9){
       if(data2[i,j]=="?"){
         count_miss <- count_miss +1
         #cat(sprintf("Row %d missing count is %d\n", i,count))
       }
       else{
         #cat(sprintf("Row %d missing count is %d\n", i,count))
         count<-0
       }
     }
  #cat(sprintf("Row %d missing count is %d\n", i,count_miss))
  missing_values = append(missing_values,count_miss)
  count_miss<-0
}

x_val = c(0:2)
hist_plot = data.frame(missing_values)
graphobj = ggplot(hist_plot,aes(missing_values))+geom_histogram()+scale_x_discrete(limits=c(0,1,2))
graphobj

#problem 3
#histograms
graphobj2 = ggplot(data2,aes(age))+geom_histogram()+xlab('Age')+ylab('Counts based on Income')+facet_grid(income ~.)
graphobj2
graphobj3 = ggplot(data2,aes(hrs_per_week))+geom_histogram()+xlab('hrs_per_week')+ylab('Counts based on Income')+facet_grid(income ~.)
graphobj3

#boxplots
graphobj4 = ggplot(data=data2,aes(x=income,y=age))+geom_boxplot()+xlab('Income Categories')+ylab('Age')
graphobj4
graphobj5 = ggplot(data2,aes(x=income,y=hrs_per_week))+geom_boxplot()+xlab('Income Categories')+ylab('Hours Per Week')
graphobj5

#problem 4a
graphobj6 = ggplot(data=data2,aes(work))+geom_bar()+xlab('Work Categories')+ylab('Frequency')
graphobj6
graphobj7 = ggplot(data=data2,aes(edu))+geom_bar()+xlab('EDU Categories')+ylab('Frequency')
graphobj7
graphobj8 = ggplot(data=data2,aes(marital))+geom_bar()+xlab('Marital')+ylab('Frequency')
graphobj8
graphobj9 = ggplot(data=data2,aes(occupation))+geom_bar()+xlab('Occupation')+ylab('Frequency')
graphobj9
graphobj10 = ggplot(data=data2,aes(race))+geom_bar()+xlab('Race')+ylab('Frequency')
graphobj10
graphobj11 = ggplot(data=data2,aes(sex))+geom_bar()+xlab('Sex')+ylab('Frequency')
graphobj11

#problem 4b
graphobj12 = ggplot(data=data2,aes(work))+geom_bar()+xlab('Work Categories')+ylab('Frequency')+facet_grid(income ~.)
graphobj12
graphobj13 = ggplot(data=data2,aes(edu))+geom_bar()+xlab('EDU Categories')+ylab('Frequency')+facet_grid(income ~.)
graphobj13
graphobj14 = ggplot(data=data2,aes(marital))+geom_bar()+xlab('Marital')+ylab('Frequency')+facet_grid(income ~.)
graphobj14
graphobj15 = ggplot(data=data2,aes(occupation))+geom_bar()+xlab('Occupation')+ylab('Frequency')+facet_grid(income ~.)
graphobj15
graphobj16 = ggplot(data=data2,aes(race))+geom_bar()+xlab('Race')+ylab('Frequency')+facet_grid(income ~.)
graphobj16
graphobj17 = ggplot(data=data2,aes(sex))+geom_bar()+xlab('Sex')+ylab('Frequency')+facet_grid(income ~.)
graphobj17

#problem 5
graphobj18 = ggplot(data=data2,aes(x=age, y=hrs_per_week))+geom_point()+xlab('Age')+ylab('hrs_per_week')
graphobj18
graphobj18 = ggplot(data=data2,aes(x=age, y=hrs_per_week))+geom_point(alpha=0.1)+xlab('Age')+ylab('hrs_per_week')
graphobj18
correlation_coefficient = cor.test(data2$age,data2$hrs_per_week)
correlation_coefficient

#problem 6
graphobj19 = ggplot(data=data2,aes(x=age, y=hrs_per_week, color=work))+geom_point(alpha=0.1)+xlab('Age')+ylab('hrs_per_week')
graphobj19

graphobj21 = ggplot(data=data2,aes(x=age, y=hrs_per_week, color=edu))+geom_point(alpha=0.1)+xlab('Age')+ylab('hrs_per_week')
graphobj21

graphobj23 = ggplot(data=data2,aes(x=age, y=hrs_per_week, color=marital))+geom_point(alpha=0.1)+xlab('Age')+ylab('hrs_per_week')
graphobj23

graphobj25 = ggplot(data=data2,aes(x=age, y=hrs_per_week, color=occupation))+geom_point(alpha=0.1)+xlab('Age')+ylab('hrs_per_week')
graphobj25

graphobj27 = ggplot(data=data2,aes(x=age, y=hrs_per_week, color=race))+geom_point(alpha=0.1)+xlab('Age')+ylab('hrs_per_week')
graphobj27

graphobj29 = ggplot(data=data2,aes(x=age, y=hrs_per_week, color=sex))+geom_point(alpha=0.1)+xlab('Age')+ylab('hrs_per_week')
graphobj29

