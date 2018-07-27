library(ggplot2)
library(dplyr)
library(stats)



cancer_data = read.csv('cancer_reg.csv')

##################################Missing Values Analysis###############################################
missing_val = data.frame(apply(cancer_data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(cancer_data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Missing_perc.csv", row.names = F)

ggplot(data = missing_val[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "green")+xlab("Parameter")+
  ggtitle("Missing data percentage (Train)") + theme_bw()



cancer_data_m = cancer_data[,c(-17,-24)]


str(cancer_data_m)
summary(cancer_data_m$target_deathrate)
hist(cancer_data_m$target_deathrate)


num_vars <- dplyr::select_if(cancer_data_m,is.numeric)
num_int <- dplyr::select_if(cancer_data_m,is.integer)


int_num_data <- cbind(num_int,num_vars)

int_num_data <- int_num_data[,-1]

set.seed(101)
sample <- sample.int(n = nrow(int_num_data),size = floor(0.75*nrow(int_num_data)))
train <- int_num_data[sample,]
test <- int_num_data[-sample,-5]

m <- lm(target_deathrate ~ ., data = train )
m
p <- predict(m,test)
p
summary(m)

