###################################################
# Author> 	Gonzalo Oyanedel Vial				  #
#			https://goyanedelv.github.io		  #
# Task>		McKinsey Analytics Online Hackathon   #
# Date>		July 20 - 22, 2018					  #
# Version>	3.0/simplified/DRAFT				  #
###################################################

# Folder Structure
	# McKH
		# 1.data
		# 2.code
		# 3.output

# Preamble

	# Packages
		library(randomForest)
		library(Hmisc)

	# Working directory
		setwd("~/McKH")

	# Data
		train = read.csv("1.data/train_ZoGVYWq.csv")
		test = read.csv("1.data/test_66516Ee.csv")

	# Exploratory
		nrow(train) == nrow(unique(train))
		nrow(train) == length(unique(train$id))
		sapply(train, function(y) sum(length(which(is.na(y)))))
		# check ids train vs test
		# check age

# Solving A: The base probability of receiving a premium on a policy without considering any incentive
	
	# NA handling

		formula= ~ perc_premium_paid_by_cash_credit+age_in_days+Income+Count_3.6_months_late+Count_6.12_months_late+
				Count_more_than_12_months_late+application_underwriting_score+no_of_premiums_paid+
				sourcing_channel+residence_area_type+premium

		lala = aregImpute(formula,data = train,n.impute=10, nk=0)
		fill_data = function(impute = lala, data = train, im = 1) {
								cbind.data.frame(impute.transcan(x = impute, 
							                                   imputation = im, 
							                                   data = data, 
							                                   list.out = TRUE, 
							                                   pr = FALSE))}

		lele = aregImpute(formula,data = test,n.impute=10, nk=0)
		fill_data2 = function(impute = lele, data = test, im = 1) {
								cbind.data.frame(impute.transcan(x = impute, 
							                                   imputation = im, 
							                                   data = data, 
							                                   list.out = TRUE, 
							                                   pr = FALSE))}
								
		train_imp = fill_data(im = 1)
		train_imp = cbind(train_imp,train$id,train$renewal)
		names(train_imp)[c(12,13)] = c("id","renewal")

		test_imp = fill_data2(im = 3)
		test_imp = cbind(test_imp,test$id)
		names(test_imp)[c(12)] = c("id")

	# Random Forest
		train_imp$renewal = as.factor(train_imp$renewal)
		samp = sample(nrow(train_imp), 0.6 * nrow(train_imp))
		train_imp_train = train_imp[samp, ]
		train_imp_validate = train_imp[-samp, ]

		rf_classifier = randomForest(renewal ~ ., data=train_imp_train[,-c(12)], ntree=100, mtry=2, importance=TRUE)#,na.action=na.exclude)

		pred = predict(rf_classifier, type="prob",newdata=train_imp_validate[,-c(12)])

		acc = table(round(pred[,2]), train_imp_validate$renewal)

		accuracy=(acc[1,1]+acc[2,2])/nrow(train_imp_validate)

		predicted = predict(rf_classifier, type="prob",newdata=test_imp[,-c(12)])

# Solving B: The monthly incentives you will provide on each policy to maximize the net revenue 

	# Auxiliary Relations from the Client
		 Effort = function(Incentives){
			return(10*(1-exp(-Incentives/400)))
		 }
		 	
		 Improvement = function(Effort){
			return(20*(1-exp(-Effort/5)))
		 }

		 IncToImprov = function(Incentives){
		 	return(20*(1-exp(-(10*(1-exp(-Incentives/400)))/5)))

		 }

	# Preparing df
		df_test = cbind(test_imp,predicted[,2])
		names(df_test)[13] = "renewal_est"
		#df_test = na.omit(df_test) # temporal

	# Optomization
		MaxTotalNetRevenue_i = function(incentive,p_est,premium){
				valor = min(p_est*(1+IncToImprov(incentive)/100),1)*premium-max(0,incentive)
							
			return(valor)
		}

		Maxer = function(p_est,premium){
			optim = optim(fn=MaxTotalNetRevenue_i,par=0,p_est=p_est,premium=premium,
				lower=0,control=list(fnscale=-1),method="L-BFGS-B")
			
			result = c(optim$value,optim$par)
			names(result) = c("MaxTNR","Incentive")
			return(result)
		}

	# Calculating the optimal incentives
		incentives_set = rep(0,nrow(df_test))
		TNR = 0

		for(i in 1:nrow(df_test)){

			opt = Maxer(df_test$renewal_est[i],df_test$premium[i])
			incentives_set[i] = opt[2]
			TNR = TNR + opt[1]
		}

# Output file
	df_test_output = data.frame(id=df_test$id,renewal=df_test$renewal_est,incentives=incentives_set)
	write.csv(df_test_output,paste0("3.output/gov-",substring(Sys.time(), 1, 10),".csv"),row.names=FALSE)

