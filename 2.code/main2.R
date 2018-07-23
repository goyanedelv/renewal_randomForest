###################################################
# Author> 	Gonzalo Oyanedel Vial				  #
#			https://goyanedelv.github.io		  #
# Task>		McKinsey Analytics Online Hackathon   #
# Date>		July 20 - 22, 2018					  #
###################################################

# Folder Structure
	# McKH
		# 1.data
		# 2.code
		# 3.output
# Preamble

	# Packages
		library(randomForest)
		library(foreach)
		library(DEoptim)
		library(locfit)

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

	# Random Forest
		train$renewal <- as.factor(train$renewal)
		rf_classifier = randomForest(renewal ~ ., data=train[,seq(2,13)], ntree=100, mtry=2, importance=TRUE,na.action=na.exclude)
		predicted = predict(rf_classifier, type="prob",newdata=test[,seq(2,12)])

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

		 # Inverses
		 ImprovToInc = function(Impr){
		 	return(-log(0.5*log((20-Impr)/20)-2)*400)
		 }

 		InvEffort = function(eff){
 			return(-400*log(1-eff/10))
 		}

 		InvImprovement = function(impr){
 			return(-5*log(1-impr/20))
 		}

 		# We have some invertibility problems for reaching from Eq 2 to Eq 1.
 		# Let's analyze the chart Incentives vs Improvements

	# Graphing
		vector_incentives = seq(1,4000)
		vector_effort = sapply(vector_incentives,function(y) Effort(y))
		df_plot_effort = data.frame(vector_incentives,vector_effort)

		vector_improv = sapply(vector_effort,function(y) Improvement(y))
		df_plot_improv = data.frame(vector_effort,vector_improv)

		vector_improv2 = sapply(vector_incentives,function(y) IncToImprov(y))
		df_plot_mix = data.frame(vector_incentives,vector_improv2)

	 	par(mfrow=c(3,1))
	 	plot(df_plot_effort)
	 	plot(df_plot_improv)
	 	plot(df_plot_mix)
	 	dev.off() #temporal

	# Some conclusions regarding the equations and charts
		# Eq Effort has its max at Effort(infinity) = 10
		# Thus, Eq Improvement, has constrained
		# as its max at Improvement(Effort[infinity]) = 17.29
			maxImprov = Improvement(Effort(999999999999))
		# despite analytically can reach 20
		# Thus, Improvement = f(Incentives)
		# Curve is more pronounced with compounded Eq (good for us!)
		# Improvement = f(Incentives) has the same behavior as enzymes
		# Improvement = f(Incentives) can be modelled with Michaelis-Menten Eq
		# V0 = Vmax ([S]/([S] + KM))
		# Improvement = 17.29*(Incentive/(Incentive + KM))
		# Km is the Incentive such MaxImprovement/2, Km = 133.09222999
		# Improvement = 17.29*(Incentive/(Incentive + 133.09222999))
		# 1/Improvement = 7.697642*(1/Incentive) + 1/17.29, which is linear
		# Y = beta_o + beta_1*X

	# Then, my invertible equation can be determined with:

		df_linear_rel = data.frame(X= 1/vector_incentives,Y=1/vector_improv2)
		linearMod = lm(X ~ Y, data=df_linear_rel)

		InvertEnzyme = function(improvement){
			incentive = linearMod$coefficients[1] + (1/improvement)*linearMod$coefficients[2]
			return(1/incentive)

		}
		# not accurate enoughh
	
	# Testing with local regression
		locfit_aprox=locfit.raw(x=vector_improv2,y=vector_incentives,kern="tri",alpha=0.02)
		# predict(locfit_aprox,IncToImprov(2.5))
		# predict(locfit_aprox,IncToImprov(25))
		# predict(locfit_aprox,IncToImprov(250))
		# predict(locfit_aprox,IncToImprov(2500))
		# Good for dominum until Incentive>3000, deviate
		# it also deviate at 0
		# Wrapped in a function:
		ImprovToInc2 = function(Impr){
		 	
			if(Impr == 0){
				return(0)
			}
			else(return(round(predict(locfit_aprox,Impr),digits=0)))
			#else(return(predict(locfit_aprox,Impr)))

		 }

	# Preparing df
		df_test = cbind(test,predicted[,2])
		names(df_test)[13] = "renewal_est"
		df_test = na.omit(df_test) # temporal

	# Definig the Optimization Function
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

		incentives_set = rep(0,nrow(df_test))
		TNR = 0

		for(i in 1:nrow(df_test)){

			opt = Maxer(df_test$renewal_est[i],df_test$premium[i])
			incentives_set[i] = opt[2]
			TNR = TNR + opt[1]

		}

### workspace
		df_test$maxP_CAP = df_test$maxP
		df_test$maxP_CAP[df_test$maxP>maxImprov/100] = maxImprov /100 

		# Given the max Improvement, what's the max Incentive?
		df_test$maxInc =sapply(df_test$maxP_CAP*100,function(y) ImprovToInc2(y))

		already_optimized = subset(df_test,df_test$maxIn ==0)
		to_optimize = subset(df_test,df_test$maxIn> 0)

		# The minimum Incentive is 0
		lowers = rep(0,nrow(to_optimize))

		# The maximum Incentive is stored in df_test$maxInc
		uppers = to_optimize$maxInc #rep(2500,nrow(to_optimize))

		oneCore <- system.time( DEoptim(fn=TotalNetRevenue, lower=lowers, upper=uppers,
			control=list(NP=10*500, itermax=50, trace = FALSE)))

		withParallel <- system.time( DEoptim(fn=TotalNetRevenue, lower=lowers, upper=uppers,
			control=list(NP=10*500, itermax=50, parallelType=1, trace = FALSE,parVar=c("to_optimize"))))

		(oneCore)
		(withParallel)