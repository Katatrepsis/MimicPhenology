# GENERAL MODEL SUMMARY
# The model is designed to simulate one population of mimics and one population of models over a single season.  This
# is accomplished through a temperature function which boths stimulates development to a certain degree-day threshold
# at which the animals emerge, and kills the remaining animals at the end of the season.  Each model and mimic is
# allocated a specific phenotype, specified along a single axis, which varies depending on the mimetic similarity.
# Mimetic similarity is the difference between mimic.mean.phenotype and model.mean.phenotype. After emergence, predators
# interact with the models and mimics, encountering one per day (for ease of calculation) and deciding on whether to
# attack or not depending on a combination of prior probability of attack and a discriminability between models and
# mimics.  Predator probability to attack a given phenotype is determined by what they identify the individuals as
# and the resulting pay-off.  The model concludes by calculating the percentage mortality of models and mimics at a
# range of different (i) relative phenologies, and (ii) mimetic similarity under a series of replicates, creating a
# 3D array of phenology x similarity x replicate.  This result is then plotted as a heat map.  Also included are some
# basic plots for visualising the concepts.  I have attempted to comment the code as well as possible:

# General model parameters
	# Number of days to run the model for
	model.days<-365
	# A time function to help with the sin-wave temperature function
	TIME<-c(1:365)
	# A sin-wave temperature function (to simplify things)
	temp.fun<-10*sin(2*pi*(1/365)*TIME+(1.5*pi))+10
	# A temperature-related mortality function (in addition to standard daily hazard)
	temp.mort<-sin(2*pi*(1/365)*TIME+(1.5*pi))*-1+0.4
	for(x in 1:model.days){
	if(temp.mort[x]>1) {temp.mort[x]<-1}
	if(temp.mort[x]<0) {temp.mort[x]<-0}
	}
	# Visualising the temp and mort functions
	plot.ts(cbind(temp.fun,temp.mort),yax.flip = TRUE,main="Temperature and mortality functions through the year")
	
# Set varying phenology for the mimics.  The models are assumed to have fixed phenology, with the mimics varying around them, although
# this can be changed.  The model phenology is fixed below at day 150, so this phenological variation gives a spread of dates both
# before and after the models emerge.
	mimic.mean.emergence<-c(131:170)
# Set varying mimetic fidelity.  Again, the models have a fixed phenotype and the mimics vary in their appearance.  In this case, the
# models are fixed at a value of 35, but because the effects of mimetic similarity are symmetrical around the mean, I have only
# included variation to one side of the models.
	mimic.mean.phenotype<-c(6:35)
# Set reps per parameter combination.  This just sets the number of replicates for each phenology-phenotype combination.  Note that
# processing time is LONG, so beware of going much above 10...
	model.reps<-1

# Variables to catch the predator mortality.  These arrays capture the results of the exploration of parameter space in terms of the
# mortality experienced by each of the models and mimics.  The arrays are 3D, corresponding to each of the phenotype x phenology
# parameter combinations replicated by the model.reps
	Mimic.predator.mortality<-array(0,dim=c(length(mimic.mean.phenotype),length(mimic.mean.emergence),model.reps))
	Model.predator.mortality<-array(0,dim=c(length(mimic.mean.phenotype),length(mimic.mean.emergence),model.reps))

# Some fairly cumbersome concentric loops.  This first one varies the relative phenotype using the mimic.mean.phenotype vector 
# specified above.
for(DEF in 1:length(mimic.mean.phenotype)){

# Within each level of phenotypic varation is the phenological variation, determined by mimic.mean.phenology from above.
for(ABC in 1:length(mimic.mean.emergence)){

# Finally, we have the third axis of the array which replicates the simulation for each parameter pairing.
for(GHI in 1:model.reps){
# Prey model parameters
	# Set the population size for the mimics
	mimic.pop.size<-200
	# Set the degree day threshold for adulthood
	mimic.emergence.threshold<-3650
	# Set the population size for the models
	model.pop.size<-200
	# Set the degree day threshold for adulthood
	model.emergence.threshold<-3650

# This matrix contains the information about each mimic individuals, generating four columns as the start to track mimics through
# their lives.  The first column is simply a numeric code for the animal, the second column gives an original laying date from the
# previous year according to the phenology of the species (used to create a little bit of ecologically relevant variation in 
# emergence dates), the third column allocates a phenotype according to the value of mean.mimic.phenotype in this run, and the fourth
# column simply denotes the individual as a "mimic".
	mimic.pop<-matrix(ncol=4,nrow=mimic.pop.size)
	colnames(mimic.pop)<-c("ID","Laid","Phenotype","Species")
	# COLUMN 1: ID codes for each individual
	mimic.pop[,1]<-c(1:mimic.pop.size)
	# COLUMN 2: Date of laying/hatching/birth for DD calculations
	mimic.pop[,2]<-ceiling(rnorm(mimic.pop.size,mean=mimic.mean.emergence[ABC],sd=3))
	# COLUMN 3: Phenotype allocation based on random sampling from a normal distribution of phenotypes
	mimic.pop[,3]<-rnorm(mimic.pop.size,mean=mimic.mean.phenotype[DEF],sd=3)
	# COLUMN 4: Labelling as either model or mimic
	mimic.pop[,4]<-rep("Mimic",mimic.pop.size)
	
# This matrix contains the information about each model individual as above.  Note that the values of phenology and phenotype here
# are fixed.
	model.pop<-matrix(ncol=4,nrow=model.pop.size)
	colnames(model.pop)<-c("ID","Laid","Phenotype","Species")
	# COLUMN 1: ID codes for each individual
	model.pop[,1]<-c(1:model.pop.size)
	# COLUMN 2: Date of laying/hatching/birth for DD calculations
	model.pop[,2]<-ceiling(rnorm(model.pop.size,mean=150,sd=3))
	# COLUMN 3: Phenotype allocation based on random sampling from a normal distribution of phenotypes
	model.pop[,3]<-rnorm(model.pop.size,mean=35,sd=3)
	# COLUMN 4: Labelling as either model or mimic
	model.pop[,4]<-rep("Model",model.pop.size)

# These matrices hold the behavioural state of the predator according to a number of different aspects of the predator-prey interaction:
	# Set the number of predators (rapidly slows things down above a certain number - 3 gives a workable result for now)
	pred.num<-3
	Time.point<-numeric(length=model.days)
	# Set up a matrix where each column is a predator and each row is a day of the season.  For each day, a predator's probability of 
	# attacking models and mimics changes depending on their experience with the two types of prey.  I'll go into this more when we
	# get to the behavioural interactions.
	p.attack.model<-matrix(ncol=pred.num,nrow=model.days) # p(attack) for models starts at 1
	p.attack.mimic<-matrix(ncol=pred.num,nrow=model.days) # p(attack) for mimics starts at 1
	# This matrix specifies which type of prey item (if any) was encountered by each predator
	enc.species<-matrix(ncol=pred.num,nrow=model.days)
	# This matrix specifies whether the prey were attacked by each predator
	attacked<-matrix(ncol=pred.num,nrow=model.days)
	# This matrix represents the ability of a predator to learn to distinguish models from mimics.  I have coded this as a reduction in the
	# standard deviation of perceived phenotypic differences in both the models and the mimics.  This will become clearer where I use the
	# parameter further down.  Basically, the uncertainty over the identification of models and mimics decays exponentially with a greater
	# number of encounters.
	learning<-matrix(ncol=pred.num,nrow=model.days)
	
# The "year" begins here and lasts model.days
	for(y in 1:model.days){
	# For each of the models and mimics, we produce a vector that tracks their life history stage (and, hence, availability to predators) on 
	# each day of the model.  Naturally this is clumsy and inefficient, but I can't think of an alternative.
	newday.mim<-vector(length=mimic.pop.size)
	newday.mod<-vector(length=model.pop.size)
	
	# Check on the life history stage of each of the mimics
	for(x in 1:mimic.pop.size){
	# Make sure that the mimics stay dead once they die.
	if(mimic.pop[x,y+3]=="Dead" | mimic.pop[x,y+3]=="Still dead") {newday.mim[x]<-"Still dead" ; next}
	# Fixed mortality rate (random and probabilistic at 0.1) per day and an extra mortality from temperature to represent seasonal mortality
	if(mimic.pop[x,y+3]=="adult" & runif(1)>=0.9) {newday.mim[x]<-"Dead" ; next}
	if(mimic.pop[x,y+3]=="adult" & runif(1)<=temp.mort[y]) {newday.mim[x]<-"Dead" ; next}
	# The larvae transition into adults if they have reached the degree day threshold.
	if((sum(temp.fun[mimic.pop[x,2]:365])+sum(temp.fun[1:y]))>3650) {newday.mim[x]<-"adult"} else {newday.mim[x]<-"larva"}
	}
	
	# Check on the life history stage of each of the models
	for(x in 1:model.pop.size){
	# Make sure that the models stay dead once they die.
	if(model.pop[x,y+3]=="Dead" | model.pop[x,y+3]=="Still dead") {newday.mod[x]<-"Still dead"; next}
	# Fixed mortality rate (probabilistic at 0.15) per day and an extra mortality from temperature
	if(model.pop[x,y+3]=="adult" & runif(1)>=0.9) {newday.mod[x]<-"Dead" ; next}
	if(model.pop[x,y+3]=="adult" & runif(1)<=temp.mort[y]) {newday.mod[x]<-"Dead" ; next}
	# The larvae transition into adults if they have reached the degree day threshold.
	if((sum(temp.fun[model.pop[x,2]:365])+sum(temp.fun[1:y]))>3650) {newday.mod[x]<-"adult"} else {newday.mod[x]<-"larva"}
	}
	
	# Add the new life history updates to the growing matrices
	mimic.pop<-cbind(mimic.pop,newday.mim)
	model.pop<-cbind(model.pop,newday.mod)

# This code looks at the predator response to each of the models and mimics, taking each predator in turn.
for (z in sample(c(1:pred.num),pred.num)){	
	# If there aren't any adults and we are on the first day of the year, establish the initial values (probability of attacking anything is 1) and move to the next day
	if(y==1 & length(subset(c(mimic.pop[,1],model.pop[,1]),c(mimic.pop[,y+4],model.pop[,y+4])=="adult"))==0) {Time.point[y]<-y; p.attack.model[y,z]<-1; p.attack.mimic[y,z]<-1; enc.species[y,z]<-"None"; attacked[y,z]<-"None" ; next}
	# If there aren't any adults and we are not on the first day of the year, everything stays the same and move to the next day
	if(y>1 & length(subset(c(mimic.pop[,1],model.pop[,1]),c(mimic.pop[,y+4],model.pop[,y+4])=="adult"))==0) {Time.point[y]<-y; p.attack.model[y,z]<-p.attack.model[y-1,z]; p.attack.mimic[y,z]<-p.attack.mimic[y-1,z]; enc.species[y,z]<-"None"; attacked[y,z]<-"None" ; next}
	# If there are adults, select one at random out of the pool of mimics and models
	enc<-sample(1:length(subset(c(mimic.pop[,1],model.pop[,1]),c(mimic.pop[,y+4],model.pop[,y+4])=="adult")),1)
	# Find that individual's phenotype
	enc.phenotype<-as.numeric(subset(c(mimic.pop[,3],model.pop[,3]),c(mimic.pop[,y+4],model.pop[,y+4])=="adult")[enc])
	# Find that individual's species
	enc.species[y,z]<-subset(c(mimic.pop[,4],model.pop[,4]),c(mimic.pop[,y+4],model.pop[,y+4])=="adult")[enc]
	# Find that individual's unique ID (can link this to evolutionary algorithms later, but not yet)
	enc.id<-subset(c(mimic.pop[,1],model.pop[,1]),c(mimic.pop[,y+4],model.pop[,y+4])=="adult")[enc]
	# Calculate the current extent of the focal predator's learning.  This is through exponential decay of the SD of the signal detection 
	# distribution from 3 to 1. Note that the predator only learns when it actually attacks the prey item.
	learning[y,z]<-2*exp(-0.05*length(subset(attacked[,z],attacked=="attacked")))+1
	# The predator makes a decision over whether or not to attack the prey item based on the relatively likelihood of it being either a mimic
	# or a model.  This code says that the predator will consider the prey a model if a random number between 0 and 1 is lower than the relative 
	# probability that the animal is a model.  Hence, when the probability of the phenotype indicating a model is high, there is a high chance that
	# that probability will be greater than a random number between 0 and 1, and the predator will be more likely to guess "model".  The sd
	# of 3 can be replaced by learning[y,z] to make the predators learn.  Currently their response is constant.
	if(runif(1)<dnorm(enc.phenotype,mean=35,sd=3)/(dnorm(enc.phenotype,mean=35,sd=3)+dnorm(enc.phenotype,mean=mimic.mean.phenotype[DEF],sd=3))) {guess<-"model"} else {guess<-"mimic"}

# For the first time step, it was easier to specify what was happening.  Because the initial p.attack for both models and mimics is 1, the
# predator will attack whatever it sees.  Of course, under the current temp.fun, it never sees anything on day 1 so this is moot.
if(y==1){
	# Set a mimic attack random roll
	RRmimic<-runif(1)
	# Set a mimic attack random roll
	RRmmodel<-runif(1)
	# There are 6 potential outcomes from the identification and actual identity:
	# If the prey is identified as a model but predator decides not to attack and it is a model: 	"not attacked" 	p(attack model) remains the same	p(attack mimic) remains the same
	if(guess=="model" & runif(1)>1 & enc.species[y]=="Model") {attacked[y,z]<-"not attacked" & p.attack.model[y,z]<-1 & p.attack.mimic<-1}
	# If the prey is identified as a model but predator decides not to attack and it is a mimic: 	"not attacked" 	p(attack model) remains the same	p(attack mimic) remains the same
	if(guess=="model" & runif(1)>1 & enc.species[y]=="Mimic") {attacked[y,z]<-"not attacked" & p.attack.model[y,z]<-p.attack.model[y,z] & p.attack.mimic[y,z]<-1}
	# If the prey is identified as a model and predator decides to attack and it is a model: 		"attacked" 		p(attack model) declines			p(attack mimic) remains the same	
	if(guess=="model" & runif(1)<1 & enc.species[y]=="Model") {attacked[y,z]<-"attacked" & p.attack.model[y,z]<-1*exp(-0.1) & p.attack.mimic[y,z]<-1}
	# If the prey is identified as a model and predator decides to attack and it is a mimic: 		"attacked" 		p(attack model) increases			p(attack mimic) remains the same
	if(guess=="model" & runif(1)<1 & enc.species[y]=="Mimic") {attacked[y,z]<-"attacked" & p.attack.model[y,z]<-1*exp(0.1) & p.attack.mimic[y,z]<-1}
	# If the prey is identified as a mimic and it is a mimic: 										"attacked" 		p(attack model) remains the same	p(attack mimic) increases
	if(guess=="mimic" & enc.species[y]=="Mimic") {attacked[y,z]<-"attacked" & p.attack.model[y,z]<-p.attack.model[y,z] & p.attack.mimic[y,z]<-1*exp(0.1)}
	# If the prey is identified as a mimic and it is a model: 										"attacked" 		p(attack model) remains the same	p(attack mimic) declines
	if(guess=="mimic" & enc.species[y]=="Model") {attacked[y,z]<-"attacked" & p.attack.model[y,z]<-p.attack.model[y,z] & p.attack.mimic[y,z]<-1*exp(-0.1)}	
	# If the prey is identified as a mimic but predator decides not to attack and it is a model: 	"not attacked" 	p(attack model) remains the same	p(attack mimic) remains the same
	if(guess=="mimic" & enc.species[y]=="Mimic") {attacked[y,z]<-"attacked" & p.attack.model[y,z]<-p.attack.model[y,z] & p.attack.mimic[y,z]<-p.attack.mimic[y,z]}
	# If the prey is identified as a mimic but predator decides not to attack and it is a mimic: 	"not attacked" 	p(attack model) remains the same	p(attack mimic) remains the same
	if(guess=="mimic" & enc.species[y]=="Model") {attacked[y,z]<-"attacked" & p.attack.model[y,z]<-p.attack.model[y,z] & p.attack.mimic[y,z]<-p.attack.mimic[y,z]}	
	}
	
# For all other time steps, the model refers to data for previous time steps
if(y>1){
	# Set a mimic attack random roll
	RRmimic<-runif(1)
	# Set a mimic attack random roll
	RRmmodel<-runif(1)
	# There are 6 potential outcomes from the identification and actual identity:
	# If the prey is identified as a model but predator decides not to attack and it is a model: 	"not attacked" 	p(attack model) remains the same	p(attack mimic) remains the same
	if(guess=="model" & RRmmodel>p.attack.model[y-1,z] & enc.species[y,z]=="Model") {attacked[y,z]<-"not attacked" ; p.attack.model[y,z]<-p.attack.model[y-1,z] ; p.attack.mimic[y,z]<-p.attack.mimic[y-1,z]}
	# If the prey is identified as a model but predator decides not to attack and it is a mimic: 	"not attacked" 	p(attack model) remains the same	p(attack mimic) remains the same
	if(guess=="model" & RRmmodel>p.attack.model[y-1,z] & enc.species[y,z]=="Mimic") {attacked[y,z]<-"not attacked" ; p.attack.model[y,z]<-p.attack.model[y-1,z] ; p.attack.mimic[y,z]<-p.attack.mimic[y-1,z]}
	# If the prey is identified as a model and predator decides to attack and it is a model: 		"attacked" 		p(attack model) declines			p(attack mimic) remains the same	
	if(guess=="model" & RRmmodel<p.attack.model[y-1,z] & enc.species[y,z]=="Model") {attacked[y,z]<-"attacked" ; p.attack.model[y,z]<-p.attack.model[y-1,z]*exp(-0.1) ; p.attack.mimic[y,z]<-p.attack.mimic[y-1,z]; model.pop[as.numeric(enc.id),y+4]<-"Dead"}
	# If the prey is identified as a model and predator decides to attack and it is a mimic: 		"attacked" 		p(attack model) increases			p(attack mimic) remains the same
	if(guess=="model" & RRmmodel<p.attack.model[y-1,z] & enc.species[y,z]=="Mimic") {attacked[y,z]<-"attacked" ; p.attack.model[y,z]<-p.attack.model[y-1,z]*exp(0.1) ; p.attack.mimic[y,z]<-p.attack.mimic[y-1,z]; mimic.pop[as.numeric(enc.id),y+4]<-"Dead"}
	# If the prey is identified as a mimic and it is a mimic: 										"attacked" 		p(attack model) remains the same	p(attack mimic) increases
	if(guess=="mimic" & RRmimic<p.attack.mimic[y-1,z] & enc.species[y,z]=="Mimic") {attacked[y,z]<-"attacked" ; p.attack.model[y,z]<-p.attack.model[y-1,z] ; p.attack.mimic[y,z]<-p.attack.mimic[y-1,z]*exp(0.1); mimic.pop[as.numeric(enc.id),y+4]<-"Dead"}
	# If the prey is identified as a mimic and it is a model: 										"attacked" 		p(attack model) remains the same	p(attack mimic) declines
	if(guess=="mimic" & RRmimic<p.attack.mimic[y-1,z] & enc.species[y,z]=="Model") {attacked[y,z]<-"attacked" ; p.attack.model[y,z]<-p.attack.model[y-1,z] ; p.attack.mimic[y,z]<-p.attack.mimic[y-1,z]*exp(-0.1); model.pop[as.numeric(enc.id),y+4]<-"Dead"}	
	# If the prey is identified as a mimic but predator decides not to attack and it is a model: 	"not attacked" 	p(attack model) remains the same	p(attack mimic) remains the same
	if(guess=="mimic" & RRmimic>p.attack.mimic[y-1,z] & enc.species[y,z]=="Model") {attacked[y,z]<-"not attacked" ; p.attack.model[y,z]<-p.attack.model[y-1,z] ; p.attack.mimic[y,z]<-p.attack.mimic[y-1,z]}
	# If the prey is identified as a mimic but predator decides not to attack and it is a mimic: 	"not attacked" 	p(attack model) remains the same	p(attack mimic) remains the same
	if(guess=="mimic" & RRmimic>p.attack.mimic[y-1,z] & enc.species[y,z]=="Mimic") {attacked[y,z]<-"not attacked" ; p.attack.model[y,z]<-p.attack.model[y-1,z] ; p.attack.mimic[y,z]<-p.attack.mimic[y-1,z]}
}

	# the above code can result in >1 probabilities - this sorts it out	
	if(p.attack.model[y,z]>1) {p.attack.model[y,z]<-1}
	if(p.attack.mimic[y,z]>1) {p.attack.mimic[y,z]<-1}
	}
		
}

# Record the number of models and mimics that were eaten in the array
Mimic.predator.mortality[DEF,ABC,GHI]<-table(enc.species,attacked)[1,1]
Model.predator.mortality[DEF,ABC,GHI]<-table(enc.species,attacked)[2,1]

}
}
# Convenience function for tracking progress of the code
print(paste(ABC+(length(mimic.mean.emergence)*(DEF-1)),"out of",length(mimic.mean.phenotype)*length(mimic.mean.emergence)))
flush.console()
}

# Save the resulting arrays
saveRDS(Mimic.predator.mortality,"H:\\Document Files\\Research\\HOVERFLY PROJECTS\\MIMICRY MODELLINGS\\Mimic.predator.mortality array decoupling + fidelity")
saveRDS(Model.predator.mortality,"H:\\Document Files\\Research\\HOVERFLY PROJECTS\\MIMICRY MODELLINGS\\Model.predator.mortality array decoupling + fidelity")

Mimic.predator.mortality<-readRDS("H:\\Document Files\\Research\\HOVERFLY PROJECTS\\MIMICRY MODELLINGS\\Mimic.predator.mortality array decoupling + fidelity")
Model.predator.mortality<-readRDS("H:\\Document Files\\Research\\HOVERFLY PROJECTS\\MIMICRY MODELLINGS\\Model.predator.mortality array decoupling + fidelity")


MimMort<-matrix(ncol=30,nrow=40)
ModMort<-matrix(ncol=30,nrow=40)

# Turn into matrices averaged across replicates
for(y in 1:30){
for(x in 1:40){
MimMort[x,y]<-mean(Mimic.predator.mortality[y,x,])
ModMort[x,y]<-mean(Model.predator.mortality[y,x,])
}
}

# Plot resulting parameter space with averages across replicates
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
image(MimMort,col=rainbow(10))
legend("topright",inset=c(-0.2,0), legend = c(quantile(MimMort,c(1:10)/10)), fill = rev(rainbow(10)))
image(ModMort,col=rainbow(10))
legend("topright",inset=c(-0.2,0), legend = c(quantile(ModMort,c(1:10)/10)), fill = rev(rainbow(10)))

########################################################################################################################################################################################################
########################################################################################################################################################################################################
# Illustrative plots
# 1 - Signal detection
par(mfrow=c(1,1))
plot(density(rnorm(1000,mean=35,sd=3)),col="blue",xlim=c(10,60),ylim=c(0,0.45))
lines(density(rnorm(1000,mean=30,sd=3)),col="red")
text(10,0.4,"0 experience",pos=4)
plot(density(rnorm(1000,mean=35,sd=2)),col="blue",xlim=c(10,60),ylim=c(0,0.45))
lines(density(rnorm(1000,mean=30,sd=2)),col="red")
text(10,0.4,"Some experience",pos=4)
plot(density(rnorm(1000,mean=35,sd=1)),col="blue",xlim=c(10,60),ylim=c(0,0.45))
lines(density(rnorm(1000,mean=30,sd=1)),col="red")
text(10,0.4,"Lots of experience",pos=4)

# 2 - Plot the mortality of each group for each year
plot(c(100:140),Mimic.predator.mortality,ylim=c(min(c(min(Mimic.predator.mortality),min(Model.predator.mortality))),max(c(max(Mimic.predator.mortality),max(Model.predator.mortality)))),pch=19,col="red",xlab="Phenology of mimics (models emerge on 120)",ylab="Predator-induced mortality")
points(c(100:140),Model.predator.mortality,pch=19,col="blue")

# 3 - Plot population dynamics
mim.adult.pop<-mim.larva.pop<-mod.adult.pop<-mod.larva.pop<-numeric(length=model.days)
for(x in 1:model.days){
mim.adult.pop[x]<-length(subset(mimic.pop[,x+4],mimic.pop[,x+4]=="adult"))
mim.larva.pop[x]<-length(subset(mimic.pop[,x+4],mimic.pop[,x+4]=="larva"))
mod.adult.pop[x]<-length(subset(model.pop[,x+4],model.pop[,x+4]=="adult"))
mod.larva.pop[x]<-length(subset(model.pop[,x+4],model.pop[,x+4]=="larva"))
}
par(mfrow=c(2,1))
ts.plot(mim.adult.pop,col="red",ylim=c(0,max(c(max(mim.larva.pop),max(mod.larva.pop)))))
lines(mim.larva.pop,col="red",lty=2)
lines(mod.adult.pop,col="blue")
lines(mod.larva.pop,col="blue",lty=2)

# 4 - Plot learning
ts.plot(p.attack.model,col="red")
lines(p.attack.mimic,col="blue")

