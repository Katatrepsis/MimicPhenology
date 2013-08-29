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

# Set varying phenology
	mimic.mean.emergence<-c(131:170)
# Set varying mimetic fidelity
	mimic.mean.phenotype<-c(6:35)
# Set reps per parameter combination
	model.reps<-1

# Variables to catch the predator mortality
	Mimic.predator.mortality<-array(0,dim=c(length(mimic.mean.phenotype),length(mimic.mean.emergence),model.reps))
	Model.predator.mortality<-array(0,dim=c(length(mimic.mean.phenotype),length(mimic.mean.emergence),model.reps))
		
for(DEF in 1:length(mimic.mean.phenotype)){
for(ABC in 1:length(mimic.mean.emergence)){
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

# This matrix contains the information about each mimic individual (and backs that up in case of cock-up)
	mimic.pop<-matrix(ncol=4,nrow=mimic.pop.size)
	colnames(mimic.pop)<-c("ID","Laid","Phenotype","Species")
	# ID codes for each individual
	mimic.pop[,1]<-c(1:mimic.pop.size)
	# Date of laying/hatching/birth for DD calculations
	mimic.pop[,2]<-ceiling(rnorm(mimic.pop.size,mean=mimic.mean.emergence[ABC],sd=3))
	# Phenotype allocation based on random sampling from a normal distribution of phenotypes
	mimic.pop[,3]<-rnorm(mimic.pop.size,mean=mimic.mean.phenotype[DEF],sd=3)
	# Labelling as either model or mimic
	mimic.pop[,4]<-rep("Mimic",mimic.pop.size)
	# Backup just in case
	mimic.pop.backup<-mimic.pop
	
# This matrix contains the information about each model individual (and backs that up in case of cock-up)
	model.pop<-matrix(ncol=4,nrow=model.pop.size)
	colnames(model.pop)<-c("ID","Laid","Phenotype","Species")
	# ID codes for each individual
	model.pop[,1]<-c(1:model.pop.size)
	# Date of laying/hatching/birth for DD calculations
	model.pop[,2]<-ceiling(rnorm(model.pop.size,mean=150,sd=3))
	# Phenotype allocation based on random sampling from a normal distribution of phenotypes
	model.pop[,3]<-rnorm(model.pop.size,mean=35,sd=3)
	# Labelling as either model or mimic
	model.pop[,4]<-rep("Model",model.pop.size)
	# Backup just in case
	model.pop.backup<-model.pop

# Here is the predator behaviour data (in separate columns because matrices can't handle factors and numeric)
	pred.num<-3
	Time.point<-numeric(length=model.days)
	p.attack.model<-matrix(ncol=pred.num,nrow=model.days) # p(attack) for models starts at 1
	p.attack.mimic<-matrix(ncol=pred.num,nrow=model.days) # p(attack) for mimics starts at 1
	enc.species<-matrix(ncol=pred.num,nrow=model.days)
	attacked<-matrix(ncol=pred.num,nrow=model.days)
	learning<-matrix(ncol=pred.num,nrow=model.days)
	
# The emergence function that produces a series of adults
	for(y in 1:model.days){
	# for each of the models and mimics, we produce a vector that tracks their life history stage (and, hence, availability to predators)
	newday.mim<-vector(length=mimic.pop.size)
	newday.mod<-vector(length=model.pop.size)
	
	# Check on the life history stage of the mimics
	for(x in 1:mimic.pop.size){
	if(mimic.pop[x,y+3]=="Dead" | mimic.pop[x,y+3]=="Still dead") {newday.mim[x]<-"Still dead" ; next}
	# Fixed mortality rate (random and probabilistic at 0.1) per day and an extra mortality from temperature to represent seasonal mortality
	if(mimic.pop[x,y+3]=="adult" & runif(1)>=0.9) {newday.mim[x]<-"Dead" ; next}
	if(mimic.pop[x,y+3]=="adult" & runif(1)<=temp.mort[y]) {newday.mim[x]<-"Dead" ; next}
	if((sum(temp.fun[mimic.pop[x,2]:365])+sum(temp.fun[1:y]))>3650) {newday.mim[x]<-"adult"} else {newday.mim[x]<-"larva"}
	}
	# Check on the life history stage of the models
	for(x in 1:model.pop.size){
	if(model.pop[x,y+3]=="Dead" | model.pop[x,y+3]=="Still dead") {newday.mod[x]<-"Still dead"; next}
	# Fixed mortality rate (probabilistic at 0.15) per day and an extra mortality from temperature
	if(model.pop[x,y+3]=="adult" & runif(1)>=0.9) {newday.mod[x]<-"Dead" ; next}
	if(model.pop[x,y+3]=="adult" & runif(1)<=temp.mort[y]) {newday.mod[x]<-"Dead" ; next}
	if((sum(temp.fun[model.pop[x,2]:365])+sum(temp.fun[1:y]))>3650) {newday.mod[x]<-"adult"} else {newday.mod[x]<-"larva"}
	}
	# Add the new life history updates to the growing matrices
	mimic.pop<-cbind(mimic.pop,newday.mim)
	model.pop<-cbind(model.pop,newday.mod)
for (z in 1:pred.num){	
	# If there aren't any adults, move to the next day
	if(y==1 & length(subset(c(mimic.pop[,1],model.pop[,1]),c(mimic.pop[,y+4],model.pop[,y+4])=="adult"))==0) {Time.point[y]<-y; p.attack.model[y,z]<-1; p.attack.mimic[y,z]<-1; enc.species[y,z]<-"None"; attacked[y,z]<-"None" ; next}
	if(y>1 & length(subset(c(mimic.pop[,1],model.pop[,1]),c(mimic.pop[,y+4],model.pop[,y+4])=="adult"))==0) {Time.point[y]<-y; p.attack.model[y,z]<-p.attack.model[y-1,z]; p.attack.mimic[y,z]<-p.attack.mimic[y-1,z]; enc.species[y,z]<-"None"; attacked[y,z]<-"None" ; next}
	# If there are adults, select one out of the pool of mimics and models
	enc<-sample(1:length(subset(c(mimic.pop[,1],model.pop[,1]),c(mimic.pop[,y+4],model.pop[,y+4])=="adult")),1)
	# Find that individual's phenotype
	enc.phenotype<-as.numeric(subset(c(mimic.pop[,3],model.pop[,3]),c(mimic.pop[,y+4],model.pop[,y+4])=="adult")[enc])
	# Find that individual's species
	enc.species[y,z]<-subset(c(mimic.pop[,4],model.pop[,4]),c(mimic.pop[,y+4],model.pop[,y+4])=="adult")[enc]
	# Find that individual's unique ID (not sure how this will be used yet, so it can probably be cut)
	enc.id<-subset(c(mimic.pop[,1],model.pop[,1]),c(mimic.pop[,y+4],model.pop[,y+4])=="adult")[enc]
	# Calculate predator learning decays the SD of the signal detection distribution from 3 to 1 (only when actually sampling, though)
	learning[y,z]<-2*exp(-0.05*length(subset(attacked[,z],attacked=="attacked")))+1
	# The predator guesses the species of the prey item on the basis of the signal detection model (driven by two overlapping distributions)
	if(runif(1)<dnorm(enc.phenotype,mean=35,sd=learning[y,z])/(dnorm(enc.phenotype,mean=35,sd=learning[y,z])+dnorm(enc.phenotype,mean=30,sd=learning[y,z]))) {guess<-"model"} else {guess<-"mimic"}
	
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
	#Time.point[y]<-y

	
	
}

# Output predator behaviour data	
#pred.behaviour<-data.frame(Time.point,p.attack.model,p.attack.mimic,enc.species,attacked,learning)
#pred.behaviour
#pred.mort.mimic<-numeric(length=pred.num)
#pred.mort.model<-numeric(length=pred.num)
#for(y in 1:pred.num){
#pred.mort.mimic<-length(subset(enc.species[,y],enc.species[,y]=="Mimic" & attacked[,y]=="attacked"))
#pred.mort.model<-nrow(subset(enc.species[,y],enc.species[,y]=="Model" & attacked[,y]=="attacked"))
#}
Mimic.predator.mortality[DEF,ABC,GHI]<-table(enc.species,attacked)[1,1]
Model.predator.mortality[DEF,ABC,GHI]<-table(enc.species,attacked)[2,1]


}
}
print(paste(DEF,"out of",length(mimic.mean.phenotype)))
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

