d = read.csv("pokemon.csv")
rows = nrow(d) * 0.7			#70% of data
simple_random_sample = d[sample(nrow(d), as.integer(rows)), ]	#random sample 

rows = nrow(d)
i = 1
systematic_sample = data.frame()
while(i < rows) {							#systematic sampling
	systematic_sample = rbind(systematic_sample, data.frame(d[i,]))		#choose every 3rd row
	i = i + 3
}

male = subset(d, Pr_Male >= 0.5)				#male pokemon
female = subset(d, Pr_Male < 0.5)				#female pokemon
nogen = subset(d, !hasGender)				#pokemon without gender
startified_sample = rbind(male[sample(nrow(male), as.integer(nrow(male) * 0.7)), ], female[sample(nrow(female), as.integer(nrow(female) * 0.7)), ]) #70% of each strata
startified_sample = rbind(startified_sample,nogen[sample(nrow(nogen), as.integer(nrow(nogen) * 0.7)), ])

ptype = unique(d$Type_1)			#get unique primary types
ptype = ptype[c(sample(length(ptype), length(ptype) * 0.7))]
clustered_sample = data.frame()
for(type in ptype) {							#get 70% of each type
	temp = subset(d, Type_1 == type)
	temp = temp[sample(nrow(temp), nrow(temp) * 0.7), ]
	clustered_sample = rbind(clustered_sample, temp)
}

pdf(file = "distributions.pdf")

plot(density(d$Total), col = 'black')
lines(density(simple_random_sample$Total), col = 'green')
lines(density(systematic_sample$Total), col = 'red')
lines(density(startified_sample$Total), col = 'pink')
lines(density(clustered_sample$Total), col = 'blue')
#hist((d$Total), main = 'Data : Total')
#hist((simple_random_sample$Total), main = 'Simple random sample : Total')
#hist((systematic_sample$Total), main = 'Systematic sample with k = 3 : Total')
#hist((startified_sample$Total), main = 'Stratified random sample : Total')
#hist((clustered_sample$Total),  main = 'Cluster sample : Total')

#simple random sampling is closest to population