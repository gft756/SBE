
# 2 Some simulation in R

# 2.1
genoType = function(n = 1) {
  sample(c('ii', 'iI_A', 'iI_B', 'I_AI_A', 'I_AI_B', 'I_BI_B'),
         size = n,
         prob = c(1/9, 2/9, 2/9, 1/9, 2/9, 1/9),
         replace = TRUE)
}

#2.2
offspring = function(a, b) {
  
  if(a == 'ii' & b == 'iI_A' | a == 'iI_A' & b == 'ii') {
    sample(c('ii', 'iI_A'), prob = c(.5, .5), size = 1)
  }
  
  else if(a == 'ii' & b == 'iI_B' | a =='iI_B' & b == 'ii' ) {
    sample(c('ii, iI_B'), prob = c(.5, 0.5), size = 1)
  }
  
  else if(a == 'ii' & b == 'I_AI_A' | a =='I_AI_A' & b == 'ii' ){
    sample('I_A')
  }
  
  else if(a == 'ii' & b == 'I_AI_B' | a =='I_AI_B' & b == 'ii' ){
    sample(c('iI_A', 'iI_B'), prob = c(.5, .5), size = 1)
  }
  
  else if(a == 'ii' & b == 'I_BI_B' | a =='I_BI_B' & b == 'ii' ){
    sample('iI_B')
  }
  
  else if(a == 'iI_A' & b == 'iI_B' | a =='iI_B' & b == 'iI_A' ){
    sample(c('ii', 'iI_B', 'iI_A', 'I_AI_B'), prob = c(.25, .25, .25, 25), size = 1)
  }
  
  else if(a == 'iI_A' & b == 'I_AI_A' | a =='I_AI_A' & b == 'iI_A' ){
    sample( c('iI_A', 'I_AI_A'), prob = c(.5, .5), size = 1)
  }
  
  else if(a == 'iI_A' & b == 'I_AI_B' | a =='I_AI_B' & b == 'iI_A' ){
    sample(c('iI_A', 'iI_B', 'I_AI_A', 'I_AI_B'), prob = c(.25, .25, .25, .25), size = 1)
  }
  
  else if(a == 'iI_A' & b == 'I_BI_B' | a =='I_BI_B' & b == 'iI_A' ){
    sample(c('iI_B', 'I_AI_B'), prob = c(.5, .5), size = 1)
  }
  
  else if(a == 'iI_B' & b == 'I_AI_A' | a =='I_AI_A' & b == 'iI_B' ){
    sample(c('iI_A', 'I_AI_B'), prob = c(.5, .5), prob = c(.5, .5), size = 1)
  }
  
  else if(a == 'iI_B' & b == 'I_AI_B' | a =='I_AI_B' & b == 'iI_B' ){
    sample(c('iI_A', 'iI_B', 'I_AI_B', 'I_BI_B'), prob = c(.25, .25, .25, .25), size = 1) 
  }
  
  else if(a == 'iI_B' & b == 'I_BI_B' | a =='I_BI_B' & b == 'iI_B' ){
    sample(c('iI_B', 'I_BI_B'), prob = c(.5, .5), size = 1)
  }
  
  else if(a == 'I_AI_A' & b == 'I_AI_B' | a =='I_AI_B' & b == 'I_AI_A' ){
    sample( c('I_AI_A, I_AI_B'), prob = c(.5, .5), size = 1)
  }
  
  else if(a == 'I_AI_A' & b == 'I_BI_B' | a =='I_BI_B' & b == 'I_AI_A' ){
    sample('I_AI_B')
  }
  
  else if(a == 'I_AI_B' & b == 'I_BI_B' | a =='I_BI_B' & b == 'I_AI_A' ){
    sample(c('I_AI_B', 'I_BI_B'), prob = c(.5, .5), size = 1)
  }
  
  else {
    "not a genotype"
  }
}


phenotype = function(x){
  
}




# 3: A case study of neuronal data

isidata = read.table("neuronspikes.txt", col.names = "isi")
x = isidata$isi


# 3.1

par(mfrow = c(2,2))

hist(x,
     breaks = 50,
     probability = T,
     main = "Histogram of isi, breaks = 50", xlab = "isi",
     col = "seagreen1")

hist(x,
     breaks = 40,
     probability = T,
     main = "Histogram of isi, breaks = 40", xlab = "isi",
     col = "cyan1")

hist(x,
     breaks = 30,
     probability = T,
     main = "Histogram of isi, breaks = 30", xlab = "isi",
     col = "plum1")

hist(x,
     breaks = 20,
     probability = T,
     main = "Histogram of isi, breaks = 20", xlab = "isi",
     col = "orangered")


# 3.2

par(mfrow = c(1,1))


hist(isidata$isi,
     breaks = 50,
     probability = T,
     main = "Histogram of isi", xlab = "isi")


first_plot <- TRUE
for (rate in  seq(0.1, to = 1.5, 0.1)){
  curve(dexp(x, rate = rate), add = first_plot, 
        from = 0, to = 6,
        col= 10*rate)
}


# it appears like rate = 1.5 approximates the histogram the best. 



# 3.3 

pexp(1.5, rate = 1.5) # 3.3.1

pexp(1.5, rate = 1.5) - pexp(0.5, rate = 1.5) # 3.3.2




##############################

# 4: Brain cell database

cells = read.csv("cell_types.csv", na.strings = "")


# 4.1

# na.strings = "" replaces all empty (stripped for whitespaces) strings with NA values.

# 4.2

table(cells$donor__species) # 4.2.1


table(cells$donor__species)[2] / sum(table(cells$donor__species)) # 4.2.2


# 4.3

hist(cells$ef__peak_t_ramp, probability = T, xlab = 'ef__peak_t_ramp',
     main = 'ef__peak_t_ramp') # 4.3.1


# 4.3.2
par(mfrow = c(1,2))

homo_sapiens = cells$donor__species == 'Homo Sapiens'
Mus_musculus = cells$donor__species == 'Mus musculus' 

hist(cells$ef__peak_t_ramp[homo_sapiens], probability = T, xlab = 'ef__peak_t_ramp',
     main = 'Homo sapiens ef__peak_t_ramp') 

hist(cells$ef__peak_t_ramp[Mus_musculus], probability = T, xlab = 'ef__peak_t_ramp',
     main = 'Mus_musculus ef__peak_t_ramp') 


# 4.4

par(mfrow = c(1,1))

hist(cells$ef__peak_t_ramp, probability = T, xlab = 'ef__peak_t_ramp',
       main = 'ef__peak_t_ramp') # 4.3.1
  
first_plot <- TRUE
for (meanlog in  seq(1, to = 3, 0.5)){
  curve(dlnorm(x, sdlog = 0.6, meanlog = meanlog), add = first_plot, 
        from = 0.5, to = 30,
        col= 10*meanlog)
  }


# 4.5

# It appears like meanlog = 1.5 approximates the histogram the best. 

# 4.6

hist(cells$ef__peak_t_ramp, probability = T, xlab = 'ef__peak_t_ramp',
     main = 'ef__peak_t_ramp') # 4.3.1


curve(dlnorm(x, sdlog = 0.6, meanlog = 1.5), add = T, 
      from = 0.5, to = 30,
      col= 'royalblue2',
      lwd = 2)

# 4.7


homo_sapiens = subset(cells, donor__species == 'Homo Sapiens')

table(homo_sapiens$donor__sex) # frequency of male and females of homo sapiens



