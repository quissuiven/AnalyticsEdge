

# is it possible to predict the accuracy of oscars with any reasonable degree of accuracy?
# 5 actors, 1 win, 4 lose. 
# Outputs still 0 or 1, therefore discrete choice model

oscars = read.csv("oscars.csv")
#mode 1 2 3 4 5, 1st set of obs, 2nd set of obs
#ch determines whether win or lose, currently 2 or 1, want to make it 1 or 0
oscars$Ch <- 2-oscars$Ch

#getting sense of data before building model

tapply(oscars$Nom[oscars$PP==1],oscars$Ch[oscars$PP==1], mean)              #compute avg nominations for 1) movies nominated for best pic but nv win 2) movies nominated for best pic and won
tapply(oscars$Nom[oscars$PP==1],oscars$Ch[oscars$PP==1], var)             #mean not sufficient, need to calc var
t.test(oscars$Nom[oscars$PP==1&oscars$Ch==1],oscars$Nom[oscars$PP==1&oscars$Ch==0], alternative = c("greater"))  
              #null hypothesis: Avg no of nom for pic that won   <=   Avg no of nom for pic that did not win
               #alt hypothesis:                                   >
               #typically want to reject null, prove alt
               #therefore, since p value low. Reject null hypothesis. Significant that avg no of pic that won > avg no of nom for pic that did not win

#do best pic winners also receive noms for best director?
table(oscars$Dir[oscars$PP==1&oscars$Ch==1])               
which(oscars$Dir==0&oscars$PP==1&oscars$Ch==1)
oscars[362,]

table(oscars$Dir[oscars$PP==1&oscars$Ch==0])

#do best actor who won also receive nominations for best pic?
table(oscars$Pic[oscars$MM==1&oscars$Ch==1])   #43 ppl nominated, and won main actor also had pic nominated
      
table(oscars$Pic[oscars$MM==1&oscars$Ch==0])   #104 ppl nominated, but did not win main actor also had pic nominated

table(oscars$Pic[oscars$FF==1&oscars$Ch==1])  

(oscars$Year[oscars$FF==1&oscars$Ch==1])        #1968 appears twice, 2 best actress in that year
subset(oscars, Year == 1968&FF==1)

#do golden globe awards help predict the oscars?
table(oscars$Gdr[oscars$PP==1&oscars$Ch==1]+oscars$Gmc[oscars$PP==1&oscars$Ch==1])
39/57
table(oscars$Gd[oscars$DD==1&oscars$Ch==1])
