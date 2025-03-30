# Matthew L. Pergolski
# IST 772
# Dr. Block

############################################################
  
  #The homework for week two is exercises 1 and 2 on page 35, as well as problems 6, 7, and 8 on page 36.
  
  #1 ############################################################
  
        #flipping coin one trial of 9 occurences
        rbinom(1, 9, .5)
        table(rbinom(1, 9, .5))
   
        #repeating process 100,000 times
        table(rbinom(100000, 9, .5))
  
  
  #2  ############################################################
        
        
        #generating barplot
        barplot(table(rbinom(100000, 9, .5)))
        
        #generating probabilities
        table(rbinom(100000, 9, .5))/100000
        barplot(table(rbinom(100000, 9, .5))/100000)
        cumsum(table(rbinom(100000, 9, .5))/100000)
  
        
  #6  ############################################################
          
        #making contingency matrix without knowing cell values
        students <- matrix(c(0,0,0,0), ncol = 2, byrow =  TRUE)
        students
        colnames(students) <- c('High School', 'College')
        students
        rownames(students) <- c('PASS', 'FAIL')
        students
        
        #update contingency table with know values -- 3 college students failed
        students <- matrix(c(33,47,17,3), ncol = 2, byrow =  TRUE)
        students
        colnames(students) <- c('High School', 'College')
        students
        rownames(students) <- c('PASS', 'FAIL')
        students
        
        #marginal totals
        margin.table(students)
        margin.table(students, 1)
        margin.table(students, 2)
        
        #create probability table and perform marginal totals
        prob.students <- as.table(students/margin.table(students))
        prob.students
        margin.table(prob.students)
        margin.table(prob.students, 1)
        margin.table(prob.students, 2)
        
        #pass rate for H.S. students (normalize values)
        prob.students
        prob.students[,1]
        prob.students[,1]/sum(prob.students[,1])
        
  
  #7  ############################################################
      
      #creating homes matrix 
      homes <- matrix(c(93933, 2, 5996, 69), ncol = 2, byrow = TRUE)
      homes 
      
      #column and row names
      colnames(homes) <- c('Not Repossessed', 'Repossessed')  
      homes  
      rownames(homes) <- c('PASS', 'FAIL')      
      homes 
      
      #margin totals (non-probabilities)
      margin.table(homes)
      margin.table(homes, 1)
      margin.table(homes, 2)
  
      #margin totals (with probabilities)
      homes.prob.table <- homes/margin.table(homes) 
      homes.prob.table  
      margin.table(homes.prob.table)
      margin.table(homes.prob.table, 1)
      margin.table(homes.prob.table, 2) 
      homes.prob.table
      
      #calculating answer to question -- normalizing values
      homes.prob.table
      homes.prob.table[,1]
      sum(homes.prob.table[,1])
      homes.prob.table[,1]/sum(homes.prob.table[,1])
   
      
  #8  ############################################################   
      
      #using same tables as above, only normalizing different values
      homes.prob.table
      homes.prob.table[2,]
      sum(homes.prob.table[2,])
      homes.prob.table[2,]/sum(homes.prob.table[2,])
      
      
