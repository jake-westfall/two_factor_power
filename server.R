library(shiny)
library(minqa)


# design schematics -------------------------------------------------------

shinyServer(function(input, output) {

  # function to display design schematic
  getTable <- reactive({
    AB <- matrix("AB", nrow=3, ncol=3)
    A <- matrix("A", nrow=3, ncol=3)
    B <- matrix("B", nrow=3, ncol=3)
    mat <- matrix("", nrow=3, ncol=3)
    if(input$design=="CCC"){
      tab <- cbind(rbind(AB, AB), rbind(AB, AB))
      rownames(tab) <- paste0("Participant", 1:6)
      colnames(tab) <- paste0("Target", 1:6)
    } else if(input$design=="CNC"){
      tab <- cbind(rbind(A, A), rbind(B, B))
      rownames(tab) <- paste0("Participant", 1:6)
      colnames(tab) <- paste0("Target", 1:6)
    } else if(input$design=="NCC"){
      tab <- rbind(cbind(A, A), cbind(B, B))
      rownames(tab) <- paste0("Participant", 1:6)
      colnames(tab) <- paste0("Target", 1:6)
    } else if(input$design=="NNC"){
      tab <- rbind(cbind(A, mat), cbind(mat, B))
      rownames(tab) <- paste0("Participant", 1:6)
      colnames(tab) <- paste0("Target", 1:6)
    } else if(input$design=="CCNp"){
      ab <- cbind("AB","AB","AB")
      x <- cbind("","","")
      tab <- cbind(rbind(ab,x,x), rbind(x,ab,x), rbind(x,x,ab))
      rownames(tab) <- paste0("Participant", 1:3)
      colnames(tab) <- paste0("Target", 1:9)
    } else if(input$design=="CNNp"){
      ab <- cbind("A","A","B","B")
      x <- cbind("","","","")
      tab <- cbind(rbind(ab,x,x), rbind(x,ab,x), rbind(x,x,ab))
      rownames(tab) <- paste0("Participant", 1:3)
      colnames(tab) <- paste0("Target", 1:12)
    } else if(input$design=="NNNp"){
      a <- cbind("A","A","A")
      b <- cbind("B","B","B")
      x <- cbind("","","")
      tab <- cbind(rbind(a,x,x,x),rbind(x,a,x,x),rbind(x,x,b,x),rbind(x,x,x,b))
      rownames(tab) <- paste0("Participant", 1:4)
      colnames(tab) <- paste0("Target", 1:12)
    } else if(input$design=="CCNt"){
      ab <- rbind("AB","AB","AB")
      x <- rbind("","","")
      tab <- rbind(cbind(ab,x,x), cbind(x,ab,x), cbind(x,x,ab))
      rownames(tab) <- paste0("Participant", 1:9)
      colnames(tab) <- paste0("Target", 1:3)
    } else if(input$design=="NCNt"){
      ab <- rbind("A","A","B","B")
      x <- rbind("","","","")
      tab <- rbind(cbind(ab,x,x), cbind(x,ab,x), cbind(x,x,ab))
      rownames(tab) <- paste0("Participant", 1:12)
      colnames(tab) <- paste0("Target", 1:3)
    } else if(input$design=="NNNt"){
      a <- rbind("A","A","A")
      b <- rbind("B","B","B")
      x <- rbind("","","")
      tab <- rbind(cbind(a,x,x,x),cbind(x,a,x,x),cbind(x,x,b,x),cbind(x,x,x,b))
      rownames(tab) <- paste0("Participant", 1:12)
      colnames(tab) <- paste0("Target", 1:4)
    } else if(input$design=="R(CCC)"){
      tab <- cbind(rbind(AB,mat,mat),rbind(mat,AB,mat),rbind(mat,mat,AB))
      rownames(tab) <- paste0("Participant", 1:9)
      colnames(tab) <- paste0("Target", 1:9)
    } else if(input$design=="R(CNC)"){
      ab <- cbind("A","A","B","B")
      x <- cbind("","","","")
      tab <- cbind(rbind(ab,ab,ab,x,x,x,x,x,x),
                   rbind(x,x,x,ab,ab,ab,x,x,x),
                   rbind(x,x,x,x,x,x,ab,ab,ab))
      rownames(tab) <- paste0("Participant", 1:9)
      colnames(tab) <- paste0("Target", 1:12)
    } else if(input$design=="R(NCC)"){
      ab <- rbind("A","A","B","B")
      x <- rbind("","","","")
      tab <- rbind(cbind(ab,ab,ab,x,x,x,x,x,x),
                   cbind(x,x,x,ab,ab,ab,x,x,x),
                   cbind(x,x,x,x,x,x,ab,ab,ab))
      rownames(tab) <- paste0("Participant", 1:12)
      colnames(tab) <- paste0("Target", 1:9)
    } else if(input$design=="R(NNC)"){
      Amat <- cbind(rbind(A[-1,-1],mat[-1,-1],mat[-1,-1]),
                    rbind(mat[-1,-1],A[-1,-1],mat[-1,-1]),
                    rbind(mat[-1,-1],mat[-1,-1],A[-1,-1]))
      Bmat <- cbind(rbind(B[-1,-1],mat[-1,-1],mat[-1,-1]),
                    rbind(mat[-1,-1],B[-1,-1],mat[-1,-1]),
                    rbind(mat[-1,-1],mat[-1,-1],B[-1,-1]))
      tab <- cbind(rbind(Amat,"","","","","",""),rbind("","","","","","",Bmat))
      rownames(tab) <- paste0("Participant", 1:12)
      colnames(tab) <- paste0("Target", 1:12)
    } else if(input$design=="Counterbalanced"){
      tab <- rbind(cbind(A, B), cbind(B, A))
      rownames(tab) <- paste0("Participant", 1:6)
      colnames(tab) <- paste0("Target", 1:6)
    } 
    data.frame(tab)
  })
  
  # produce design schematic output
  output$designTab <- renderTable({
    tab <- getTable()
    tab
  })


# output model fitting code -----------------------------------------------

  output$R_code <- renderPrint({
    if(input$design %in% c("CCC","R(CCC)")){
      cat(
"model <- lmer(y ~ condition + (condition|participant) + (condition|target) + (1|participant:target), data=myData)
summ <- summary(model)
summ")
    } else if(input$design %in% c("CNC","R(CNC)")){
      cat(
"model <- lmer(y ~ condition + (condition|participant) + (1|target), data=myData)
summ <- summary(model)
summ")
    } else if(input$design %in% c("NCC","R(NCC)")){
      cat(
"model <- lmer(y ~ condition + (1|participant) + (condition|target), data=myData)
summ <- summary(model)
summ")
    } else if(input$design %in% c("NNC","R(NNC)")){
      cat(
"model <- lmer(y ~ condition + (1|participant) + (1|target), data=myData)
summ <- summary(model)
summ")
    } else if(input$design=="CCNp"){
      cat(
"model <- lmer(y ~ condition + (condition|participant) + (1|target), data=myData)
summ <- summary(model)
summ")
    } else if(input$design=="CNNp"){
      cat(
"model <- lmer(y ~ condition + (condition|participant), data=myData)
summ <- summary(model)
summ")
    } else if(input$design=="NNNp"){
      cat(
"model <- lmer(y ~ condition + (1|participant), data=myData)
summ <- summary(model)
summ")
    } else if(input$design=="CCNt"){
      cat(
"model <- lmer(y ~ condition + (1|participant) + (condition|target), data=myData)
summ <- summary(model)
summ")
    } else if(input$design=="NCNt"){
      cat(
"model <- lmer(y ~ condition + (condition|target), data=myData)
summ <- summary(model)
summ")
    } else if(input$design=="NNNt"){
      cat(
"model <- lmer(y ~ condition + (1|target), data=myData)
summ <- summary(model)
summ")
    } else if(input$design=="Counterbalanced"){
      cat(
"model <- lmer(y ~ condition + (condition|participant) + (condition|target), data=myData)
summ <- summary(model)
summ")
    } 
  })
    
  output$SAS_code <- renderPrint({
    if(input$design %in% c("CCC","R(CCC)")){
      cat(
"proc mixed covtest data=myData;
class participant target;
model y=condition/solution ddfm=satt;
random intercept condition/sub=participant type=un;
random intercept condition/sub=target type=un;
random intercept/sub=participant*target;
run;")
    } else if(input$design %in% c("CNC","R(CNC)")){
      cat(
"proc mixed covtest data=myData;
class participant target;
model y=condition/solution ddfm=satt;
random intercept condition/sub=participant type=un;
random intercept/sub=target;
run;")
    } else if(input$design %in% c("NCC","R(NCC)")){
      cat(
"proc mixed covtest data=myData;
class participant target;
model y=condition/solution ddfm=satt;
random intercept/sub=participant;
random intercept condition/sub=target type=un;
run;")
    } else if(input$design %in% c("NNC","R(NNC)")){
      cat(
"proc mixed covtest data=myData;
class participant target;
model y=condition/solution ddfm=satt;
random intercept/sub=participant;
random intercept/sub=target;
run;")
    } else if(input$design=="CCNp"){
      cat(
"proc mixed covtest data=myData;
class participant target;
model y=condition/solution ddfm=satt;
random intercept condition/sub=participant type=un;
random intercept/sub=target;
run;")
    } else if(input$design=="CNNp"){
      cat(
"proc mixed covtest data=myData;
class participant target;
model y=condition/solution ddfm=satt;
random intercept condition/sub=participant type=un;
run;")
    } else if(input$design=="NNNp"){
      cat(
        "proc mixed covtest data=myData;
class participant target;
model y=condition/solution ddfm=satt;
random intercept/sub=participant;
run;")
    } else if(input$design=="CCNt"){
      cat(
"proc mixed covtest data=myData;
class participant target;
model y=condition/solution ddfm=satt;
random intercept/sub=participant;
random intercept condition/sub=target type=un;
run;")
    } else if(input$design=="NCNt"){
      cat(
"proc mixed covtest data=myData;
class participant target;
model y=condition/solution ddfm=satt;
random intercept condition/sub=target type=un;
run;")
    } else if(input$design=="NNNt"){
      cat(
"proc mixed covtest data=myData;
class participant target;
model y=condition/solution ddfm=satt;
random intercept/sub=target;
run;")
    } else if(input$design=="Counterbalanced"){
      cat(
"proc mixed covtest data=myData;
class participant target;
model y=condition/solution ddfm=satt;
random intercept condition/sub=participant type=un;
random intercept condition/sub=target type=un;
run;")
    } 
  })
    
  output$SPSS_code <- renderPrint({
    if(input$design %in% c("CCC","R(CCC)")){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept condition | subject(participant) covtype(un)
/random=intercept condition | subject(target) covtype(un)
/random=intercept | subject(participant*target).")
    } else if(input$design %in% c("CNC","R(CNC)")){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept condition | subject(participant) covtype(un)
/random=intercept | subject(target).")
    } else if(input$design %in% c("NCC","R(NCC)")){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept | subject(participant)
/random=intercept condition | subject(target) covtype(un).")
    } else if(input$design %in% c("NNC","R(NNC)")){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept | subject(participant)
/random=intercept | subject(target).")
    } else if(input$design=="CCNp"){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept condition | subject(participant) covtype(un)
/random=intercept | subject(target).")
    } else if(input$design=="CNNp"){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept condition | subject(participant) covtype(un).")
    } else if(input$design=="NNNp"){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept | subject(participant).")
    } else if(input$design=="CCNt"){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept | subject(participant)
/random=intercept condition | subject(target) covtype(un).")
    } else if(input$design=="NCNt"){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept condition | subject(target) covtype(un).")
    } else if(input$design=="NNNt"){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept | subject(target).")
    } else if(input$design=="Counterbalanced"){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept condition | subject(participant) covtype(un)
/random=intercept condition | subject(target) covtype(un).")
    } 
  })

# write text labels for input fields --------------------------------------


  output$get_d <- renderUI({ 
    textInput("d", value=.5, label=ifelse(input$type=="Standardized",
              "Effect size d:", "Mean difference:"))
  })
  output$get_E <- renderUI({ 
    textInput("E", value=.3, label=ifelse(input$type=="Standardized",
              "Residual VPC:", "Residual variance:"))
  })
  output$get_S <- renderUI({ 
    textInput("S", value=.2, label=ifelse(input$type=="Standardized",
              "Target intercept VPC:", "Target intercept variance:"))
  })
  output$get_P <- renderUI({ 
    textInput("P", value=.2, label=ifelse(input$type=="Standardized",
              "Participant intercept VPC:", "Participant intercept variance:"))
  })
  output$get_PS <- renderUI({ 
    textInput("PS", value=.1, label=ifelse(input$type=="Standardized",
              "Participant-by-Target VPC:", "Participant-by-Target variance:"))
  })
  output$get_SC <- renderUI({ 
    textInput("SC", value=.1, label=ifelse(input$type=="Standardized",
              "Target slope VPC:", "Target slope variance:"))
  })
  output$get_PC <- renderUI({ 
    textInput("PC", value=.1, label=ifelse(input$type=="Standardized",
              "Participant slope VPC:", "Participant slope variance:"))
  })
  

# grab input, define power functions --------------------------------------

  output$ans <- renderPrint({
    if(input$solve > 0){
      
      # grab input values without updating form!
      design <- isolate(input$design)
      type <- isolate(input$type)
      d <- isolate(input$d)
      code <- isolate(input$code)
      E <- isolate(input$E)
      S <- isolate(input$S)
      P <- isolate(input$P)
      PS <- isolate(input$PS)
      SC <- isolate(input$SC)
      PC <- isolate(input$PC)
      p <- isolate(input$p)
      q <- isolate(input$q)
      b <- isolate(input$b)
      power <- isolate(input$power)
      
      # define function to return power for chosen design
      if(design=="CCC"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b=NULL){
          if(type=="Standardized") code <- 1
          t <- d/sqrt(2*(E/p/q + 2*code^2*SC/q + 2*code^2*PC/p))
          MS <- c(e = E, sc = E + p*SC*code^2, pc = E + q*PC*code^2)
          DF <- (MS["pc"] + MS["sc"] - MS["e"])^2/
            (MS["e"]^2/(p-1)/(q-1) + MS["sc"]^2/(q-1) + MS["pc"]^2/(p-1))
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + PS + S + P + code^2*SC + code^2*PC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
              E=E/v, P=P/v, S=S/v, PC=code^2*PC/v, SC=code^2*SC/v, PS=PS/v)
          return(ans)
        }
      } else if(design=="CNC"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b=NULL){
          if(type=="Standardized") code <- 1
          E <- E + PS
          S <- S + code^2*SC
          t <- d/2/sqrt(E/p/q + S/q + code^2*PC/p)
          MS <- c(e = E, s = E + p*S, pc = E + q/2*PC*code^2)
          DF <- (MS["pc"] + MS["s"] - MS["e"])^2/
            (MS["e"]^2/(p-1)/(q-2) + MS["s"]^2/(q-2) + MS["pc"]^2/(p-1))
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S + P + code^2*PC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
              E=E/v, P=P/v, S=S/v, PC=code^2*PC/v)
          return(ans)
        }
      } else if(design=="NCC"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b=NULL){
          if(type=="Standardized") code <- 1
          E <- E + PS
          P <- P + code^2*PC
          t <- d/2/sqrt(E/p/q + code^2*SC/q + P/p)
          MS <- c(e = E, sc = E + p/2*SC*code^2, p = E + q*P)
          DF <- (MS["p"] + MS["sc"] - MS["e"])^2/
            (MS["e"]^2/(p-2)/(q-1) + MS["sc"]^2/(q-1) + MS["p"]^2/(p-2))
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S + P + code^2*SC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
              E=E/v, P=P/v, S=S/v, SC=code^2*SC/v)
          return(ans)
        }
      } else if(design=="NNC"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b=NULL){
          if(type=="Standardized") code <- 1
          E <- E + PS
          S <- S + code^2*SC
          P <- P + code^2*PC
          t <- d/2/sqrt(2*E/p/q + S/q + P/p)
          MS <- c(e = E, s = E + p/2*S, p = E + q/2*P)
          DF <- (MS["p"] + MS["s"] - MS["e"])^2/
            (MS["e"]^2/(p-2)/(q-2) + MS["s"]^2/(q-2) + MS["p"]^2/(p-2))
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S + P
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
              E=E/v, P=P/v, S=S/v)
          return(ans)
        }
      } else if(design=="CCNp"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b=NULL){
          if(type=="Standardized") code <- 1
          E <- E + PS + code^2*SC
          t <- d/2/sqrt(E/q/2 + code^2*PC/p)
          DF <- p - 1
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S + P + code^2*PC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
            E=E/v, P=P/v, S=S/v, PC=code^2*PC/v, SC=code^2*SC/v, PS=PS/v)
          return(ans)
        }
      } else if(design=="CNNp"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b=NULL){
          if(type=="Standardized") code <- 1
          E <- E + PS + S + code^2*SC
          t <- d/2/sqrt(E/q/2 + code^2*PC/p)
          DF <- p - 1
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + P + code^2*PC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
            E=E/v, P=P/v, S=S/v, PC=code^2*PC/v, SC=code^2*SC/v, PS=PS/v)
          return(ans)
        }
      } else if(design=="NNNp"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b=NULL){
          if(type=="Standardized") code <- 1
          E <- E + PS + S + code^2*SC
          P <- P + code^2*PC
          t <- d/2/sqrt(E/q + P/p)
          DF <- p - 2
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + P
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
            E=E/v, P=P/v, S=S/v, PC=code^2*PC/v, SC=code^2*SC/v, PS=PS/v)
          return(ans)
        }
      } else if(design=="CCNt"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b=NULL){
          if(type=="Standardized") code <- 1
          E <- E + PS + code^2*PC
          t <- d/2/sqrt(E/p/2 + code^2*SC/q)
          DF <- q - 1
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S + P + code^2*SC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
            E=E/v, P=P/v, S=S/v, PC=code^2*PC/v, SC=code^2*SC/v, PS=PS/v)
          return(ans)
        }
      } else if(design=="NCNt"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b=NULL){
          if(type=="Standardized") code <- 1
          E <- E + PS + P + code^2*PC
          t <- d/2/sqrt(E/p/2 + code^2*SC/q)
          DF <- q - 1
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S + code^2*SC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
            E=E/v, P=P/v, S=S/v, PC=code^2*PC/v, SC=code^2*SC/v, PS=PS/v)
          return(ans)
        }
      } else if(design=="NNNt"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b=NULL){
          if(type=="Standardized") code <- 1
          E <- E + PS + P + code^2*PC
          S <- S + code^2*SC
          t <- d/2/sqrt(E/p + S/q)
          DF <- q - 2
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
            E=E/v, P=P/v, S=S/v, PC=code^2*PC/v, SC=code^2*SC/v, PS=PS/v)
          return(ans)
        }
      } else if(design=="R(CCC)"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b){
          if(type=="Standardized") code <- 1
          t <- d/2/sqrt(b*E/2/p/q + code^2*SC/q + code^2*PC/p)
          MS <- c(e = E, sc = E + p/b*SC*code^2, pc = E + q/b*PC*code^2)
          DF <- (MS["pc"] + MS["sc"] - MS["e"])^2/
            (MS["e"]^2/(p-b)/(q-b) + MS["sc"]^2/(q-b) + MS["pc"]^2/(p-b))
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + PS + S + P + code^2*SC + code^2*PC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
            E=E/v, P=P/v, S=S/v, PC=code^2*PC/v, SC=code^2*SC/v, PS=PS/v)
          return(ans)
        }
      } else if(design=="R(CNC)"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b){
          if(type=="Standardized") code <- 1
          E <- E + PS
          S <- S + code^2*SC
          t <- d/2/sqrt(b*E/p/q + S/q + code^2*PC/p)
          MS <- c(e = E, s = E + p/b*S, pc = E + q/2/b*PC*code^2)
          DF <- (MS["pc"] + MS["s"] - MS["e"])^2/
            (MS["e"]^2/(p-b)/(q-2*b) + MS["s"]^2/(q-2*b) + MS["pc"]^2/(p-b))
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S + P + code^2*PC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
            E=E/v, P=P/v, S=S/v, PC=code^2*PC/v)
          return(ans)
        }
      } else if(design=="R(NCC)"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b){
          if(type=="Standardized") code <- 1
          E <- E + PS
          P <- P + code^2*PC
          t <- d/2/sqrt(b*E/p/q + code^2*SC/q + P/p)
          MS <- c(e = E, sc = E + p/2*SC*code^2, p = E + q*P)
          DF <- (MS["p"] + MS["sc"] - MS["e"])^2/
            (MS["e"]^2/(p-2*b)/(q-b) + MS["sc"]^2/(q-b) + MS["p"]^2/(p-2*b))
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S + P + code^2*SC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
            E=E/v, P=P/v, S=S/v, SC=code^2*SC/v)
          return(ans)
        }
      } else if(design=="R(NNC)"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b){
          if(type=="Standardized") code <- 1
          E <- E + PS
          S <- S + code^2*SC
          P <- P + code^2*PC
          t <- d/2/sqrt(2*b*E/p/q + S/q + P/p)
          MS <- c(e = E, s = E + p/2*S, p = E + q/2*P)
          DF <- (MS["p"] + MS["s"] - MS["e"])^2/
            (MS["e"]^2/(p-2*b)/(q-2*b) + MS["s"]^2/(q-2*b) + MS["p"]^2/(p-2*b))
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S + P
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
            E=E/v, P=P/v, S=S/v)
          return(ans)
        }
      } else if(design=="Counterbalanced"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q, b=NULL){
          if(type=="Standardized") code <- 1
          E <- E + PS
          t <- d/2/sqrt(E/p/q + code^2*SC/q + code^2*PC/p)
          MS <- c(e = E, sc = E + p/2*SC*code^2, pc = E + q/2*PC*code^2)
          DF <- (MS["pc"] + MS["sc"] - MS["e"])^2/
            (MS["e"]^2/(p-2)/(q-2) + MS["sc"]^2/(q-2) + MS["pc"]^2/(p-2))
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S + P + code^2*SC + code^2*PC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
            E=E/v, P=P/v, S=S/v, PC=code^2*PC/v, SC=code^2*SC/v)
          return(ans)
        }
      } 

# perform error checking on user input ------------------------------------
      
      # verify that only 1 variable is selected to solve for
      vals <- list(d=d,code=code,E=E,S=S,P=P,PS=PS,SC=SC,PC=PC,p=p,q=q,b=b,power=power)
      choice <- unlist(lapply(toupper(vals), "==", "X"))
      if(sum(choice) < 1) stop("No variable selected to solve for.")
      if(sum(choice) > 1) stop("Multiple variables selected to solve for.")
      
      # convert all the other variables to numeric
      choice <- names(vals)[choice]
      vals <- vals[-which(names(vals)==choice)]
      if(any(is.na(lapply(vals, function(x) as.numeric(x))))){
        stop("Non-numeric input in at least one of the provided parameters.")
      } 
      vals <- lapply(vals, as.numeric)
      
      # check that power is in [.05, 1)
      if("power" %in% names(vals)){
        if(vals$power < .05 | vals$power >= 1){
          stop("Power must be greater than or equal to .05 and less than 1.")
        }
      }
      
      # check that variance components are non-negative
      VPCs <- intersect(names(vals), c("E", "S", "P", "PS", "SC", "PC"))
      if(any(unlist(lapply(vals[VPCs], function(x) x < 0)))){
        stop("At least one variance component is negative.")
      }
      
      # check that no VPCs exceed 1
      if(type=="Standardized"){
        if(any(unlist(lapply(vals[VPCs], function(x) x > 1)))){
          stop("At least one VPC exceeds 1.")
        }
      }
      
      # check that all VPCs sum to 1 (approximately)
      if(type=="Standardized" & !(choice %in% c("E", "S", "P", "PS", "SC", "PC"))){
        total <- sum(unlist(vals[VPCs]))
        if(total < .995 | total > 1.005){
          stop(paste("With standardized input, all of the VPCs must sum to 1.
VPCs currently sum to", total))
        }
      }

# solve for selected parameter --------------------------------------------

      # solve
      if(choice=="d"){
        # define squared-error cost function
        cost <- function(test_d){
          (powFunc(test_d, vals$code, vals$E, vals$S, vals$P, vals$PS,
                   vals$SC, vals$PC, vals$p, vals$q) - vals$power)^2
        }
        # find d by minimizing cost function
        result <- bobyqa(.02, cost, lower=0, upper=Inf)
        # get answer at parameter estimate
        ans <- powFunc(result$par,vals$code,vals$E,vals$S,vals$P,vals$PS,
                       vals$SC,vals$PC,vals$p,vals$q)
        # display additional info
        info <- attr(ans, "extra")
        class(info) <- "info"
        output$info <- renderPrint({info})
        # display technical output
        class(result) <- "debug"
        output$debug <- renderPrint({result})
        # return solution
        if(type=="Standardized") {
          return(round(c("Minimum effect size d:" = result$par),3))
        } else return(round(c("Minimum mean difference:" = result$par),3))
      } else if(choice=="code"){ 
        stop("Cannot solve for contrast codes.")
      } else if(choice=="E"){
        stop("Cannot solve for variance components.")
      } else if(choice=="S"){
        stop("Cannot solve for variance components.")
      } else if(choice=="P"){
        stop("Cannot solve for variance components.")
      } else if(choice=="PS"){
        stop("Cannot solve for variance components.")
      } else if(choice=="SC"){
        stop("Cannot solve for variance components.")
      } else if(choice=="PC"){
        stop("Cannot solve for variance components.")
      } else if(choice=="p"){
        # define squared-error cost function
        cost <- function(test_p){
          (powFunc(vals$d, vals$code, vals$E, vals$S, vals$P, vals$PS,
                   vals$SC, vals$PC, test_p, vals$q) - vals$power)^2
        }
        # find p by minimizing cost function
        result <- bobyqa(4, cost, lower=1, upper=Inf)
        # display technical output
        class(result) <- "debug"
        output$debug <- renderPrint({result})
        # check for bad estimates
        if(result$ierr > 0) {
          output$info <- renderText({NULL})
          return(noquote(c(" "=
            "Power level not attainable even with infinite participants.")))
        }
        else {
          # get answer at parameter estimate
          ans <- powFunc(vals$d,vals$code,vals$E,vals$S,vals$P,vals$PS,
                         vals$SC,vals$PC,result$par,vals$q)
          # display additional info
          info <- attr(ans, "extra")
          class(info) <- "info"
          output$info <- renderPrint({info})
          # return solution
          return(round(c("Minimum number of participants:"=result$par),1))
        }
      } else if(choice=="q"){
        # define squared-error cost function
        cost <- function(test_q){
          (powFunc(vals$d, vals$code, vals$E, vals$S, vals$P, vals$PS,
                   vals$SC, vals$PC, vals$p, test_q) - vals$power)^2
        }
        # find q by minimizing cost function
        result <- bobyqa(4, cost, lower=1, upper=Inf)
        # display technical output
        class(result) <- "debug"
        output$debug <- renderPrint({result})
        # check for bad estimates
        if(result$ierr > 0) {
          output$info <- renderText({NULL})
          return(noquote(c(" "=
            "Power level not attainable even with infinite participants.")))
        }
        else {
          # get answer at parameter estimate
          ans <- powFunc(vals$d,vals$code,vals$E,vals$S,vals$P,vals$PS,
                         vals$SC,vals$PC,vals$p,result$par)
          # display additional info
          info <- attr(ans, "extra")
          class(info) <- "info"
          output$info <- renderPrint({info})
          # return solution
          return(round(c("Minimum number of targets:"=result$par),1))
        }
      } else if(choice=="b"){
        stop("Cannot solve for number of replications (yet).")
      } else if(choice=="power"){
        ans <- powFunc(vals$d,vals$code,vals$E,vals$S,vals$P,vals$PS,
                       vals$SC,vals$PC,vals$p,vals$q,vals$b)
        # display additional info
        info <- attr(ans, "extra")
        class(info) <- "info"
        output$info <- renderPrint({info})
        # display technical output
        output$debug <- renderText({NULL})
        # return solution
        return(round(c("Power:"= ans),3))
      }
    } # end if(input$solve>0)
  }) # end assignment to output$ans and renderPrint()
}) # end call to shinyServer()
