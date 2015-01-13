shinyServer(function(input, output) {
  
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    
    switch(input$input_type,
           "rock-paper-scissors" = radioButtons("dynamic", "choose you want",
                                                choices = c("rock"     = 2,
                                                            "paper"    = 3,
                                                            "scissors" = 1),
                                                selected = 2),
           "dice Big-Small" = radioButtons("dynamic", "choose game way",
                                                choices = c("Big"   = 1,
                                                            "Small" = 2),
                                                selected = 1),
            "Guess Number" = radioButtons("dynamic", "choose you want",
                                                choices = c("1" = 1,
                                                            "2" = 2,
                                                            "3" = 3,
                                                            "4" = 4,
                                                            "5" = 5 ),
                                                 selected = 1 ),
                                      
           "bat-tiger-hen-bug" = radioButtons("dynamic", "choose you want",
                                                choices = c("bat"   = 1,
                                                            "tiger" = 2,
                                                            "hen"   = 3,
                                                            "bug"   = 4),
                                                  selected = 1 ),
           "rock-paper-scissors(probability)" = c(selectInput("dynamic","choose you want",
                                                  choices = c("rock"     = 2,
                                                              "paper"    = 3,
                                                              "scissors" = 1),
                                                    selected = 2),
                                                  numericInput("size","Size ( Don't input 100000 up )",value = 100)
                                                  ),
           "dice Big-Small(probability)" = c(selectInput("dynamic", "choose game way",
                                         choices = c("Big"   = 1,
                                                     "Small" = 2),
                                         selected = 1),
                                        numericInput("size","Size ( Don't input 100000 up )",value = 100)
                                        ),
           "Guess Number(probability)" = c(radioButtons("dynamic", "choose you want",
                                         choices = c("1" = 1,
                                                     "2" = 2,
                                                     "3" = 3,
                                                     "4" = 4,
                                                     "5" = 5 ),
                                         selected = 1 ),
                                         numericInput("size","Size ( Don't input 100000 up )",value = 100)
                                         ),
           "bat-tiger-hen-bug(probability)" = c(radioButtons("dynamic", "choose you want",
                                              choices = c("bat"   = 1,
                                                          "tiger" = 2,
                                                          "hen"   = 3,
                                                          "bug"   = 4),
                                              selected = 1 ),
                                              numericInput("size","Size ( Don't input 100000 up )",value = 100)
                                              )
    )
  })
 
  A <- reactive({     
    if (input$input_type == "rock-paper-scissors"){
      x=sample(1:3, 1)
      if (x == 1){
          z = "Scissors"
        } else if (x == 2){
          z = "Rock"
        } else {z = "Paper"}
      if(!is.null(input$dynamic)){
        y=as.integer(input$dynamic)
        u=y-x
        if(u == 1 || u==-2) {
          paste("Computer's choice is:", z, "\nYou win! Congratulation!!!")
          } else 
          if(u == 2 || u == -1) {
          paste("Sorry! You lose!!! \nComputer's choice is:", z)  
          }else {paste("Tie!!!Try again. \nComputer's choice is:", z)}
      }
    } else if (input$input_type == "dice Big-Small"){
      x=sample(1:6,1)
      y=sample(1:6,1)
      if(x == 1){a="point 1"}else if(x == 2){a="point 2"}else if(x == 3){a="point 3"}else if(x == 4){a="point 4"}else if(x == 5){a="point 5"}else{a="point 6"}
      if(y == 1){b="point 1"}else if(y == 2){b="point 2"}else if(y == 3){b="point 3"}else if(y == 4){b="point 4"}else if(y == 5){b="point 5"}else{b="point 6"}
      
      if(input$dynamic == 1){
        if(x > y){paste("Your point is:", a,"COM point is:", b,"\nYOU WIN ")}else
          if(x < y){paste("Your point is:", a,"COM point is:", b,"\nYOU LOSE")}
      else{paste("Your point is:", a,"COM point is:", b,"\n PLAY AGAIN ")}
      }
      else{
        if(x < y){paste("Your point is:", a,"COM point is:", b,"\nYOU WIN ")}else
          if(x > y){paste("Your point is:", a,"COM point is:", b,"\nYOU LOSE")}
        else{paste("Your point is:", a,"COM point is:", b,"\n PLAY AGAIN ")}
      }
    }else if(input$input_type == "Guess Number"){
      x=sample(1:5,1)
      y=as.integer(input$dynamic)
      if(x == y ){paste("Your number is:",y,"right number is",x,"\nYOU WIN")}
      else{
        paste("Your chose is:",y,"COM chose is",x,"\nYOU LOSE")
      }
    }else if(input$input_type == "bat-tiger-hen-bug"){
      y=sample(1:4,1)
      x=as.integer(input$dynamic)
      if (y == 1){
        z = "bat"
      } else if (y == 2){
        z = "tiger"
      }else if (y == 3){
        z = "hen"
      }
      else {z = "bug"}
      
      if(x==1&&y==2 || x==2&&y==3 || x==3&&y==4 || x==4&&y==1){paste("Computer's choice is:", z, "\nYou win! Congratulation!!!")}
      else if(y==1&&x==2 || y==2&&x==3 || y==3&&x==4 || y==4&&x==1){paste("Sorry! You lose!!! \nComputer's choice is:", z)}
      else{paste("Computer's choice is:", z,"\nTie again")}
    }
    else if (input$input_type == "rock-paper-scissors(probability)"){
      p=0
      n=as.integer(input$size)
      if(!is.null(input$size)){
      for (i in 1:n){
          y=as.integer(input$dynamic)
          x=sample(1:3, 1)
          u=y-x
          if(u == 1 || u==-2) {
            p=p+1
          } else {
            p
          } 
          PB=p/n
      }
      paste("When you Run",n,"\nYou can get {Winner} probability is:",PB)
    }
  }else if (input$input_type == "dice Big-Small(probability)"){
    p=0
    n=as.integer(input$size)
    if(!is.null(input$size)){
      for(i in 1:n){
        x=sample(1:6,1)
        y=sample(1:6,1)
        if(input$dynamic == 1){
          if(x > y){
            p=p+1
          }
          else{
            p
          }
        }
          else {
            if(x < y){
              p=p+1
            }
            else{
              p
            }
          }
          PB=p/n
      }
      paste("When you Run",n,"\nYou can get {Winner} probability is:",PB)
     }
    }else if(input$input_type == "Guess Number(probability)"){
      p=0
      n=as.integer(input$size)
      if(!is.null(input$size)){
        for(i in 1:n){
          x=sample(1:5,1)
          y=as.integer(input$dynamic)
          if(x == y ){
            p=p+1
          }
          else{
            p
          }
          PB=p/n
        }
        paste("When you Run",n,"\nYou can get {Winner} probability is:",PB)
      }
    }
  else {
    p=0
    n=as.integer(input$size)
    if(!is.null(input$size)){
      for(i in 1:n){
        y=sample(1:4,1)
        x=as.integer(input$dynamic)
        if(x==1&&y==2 || x==2&&y==3 || x==3&&y==4 || x==4&&y==1){
          p=p+1
        }
        else{
          p
        }
        PB=p/n
      }
      paste("When you Run",n,"\nYou can get {Winner} probability is:",PB)
    }
  }
  })
  
  
  output$input_type <- renderText({
    A()
  }) 
})
