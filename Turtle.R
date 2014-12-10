library(TurtleGraphics)
library(rstackdeque)

turtle_init(mode = "clip")
print(turtle_getpos())
turtle_forward(10)
turtle_turn(45, "left")
turtle_forward(10)

turtle_turn(45, "left")
turtle_up()
turtle_backward(20)
turtle_down()
turtle_backward(10)


print(turtle_getangle())

turtle_getstate <- function(){
  state <- c(turtle_getpos(), turtle_getangle())
  return(state)
}

turtle_setstate <- function(s){
  turtle_setpos(s[1], s[2])
  turtle_setangle(s[3])
}

turtle_init(mode = "clip")
turtle_hide()

for (i in seq(1, 20)){
  turtle_forward(i)
  turtle_turn(30, "left")
}


simple_tree <- function(size){
  stored_state <- turtle_getstate()
  
  if (size <= 1) return()
  
  turtle_forward(size)
  turtle_turn(30, "left")
  turtle_forward(size/2)
  
    simple_tree(size*0.65)
  
  turtle_up()
  turtle_backward(size/2)
  turtle_turn(30, "right")
  turtle_down()
  turtle_turn(35, "right")
  turtle_forward(size/2)
    simple_tree(size*0.65)
  turtle_setstate(stored_state)
}


turtle_init(mode = "clip")
turtle_hide()
turtle_setstate(c(50, 0, 0))
simple_tree(20)



