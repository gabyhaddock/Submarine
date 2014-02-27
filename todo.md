# TODO:

## misc
* Move Sub out of record syntax? it got smaller...
* Validate original sub, make sure that each hatch describes a room in the room list

## moveRooms 


## openHatches
* allow flipped pairs in updateHatchesWith?




## prune result set
* Two GameStates are equivalent if they have equivalent Subs (equivalent subs = same room states, ignore hatches), and equal final rooms.  For equivalent sets of game states, choose the move list that has the lowest cost.
* For now, just prune at the end.  Premature pruning may result in an infinite list (make a useless move, prune back, make the same useless move). If we need early pruning, we may want to limit the number of iterations to something sane.

* Idea: on a list of GameStates, first GroupBy the final room & sub state, then SortBy cost within each list, then take the head of each list.  Figure out how to get GroupBy to pull equivalent gamestates if they aren't adjacent -- or figure out how to get them adjacent (maybe an initial sort?)


## Data output
* I like the idea of outputting JSON to represent each GameState and building a small js/canvas app to render the options.


# Done:
## moveRooms 
* Modify moveRooms to find the most recent Move action & treat that as the current room (ignore recent Hatch Open actions). currentRoom function
* Add error handling for initial states that have no moves, or no current rooms
* Write a wrapper for Sub + Room num and create initial move from it: function startGame.  Test: startGame mini 1 (success), startGame mini 5 (fails)
* add rule that the player cannot leave a room on fire (they can open a hatch, though.)

## openHatches
* Create parallel to moveRooms, openHatches.  
  * This should modify the hatches list and also modify the rooms list to handle any water flow.  
  * Blocked hatches cannot be opened

## takeTurn
* Create takeTurn function that will take a game state, perform moveRooms and openHatches and concatenate the results.  (renamed plusDepth)
