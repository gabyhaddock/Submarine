# TODO:

## moveRooms 
* Modify moveRooms to find the most recent Move action & treat that as the current room (ignore recent Hatch Open actions). create currentRoom function?

* Write a wrapper for Sub + Room num and create initial move from it
* Add error handling for initial states that have no moves, or no current rooms

## openHatches
* Create parallel to moveRooms, openHatches.  
  * This should modify the hatches list and also modify the rooms list to handle any water flow.  
  * Blocked hatches cannot be opened

## takeTurn
* Create takeTurn function that will take a game state, perform moveRooms and openHatches and concatenate the results

## Prune result set
* Two GameStates are equivalent if they have equivalent Subs (equivalent subs = same room states, ignore hatches), and equal final rooms.  For equivalent sets of game states, choose the move list that has the lowest cost.
* For now, just prune at the end.