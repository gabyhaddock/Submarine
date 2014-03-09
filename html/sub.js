var starterSub = { "rooms": [
{ "number": 1 , "state": "Clear" },
{ "number": 2 , "state": "HighFlood" },
{ "number": 3 , "state": "Fire" },
{ "number": 4 , "state": "Clear" }
], 
 "hatches": [
{ "number": "1-2" , "state": "Closed" },
{ "number": "1-3" , "state": "Closed" },
{ "number": "2-3" , "state": "Closed" },
{ "number": "2-4" , "state": "Closed" },
{ "number": "3-4" , "state": "Closed" }
], 
 "actions": [

], 
 "finalRoom": { "number": 1 , "state": "Clear" }, 
 "startRoom": { "number": 1 , "state": "Clear" }, 
 "totalCost": 0}; 

 var results = [
{ "rooms": [
{ "number": 1 , "state": "Clear" },
{ "number": 2 , "state": "HighFlood" },
{ "number": 3 , "state": "Fire" },
{ "number": 4 , "state": "Clear" }
], 
 "hatches": [
{ "number": "1-2" , "state": "Closed" },
{ "number": "1-3" , "state": "Closed" },
{ "number": "2-3" , "state": "Closed" },
{ "number": "2-4" , "state": "Closed" },
{ "number": "3-4" , "state": "Closed" }
], 
 "actions": [

], 
 "finalRoom": { "number": 1 , "state": "Clear" }, 
 "startRoom": { "number": 1 , "state": "Clear" }, 
 "totalCost": 0},
{ "rooms": [
{ "number": 1 , "state": "LowFlood" },
{ "number": 2 , "state": "LowFlood" },
{ "number": 3 , "state": "Fire" },
{ "number": 4 , "state": "Clear" }
], 
 "hatches": [
{ "number": "1-2" , "state": "Open" },
{ "number": "1-3" , "state": "Closed" },
{ "number": "2-3" , "state": "Closed" },
{ "number": "2-4" , "state": "Closed" },
{ "number": "3-4" , "state": "Closed" }
], 
 "actions": [
{ "type": "OpenHatch", "hatch": { "number": "1-2" , "state": "Closed" }, "cost": 1 },
{ "type": "Flood", "room1": { "number": 1 , "state": "Clear" }, "room2": { "number": 2 , "state": "HighFlood" }, "cost": 0 }
], 
 "finalRoom": { "number": 1 , "state": "Clear" }, 
 "startRoom": { "number": 1 , "state": "Clear" }, 
 "totalCost": 1},
{ "rooms": [
{ "number": 1 , "state": "LowFlood" },
{ "number": 2 , "state": "LowFlood" },
{ "number": 3 , "state": "Fire" },
{ "number": 4 , "state": "Clear" }
], 
 "hatches": [
{ "number": "1-2" , "state": "Open" },
{ "number": "1-3" , "state": "Closed" },
{ "number": "2-3" , "state": "Closed" },
{ "number": "2-4" , "state": "Closed" },
{ "number": "3-4" , "state": "Closed" }
], 
 "actions": [
{ "type": "OpenHatch", "hatch": { "number": "1-2" , "state": "Closed" }, "cost": 1 },
{ "type": "Flood", "room1": { "number": 1 , "state": "Clear" }, "room2": { "number": 2 , "state": "HighFlood" }, "cost": 0 },
{ "type": "Move", "room": { "number": 2 , "state": "LowFlood" }, "cost": 1 }
], 
 "finalRoom": { "number": 2 , "state": "LowFlood" }, 
 "startRoom": { "number": 1 , "state": "Clear" }, 
 "totalCost": 2},
{ "rooms": [
{ "number": 1 , "state": "LowFlood" },
{ "number": 2 , "state": "LowFlood" },
{ "number": 3 , "state": "Fire" },
{ "number": 4 , "state": "Clear" }
], 
 "hatches": [
{ "number": "1-2" , "state": "Open" },
{ "number": "1-3" , "state": "Closed" },
{ "number": "2-3" , "state": "Closed" },
{ "number": "2-4" , "state": "Open" },
{ "number": "3-4" , "state": "Closed" }
], 
 "actions": [
{ "type": "OpenHatch", "hatch": { "number": "1-2" , "state": "Closed" }, "cost": 1 },
{ "type": "Flood", "room1": { "number": 1 , "state": "Clear" }, "room2": { "number": 2 , "state": "HighFlood" }, "cost": 0 },
{ "type": "Move", "room": { "number": 2 , "state": "LowFlood" }, "cost": 1 },
{ "type": "OpenHatch", "hatch": { "number": "2-4" , "state": "Closed" }, "cost": 1 },
{ "type": "Move", "room": { "number": 4 , "state": "Clear" }, "cost": 0 }
], 
 "finalRoom": { "number": 4 , "state": "Clear" }, 
 "startRoom": { "number": 1 , "state": "Clear" }, 
 "totalCost": 3}
];