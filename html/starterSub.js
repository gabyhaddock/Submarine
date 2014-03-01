var starterSub = { "rooms": [
{ "number": 1 , "state": "LowFlood" },
{ "number": 2 , "state": "Fire" },
{ "number": 3 , "state": "HighFlood" },
{ "number": 4 , "state": "LowFlood" },
{ "number": 5 , "state": "Fire" },
{ "number": 6 , "state": "HighFlood" },
{ "number": 7 , "state": "LowFlood" },
{ "number": 8 , "state": "Fire" },
{ "number": 9 , "state": "HighFlood" },
{ "number": 10 , "state": "LowFlood" }
] , 
 "hatches": [
{ "number": "1-2" , "state": "Closed" },
{ "number": "1-3" , "state": "Blocked" },
{ "number": "2-3" , "state": "Blocked" },
{ "number": "2-4" , "state": "Blocked" },
{ "number": "2-5" , "state": "Open" },
{ "number": "3-4" , "state": "Open" },
{ "number": "4-5" , "state": "Blocked" },
{ "number": "5-6" , "state": "Open" },
{ "number": "5-7" , "state": "Open" },
{ "number": "5-8" , "state": "Open" },
{ "number": "7-8" , "state": "Blocked" },
{ "number": "7-9" , "state": "Open" },
{ "number": "8-9" , "state": "Open" },
{ "number": "8-10" , "state": "Blocked" },
{ "number": "9-10" , "state": "Open" }
]}


var finalState = { "sub": { "rooms": [
{ "number": 1 , "state": "LowFlood" },
{ "number": 2 , "state": "Fire" },
{ "number": 3 , "state": "HighFlood" },
{ "number": 4 , "state": "LowFlood" },
{ "number": 5 , "state": "Fire" },
{ "number": 6 , "state": "HighFlood" },
{ "number": 7 , "state": "LowFlood" },
{ "number": 8 , "state": "Fire" },
{ "number": 9 , "state": "HighFlood" },
{ "number": 10 , "state": "LowFlood" }
] , 
 "hatches": [
{ "number": "1-2" , "state": "Closed" },
{ "number": "1-3" , "state": "Blocked" },
{ "number": "2-3" , "state": "Blocked" },
{ "number": "2-4" , "state": "Blocked" },
{ "number": "2-5" , "state": "Open" },
{ "number": "3-4" , "state": "Open" },
{ "number": "4-5" , "state": "Blocked" },
{ "number": "5-6" , "state": "Open" },
{ "number": "5-7" , "state": "Open" },
{ "number": "5-8" , "state": "Open" },
{ "number": "7-8" , "state": "Blocked" },
{ "number": "7-9" , "state": "Open" },
{ "number": "8-9" , "state": "Open" },
{ "number": "8-10" , "state": "Blocked" },
{ "number": "9-10" , "state": "Open" }
]}
, 
 "actions": [
{ "type": "Move", "room": { "number": 5 , "state": "Clear" }, "cost": 1 },
{ "type": "OpenHatch", "hatch": { "number": "4-5" , "state": "Open" }, "cost": 1 },
{ "type": "Move", "room": { "number": 4 , "state": "Clear" }, "cost": 1 },
{ "type": "OpenHatch", "hatch": { "number": "2-4" , "state": "Open" }, "cost": 1 },
{ "type": "Move", "room": { "number": 2 , "state": "Clear" }, "cost": 1 },
{ "type": "OpenHatch", "hatch": { "number": "1-2" , "state": "Open" }, "cost": 1 },
{ "type": "Move", "room": { "number": 1 , "state": "Clear" }, "cost": 0 }
], 
 "finalRoom": { "number": 4 , "state": "Clear" }, 
 "totalCost": 4}
