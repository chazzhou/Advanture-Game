# Advanture Game
 Northwestern Fall 2020 CS111 Final Project
 
By Sean Carlson and Charles Zhou

Our text adventure game takes place in a prison. There's more details given when you start the game, but your goal is to escape. 

You will need to take, eat, and drink food and drinks to survive as your hunger and thirst go down.


There are a few types you will need to know how to use. 

Some objects are a subtype of prop called storage. They can hold other objects inside. Open them with (open (the name)) and
the storage's contents will be listed to you. Take what you like with (take name) as usual. Close the storage with (close). 

Another type of object is keycard. Once you have taken a keycard, it can be used to pass through specific locked doors.

Disguise: can be worn with (wear disguise) and removed with (remove-disguise). Use this to avoid security cameras. While wearing, there is a penalty of increased hunger and thirst.

Laptop: can be charged in a room with (charge (the laptop)) in a room with a power outlet if you also have a laptop charger. Can be used to hack security cameras with 
(hack (the securitycam)).

The type securitycam will trigger a loss if the player is in a room with it. This can be avoided in two ways: the use of a disguise subtype, will protect the player
from detection. The other way is to hack the securitycam using a laptop. 

Don't forget to use (examine thing) for more information about objects.

