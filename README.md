# Advanture Game
 Northwestern Fall 2020 CS111 Final Project
 
By Sean Carlson and Charles Zhou

## Background
Our text adventure game takes place in a prison. There's more details given when you start the game, but your goal is to escape. 

You will need to take, eat, and drink food and drinks to survive as your hunger and thirst go down.

### Hunger
Hunger value goes down when you perform actions such as (go (someplace)), (take (something)). You can eat foods to regain hunger level.

### Thirst
Thirst value goes down when you perform actions such as (go (someplace)), (take (something)). You can drink beverages to regain hunger level.

### Health
If your hunger or thirst value is zero, your health value will go down when you perform actions. Your health value will regenerate when you eat or drink enough substance that your hunger and thirst values are both full.

## There are a few types you will need to know how to use. 

### Storage
Some objects are a subtype of prop called storage. They can hold other objects inside. Open them with (open (the name)) and
the storage's contents will be listed to you. Take what you like with (take name) as usual. Close the storage with (close). 

### Keycard
Another type of object is keycard. Once you have taken a keycard, it can be used to pass through specific locked doors.

### Disguise
Disguise: a type of clothing can be worn with (wear disguise) and removed with (remove-disguise). Use this to avoid security cameras. While wearing, there is a penalty of increased hunger and thirst.

### Laptop
Laptop: laptop has a battery. If the battery is not sufficient, you can not use the laptop. Laptop can be charged in a room with (charge (the laptop)) in a room with a power outlet if you also have a laptop charger. Can be used to hack security cameras with (hack (the securitycam)).

### Security Cameras
The type securitycam will trigger a loss if the player is in a room with an operational camera. This can be avoided in two ways: the use of a disguise subtype, will protect the playerfrom detection. The other way is to hack the securitycam using a laptop, but you will need to wear something to disguise yourself before you enter the room and hack the camera. After hacking the camera, you can then take your disguise off.

### Tips
Don't forget to use (examine thing) for more information about objects.
