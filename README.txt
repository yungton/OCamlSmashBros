To compile project, add the following line to your .cs3110 file "compile.opam_packages=graphics". 
This will allow the graphics module to be used. 
Then run "cs3110 compile engine.ml" while you are in our project directory.
Then run "cs3110 run engine.ml" to play. 
The game will start with a 3 second countdown, and a random character will be selected for you and the AI. 
Both of you will have 3 lives.
After someone wins, press 'y' to replay or 'n' to quit the game. 

Keyboard Controls:
w - move up/jump  *you may have 2,1, or 0 jumps left depending on the game situation*
a - move left
s - move down
d - move right 
i - attack up   *attacks will hit if you are within range (basically touching), or they will miss*
j - attack left
k - attack down
l - attack right