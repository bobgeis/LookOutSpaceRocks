{--
LOOK OUT! SPACE ROCKS!

This is an attempt to make the game Asteroids in elm.  It's a learning project,
so apologies in advance for ugly code and bugs.    

A playable version of this should be available here: 
http://bobgeis.github.io/LookOutSpaceRocks/
Chrome is recommended.

There are some variations from Asteroids: you pilot an armed space ambulance, 
patrolling a patch of toroidal space near some star bases.  Try to keep 
the area clean and safe.
--}

{--
Credits:

Code: by Robert Geis

Sprites: by Robert Geis using paint

Background: Carina Nebula image from 
http://commons.wikimedia.org/wiki/Commons:Featured_pictures/Astronomy
--}

{--
Influences:

The Learn Elm site: 
    http://elm-lang.org/Learn.elm
    (for learning elm)

From there, the Make Pong page in particular: 
    http://elm-lang.org/blog/Pong.elm
    (for game structure in elm)

Another asteroids in elm implementation:
    https://github.com/CheatEx/elm-asteroids
    (unfamiliar with functional languages, I was unsure how to implement
    collision detection: folds or maps or etc? )
--}
