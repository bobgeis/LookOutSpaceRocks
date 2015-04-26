module Asteroids where

import Signal exposing ( (<~) , (~) )
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Text
import Time 
import Window
import Keyboard
import Mouse
import Maybe
import List
import Random

{--
LOOK OUT! SPACE ROCKS!

This is an attempt to make the game Asteroids in elm.  It's a learning project,
so apologies in advance for ugly code and bugs.    

There are some variations from Asteroids: you pilot an armed space ambulance, 
patrolling a patch of toroidal space near some star bases.  Try to keep 
the area clean and safe.

As you destroy space rocks, valuable crystals may be discovered, which you
can pick up by flying over them, and deliver by flying to the guild base.

There will be civilian transports that fly through the region and then warp
out.  If they hit a rock, they will launch lifeboats.  Also you will
be given credit for every transport that passes through without hitting a
rock.    
--}

{--
Credits:

Code: by me

Sprites: by me using paint

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

{--
Some things learned:

1) Always make sure your multiway ifs include an "otherwise".
    The compiler will not catch this!!!!
    And if you miss it, your program can hang!

2) If you don't start your module with "module _ where", then it will run fine
    in elm reactor, and *appear* to compile properly in elm make, but it will
    not run from the index.html.  You'll get a js error instead. 

3) Doing randomness properly is tricky. Elm doesn't allow you to do a lot of 
    impure things that would be taken for granted in other languages, like
    simple rand functions.  Instead you create a generator, and then
    feed it a seed, which returns the number(s) of interest, and a new seed.
    What you should then do is keep track of the seed and update it every
    time you call the RNG.  By the time I wanted to implement randomness
    this would have been more complicated than I cared to do, so I faked it 
    by giving the RNG arbitrary seeds based on game state.  
    This is ugly. 
    A game like asteroids doesn't mind this much, but a game like chess would: 
    It wouldn't be good if the chess AI always chose the exact same move in 
    response to the player's.

4) Doing sound is also hard.  Maybe harder than making a proper RNG ;)  It can 
    be done (see elm tetris, and the timer gong), but there are complications
    and drawbacks (eg breaking elm reactor).  
--}

{--
TODO:

1) Clean up. Make nicer documentation. Remove unused and refactor ugly.
    If it's all ugly then make do. IN PROGRESS

2) Make 2 bases: one for crystal drop off, one for survivors.  This will make 
    it so the player won't just camp out on the base; they'll have an incentive
    to move between opposite corners.  DONE!

3) Metallic and carbonaceous rocks? New loot (metal)?  Different calving
    behavior?  More variety in the rocks basically.

4) Pirates? Prisoners?  Will require some AI. New updates needed:
    -Enemy Ships (would AI be a separate step?)
    -Maybe AI
    -Enemy Bullets  
    -Collision with Enemy Bullets (player, transports, rocks?)
    -Pirate survivors loot (prisoners)

5) Weaponry: homing missiles?

6) Make UI nicer?  Yellow text X(  Maybe put boxes around the text?  
    Well now it's all blue.  Better?  

7) User suggestions: 
    - Don't let the ship be hidden by text. 

--}






{--

Four Sections:
1) Signals
2) Models
3) Updates
4) Views

--}





-- 1) Signals


-- make it go!
foldGame : Signal Game
foldGame = Signal.foldp updateGame startGame allInputs

-- make it show!
main = view <~ Window.dimensions ~ foldGame

-- this is called "delta" in many examples
-- but delta is change and time isn't the only thing that can change!
tick : Signal Time.Time
tick = Time.inSeconds <~ Time.fps 35    

-- this maps together all the input signals we care about
allInputs : Signal Input
allInputs = Signal.sampleOn tick <|
               Input <~ tick 
                      ~ Keyboard.arrows 
                      ~ Keyboard.space 
                      ~ Keyboard.ctrl 
                      ~ Time.every (Time.second * 3)
                      -- ^this is used to cause some things to trigger
                      -- every 3 seconds





-- 2) Models

-- all the input types                  
type alias Input =
    { tick : Time.Time
    , arrows : {x:Int,y:Int}
    , space : Bool
    , ctrl : Bool
    , currentTime : Time.Time
    }

-- some constants
-- the size of the game board (px)
(gameW,gameH) = (800,600)
(halfW,halfH) = (400,300)


-- game states: play, pause, etc
type State = Play | Pause 

-- objects in space
type alias Object a =
    {a | x:Float, y:Float, vx: Float, vy:Float, r:Float}    

-- player
type alias Player =
    Object { ang:Float
            , reload:Float
            , dead:Bool}


-- asteroid
type alias Rock =
    Object { size:Size }

type Size = Small | Medium | Large | Huge       -- asteroid sizes

rockVChange = 50                    -- when rocks explode +/- dv

-- bullet
type alias Bullet =
    Object { age: Time.Time }

bulletSpeed = 500                   -- bullet muzzle speed (px/sec)
bulletMaxAge = 1.0                  -- how long a bullet lasts (seconds)
bulletReload = 0.1                  -- how long it takes to reload (seconds)

-- explosion
type alias Explosion =
    Object { age: Time.Time }

explosionMaxAge = 0.2               -- explosions last this long (seconds)
explosionGrowthRate = 110           -- explosions grow this fast (px/sec)
explosionInitialRadius = 11         -- explosions start this big (px)

-- loot
type alias Loot =
    Object { kind : LootKind , ang : Float , vang : Float, age : Time.Time }

type LootKind = Lifeboat | Crystal  -- kinds of loot (crystal, lifeboat, etc)

lootMaxAge = 20                     -- loot lasts this long (seconds)
lootRadius = 3                      -- loot radius (px)

-- the star base and guild base
type alias Base =
    Object {ang : Float, imageName : String }

-- transports    
type alias Transport =
    Object { ang : Float, imageName : String }

transportSpeed = 50                 -- the transports' speed (px/sec)

-- this must contain all the stuff in the game!
type alias Game =
    { state : State 
    , player : Player
    , rocks : List Rock 
    , bullets : List Bullet
    , time : Time.Time
    , explosions : List Explosion
    , loot : List Loot
    , lootCaught : (Int,Int)        -- (# crystals, # lifeboats)
    , lootSaved : (Int,Int)         -- ^same
    , base : Base 
    , guild : Base 
    , seed : Random.Seed            -- for passing to RNG, but curr unused
    , transportsAway : Int
    , transports : List Transport
    , hiscores : (Int,Int,Int)      -- (# crystals, # lifeboats, # transports)
    }

-- this is the initial state of the game, needed for Signal.foldp
-- everything defined in the Game type alias must be initialized
startGame : Game
startGame = 
    { state = Pause
    , player = startPlayer
    , rocks = startRocks
    , bullets = []
    , time = 0
    , explosions = []
    , loot = []
    , lootCaught = (0,0)
    , lootSaved = (0,0)
    , base = startBase
    , guild = startGuild
    , seed = initialSeed
    , transportsAway = 0
    , transports = []
    , hiscores = (0,0,0)
    }

startPlayer : Player
startPlayer = 
    { x=0, y=0, ang=degrees 90
    , vx=0, vy=0, r=10 
    , reload=0 , dead=False}


startRocks : List Rock
startRocks = 
    [
    { x=200,y=200,vx=50,vy=-50,r=getRadius Huge,size=Huge}
    ]


startBase : Base
startBase =
    { x=-200,y=200,vx=0,vy=0,r=40,ang=37,imageName="images/Grey2.png"}

startGuild : Base
startGuild = 
    { startBase | imageName <- "images/DomeBuilder0.png"
                , x <- 200, y <- -200, r<-30, ang <- 172 }

initialSeed : Random.Seed
initialSeed = Random.initialSeed 5


-- 3) Updates

-- quick and dirty random probability
-- note that it is very easy to get the same result multiple times
-- be careful!
quickProb : Float -> Float
quickProb junk = 
    getSeed junk |> getProbFromSeed |> fst 

-- if you have a seed and want a probability, use this
-- it returns the new seed which you can use in future calls
-- to ensure you don't get the same result     
getProbFromSeed : Random.Seed -> (Float,Random.Seed)
getProbFromSeed seed =
    Random.generate (Random.float 0 1) seed

-- if you want to use a Float for a random seed, use this
getSeed : Float -> Random.Seed
getSeed junk =
    floor junk |> Random.initialSeed

-- wrap x coordinates so they stay on screen
-- remember (0,0) is at the center!
wrapX : Float -> Float
wrapX x =
    if | x > halfW -> x - gameW
       | x < 0-halfW -> x + gameW 
       | otherwise -> x

-- wrap y coordinates so they stay on screen
-- remember (0,0) is at the center!
wrapY : Float -> Float
wrapY y =
    if | y > halfH -> y - gameH
       | y < 0-halfH -> y + gameH
       | otherwise -> y
        
-- move with wrapping
moveObj : Time.Time -> Object a -> Object a
moveObj t obj =
    { obj | x <- obj.x + t*obj.vx |> wrapX 
          , y <- obj.y + t*obj.vy |> wrapY 
          }

-- is this object off the screen? 
-- (objects that wrap should never do this, but not all objects wrap)
isOffscreen : Object a -> Bool
isOffscreen obj =
    if | abs obj.x > halfW -> True
       | abs obj.y > halfH -> True
       | otherwise -> False 

-- are these two objects colliding?
isColliding : Object a -> Object b -> Bool
isColliding obj1 obj2 =
    (obj1.x - obj2.x)^2 + (obj1.y - obj2.y)^2
    <= (obj1.r + obj2.r)^2

-- collide every object in the list with the object given
-- then return every collsion as a pair of colliding objects
listCollide : List (Object b) -> Object a ->  List (Object a, Object b)
listCollide list a  =
    let
    filter a b = 
        if isColliding a b then Just (a,b) else Nothing
    in
    List.filterMap (filter a) list

-- get a unit vector pointed allow the given angle
-- ang should already be in elm's internal angle unit (radians)
-- so use (degrees x) or w/e *before* calling this
getVec : Float -> { x:Float, y:Float}
getVec ang =
    { x = cos ang
    , y = sin ang
    }          
    
-- given an asteroid's size, get its radius
getRadius : Size -> Float
getRadius size =
    case size of
        Small -> 8
        Medium -> 15
        Large -> 25
        Huge -> 35

-- given an asteroid's size, get the size of an asteroid one step smaller
-- This is used by the asteroid calving functions.
-- Small returns Small at the moment, but it probably shouldn't be used.
-- You'd get tiny asteroids that never ever die.
getSmallerSize : Size -> Size
getSmallerSize size =
    case size of 
        Small -> Small
        Medium -> Small
        Large -> Medium
        Huge -> Large


-- this updates the whole game state by one tick
-- Many of the sub systems require access to disparate parts of the game state
-- so it was easier to have them take and return the entire game model.
-- ^this seems like a dubious practice, consider cleaner methods
updateGame : Input -> Game -> Game    
updateGame input game =
    if game.state == Pause then maybeUnpause input game else
    if input.ctrl then maybePause input game else
    game 
    |> updatePlayer input                       -- update the player
    |> updateBullets input                      -- update the bullets
    |> updateRocks input                        -- update the asteroids
    |> updateBase input                         -- update the bases
    |> updateExplosions input                   -- update the explosions
    |> updateLoot input                         -- update the loot items
    |> updateTransports input                   -- update the transports
    |> updateCollisions input                   -- collide bullets and rocks
    |> timerSpawns input                        -- maybe create new rocks, etc


-- if the game is paused, then we might want to unpause
maybeUnpause : Input -> Game -> Game
maybeUnpause input game =
    if input.space == False then game else
    { game | state <- Play }

-- if the game isn't paused, then we might want to pause it, or reset it
maybePause : Input -> Game -> Game
maybePause input game = 
    let 
    player = game.player
    in
    if player.dead then 
        let 
        (x',y') = game.lootSaved
        z' = game.transportsAway
        (x,y,z) = game.hiscores
        hiscores' = (max x x',max y y',max z z')
        in
        { startGame | hiscores <- hiscores' }
    else
        { game | state <- Pause }

-- the player is pretty complicated.  this handles player:
--      shooting, moving, maneuvering, gathering, delivering, and dying
updatePlayer : Input -> Game -> Game
updatePlayer input game =
    if game.player.dead == True then game else
    let player = game.player
        dead' = List.any (isColliding player) game.rocks
        explosions' = if dead' then createExplosion player :: game.explosions
                      else game.explosions
        vec = getVec player.ang
        t = input.tick
        reload' = case player.reload of
            0 -> if input.space then bulletReload else 0
            x -> max 0 (x-t)
        bullets' = if reload' == bulletReload |> not then game.bullets
            else spawnBullet player game :: game.bullets
        thrust = case input.arrows.y of 
                1 -> 200
                (-1) -> -50
                0 -> 0
        delAng = toFloat input.arrows.x * 5.0 |> degrees
        vx' = player.vx + vec.x * thrust * t
        vy' = player.vy + vec.y * thrust * t
        damp t  = 1 - 0.25 * t
        (lootCaught',lootSaved') = 
            let 
            (x1,y1) = game.lootCaught
            (x2,y2) = game.lootSaved
            in
            if isColliding player game.base then ((x1,0),(x2,y1+y2)) else
            if isColliding player game.guild then ((0,y1),(x1+x2,y2)) else
            (game.lootCaught,game.lootSaved)
        player' = 
            { player | vx <- vx' * damp t
            , vy <- vy' * damp t
            , ang <- player.ang - delAng
            , reload <- reload'
            , dead <- dead'
            } |> moveObj t
    in
    { game | player <- player'
            , bullets <- bullets'
            , explosions <- explosions'
            , lootCaught <- lootCaught'
            , lootSaved <- lootSaved'
            }

-- this takes a Player record rather than Object a, only because it 
-- needs an angle field.  Hmm
-- also it doesn't need the Game argument currently. 
-- under what circumstances would it make use of the Game argument?  
-- maybe if it implemented homing missiles and needed to find targets?
spawnBullet : Player -> Game -> Bullet
spawnBullet player game =
    let
    vec = getVec player.ang
    in
    { x = player.x
    , y = player.y
    , vx = player.vx + vec.x * bulletSpeed
    , vy = player.vy + vec.y * bulletSpeed
    , r = 2, age = 0 }


updateBullets : Input -> Game -> Game
updateBullets input game =
    let
    bullets' = List.filterMap (updateBullet input game) game.bullets
    in
    { game | bullets <- bullets'
        }

updateBullet : Input -> Game -> Bullet -> Maybe Bullet
updateBullet input game bullet =
    let
    t = input.tick
    age' = bullet.age + t
    in
    if age' > bulletMaxAge then Nothing else
    { bullet | age <- age' }
    |> moveObj t |> Just

-- this updates transports' position, removes them if they collide or escape
-- and handles the consquences of transport removal
updateTransports : Input -> Game -> Game
updateTransports input game =
    let
    (transports',transportsAway') = 
        List.foldl (foldTransports input) 
        ([],game.transportsAway) game.transports 
    transNotColl trans = List.any (\(t,r) -> t == trans) colls |> not
    colls = List.concatMap (listCollide game.rocks) transports'
    transports'' = List.filter (transNotColl) transports'
    booms = List.map (\(t,r) -> createExplosion r) colls
    lifeboats = List.concatMap (\(t,r) -> launchLifeboats t) colls
    in
    { game | transports <- transports''
            , transportsAway <- transportsAway'
            , explosions <- List.append game.explosions booms
            , loot <- List.append game.loot lifeboats 
            }

-- folding is nice. like a map that remembers
foldTransports : Input -> Transport -> (List Transport, Int) 
                -> (List Transport,Int)  
foldTransports input transport (list,away) = 
    if isOffscreen transport then (list,away+1) else
    let 
    t = input.tick
    transport' = { transport | x <- transport.x + transport.vx * t
                             , y <- transport.y + transport.vy * t } 
    list' = transport' :: list
    in
    (list',away)

launchLifeboats : Transport -> List Loot
launchLifeboats trans =
    let
    dvx = rockVChange * ( -0.5 + quickProb ( trans.x * trans.vx ) )
    dvy = rockVChange * ( -0.5 + quickProb ( trans.x * trans.vx ) )
    lb1 = {x = trans.x , y = trans.y , vx = trans.vx +dvx , vy = trans.vy +dvy
          , r = lootRadius, kind = Lifeboat , ang = 0, vang = degrees -100
          , age = 0}
    in
    [ lb1
    , { lb1 | vx <- trans.vx - dvx , vy <- trans.vy - dvy }
    ]


-- i need a shorter word or abbreviation for explosion
-- "boom" maybe?
updateExplosions : Input -> Game -> Game
updateExplosions input game =
    let
    explosions' = List.filterMap (updateExplosion input game) game.explosions
    in
    { game | explosions <- explosions' }

updateExplosion : Input -> Game -> Explosion -> Maybe Explosion
updateExplosion input game explosion =
    let
    t = input.tick
    r' = explosion.r + t*explosionGrowthRate
    age' = explosion.age + t
    in
    if age' > explosionMaxAge then Nothing else
        { explosion | age <- age' , r <- r'}
        |> Just

updateRocks : Input -> Game -> Game
updateRocks input game = 
    let
    t = input.tick
    rocks' = List.map (moveObj t) game.rocks
    in
    { game | rocks <- rocks'}


updateBase : Input -> Game -> Game
updateBase input game = 
    let 
    t = input.tick
    rotateBase b a = { b | ang <- b.ang + a }
    base' = (20*t) |> rotateBase game.base  
    guild' =  (-15*t) |> rotateBase game.guild 

    in
    { game | guild <- guild' , base <- base' }


updateLoot : Input -> Game -> Game
updateLoot input game =
    let 
    t = input.tick
    (catches,loot') = List.partition (isColliding game.player) game.loot    
    loot'' = List.map (moveObj t) loot' 
             |> List.map (\r -> {r | age <- r.age + t
                                   , ang <- r.ang + r.vang*t})
             |> List.filter (\r -> r.age < lootMaxAge) 
    lootCaught' = List.foldl foldCatches game.lootCaught catches         
    in
    { game | loot <- loot''
           , lootCaught <- lootCaught'
           }

foldCatches : Loot -> (Int,Int) -> (Int,Int)
foldCatches loot (crystal,lifeboat) =
    case loot.kind of
        Crystal -> (crystal+1,lifeboat)
        Lifeboat -> (crystal,lifeboat+1)


updateCollisions : Input -> Game -> Game
updateCollisions input game =
    let
    t = input.tick
    colls = List.concatMap (listCollide game.rocks) game.bullets
    bulletNotColl bullet = List.any (\(b,r) -> b == bullet) colls |> not
    rockNotColl rock = List.any (\(b,r) -> r == rock) colls |> not 
    bullets' = List.filter (bulletNotColl) game.bullets
    rocks' = List.filter (rockNotColl) game.rocks
    calves = List.concatMap (\(b,r) -> calve r) colls
    booms = List.map (\(b,r) -> createExplosion r) colls
    newLoot = List.filterMap (\(b,r) -> createCrystal r) colls
    in
    { game | bullets <- bullets'
        , rocks <- List.append rocks' calves
        , explosions <- List.append game.explosions booms
        , loot <- List.append game.loot newLoot
        } 

calve : Rock -> List Rock
calve rock =
    case rock.size of
        Small -> []
        _     -> let 
                 dvx = rockVChange * (-0.5 + quickProb (rock.x) )
                 dvy = rockVChange * (-0.5 + quickProb (rock.y) )
                 in
                 [  {rock | size <- getSmallerSize rock.size
                    , vx <- rock.vx + dvx
                    , vy <- rock.vy - dvy
                    , r <- getRadius <| getSmallerSize rock.size
                    }
                    , {rock | size <- getSmallerSize rock.size
                    , vx <- rock.vx - dvx
                    , vy <- rock.vy + dvy
                    , r <- getRadius <| getSmallerSize rock.size
                    }
                 ]
     

createExplosion : Object a -> Explosion
createExplosion obj =
    {x = obj.x, y = obj.y, vx = 0, vy = 0
    , r = explosionInitialRadius, age = 0}

createCrystal : Object a -> Maybe Loot
createCrystal obj = 
    if quickProb (obj.vx * obj.vy) > 0.3 then Nothing else
    let
    dvx = rockVChange * ( -0.5 + quickProb ( obj.x * obj.vx ) )
    dvy = rockVChange * ( -0.5 + quickProb ( obj.y * obj.vy ) )
    in
    {x = obj.x , y = obj.y , vx = obj.vx +dvx , vy = obj.vy +dvy
    , r = lootRadius, kind = Crystal , ang = 0, vang = degrees 270
    , age = 0}
    |> Just



timerSpawns : Input -> Game -> Game
timerSpawns input game =
    if game.time == input.currentTime then game else
    let
    transports' = if quickProb (game.time * 7) > 0.5 then game.transports else 
                  makeRandomTransport input game :: game.transports  
    rocks' = if quickProb (game.time * 2) > 0.5 then game.rocks else
             makeRandomRock input game :: game.rocks   
    in
    { game | time <- input.currentTime 
            , rocks <- rocks' 
            , transports <- transports'}


makeRandomTransport : Input -> Game -> Transport 
makeRandomTransport input game =
    let
    side = quickProb (game.time * 8)
    coord = quickProb (game.time * 9) - 0.5 
    (x,y) = if | side < 0.25 -> (coord*gameW,halfH)
               | side < 0.50 -> (coord*gameW,-halfH)
               | side < 0.75 -> (halfW,coord*gameH)
               | otherwise -> (-halfW,coord*gameH)
    (vx,vy) = if | side < 0.25 -> (0,-transportSpeed)
                 | side < 0.50 -> (0,transportSpeed)
                 | side < 0.75 -> (-transportSpeed,0)
                 | otherwise -> (transportSpeed,0)   
    ang =     if | side < 0.25 -> degrees 270        
                 | side < 0.50 -> degrees 90
                 | side < 0.75 -> degrees 180
                 | otherwise -> 0       
    imageName = if quickProb (game.time *10) > 0.5
                then "images/Civ1.png" else "images/Builder3.png"
    in
    {x=x,y=y,vx=vx,vy=vy,r=10,ang=ang,imageName=imageName}

makeRandomRock : Input -> Game -> Rock
makeRandomRock input game =
    let
    side = quickProb (game.time * 3)
    coord = quickProb (game.time * 4) - 0.5 
    (x,y) = if | side < 0.25 -> (coord*gameW,halfH)
               | side < 0.50 -> (coord*gameW,-halfH)
               | side < 0.75 -> (halfW,coord*gameH)
               | otherwise -> (-halfW,coord*gameH)
    vx = (quickProb (game.time * 5) - 0.5) * rockVChange * 2
    vy = (quickProb (game.time * 6) - 0.5) * rockVChange * 2
    in 
    {x=x,y=y,vx=vx,vy=vy,size=Huge,r= getRadius Huge}






-- 4) Views


view : (Int,Int) -> Game -> Element
view (w,h) game = 
    container w h middle <|             -- a container for our game :)
    collage gameW gameH                 -- this makes the game board
    [ viewSky                           -- draw the stars
    , viewBase game.base                -- draw the starbase
    , viewBase game.guild               -- draw the guild base
    , viewBullets game.bullets          -- draw the bullets
    , viewLoot game.loot                -- draw the loot items
    , viewTransports game.transports    -- draw the transports
    , viewRocks game.rocks              -- draw the asteroids
    , viewHiscores game                 -- draw the high scores
    , viewExplosions game.explosions    -- draw the explosions
    , viewText game                     -- draw screen text
    , viewPauseText game                -- draw pause text
    , viewPlayer game.player            -- draw the player
    ]
-- ^FIFO: things later in the list are drawn over things earlier in the list
    
-- draw a background image
viewSky : Form
viewSky = 
    fittedImage gameW gameH "images/stars.jpg"
    |> toForm

-- remember Collage.group : List Form -> Form
viewBullets : List Bullet -> Form
viewBullets bullets =
    List.map drawBullet bullets |> group

drawBullet : Bullet -> Form
drawBullet bullet =
    let
    age = bullet.age
    color = if | age < 0.4 -> white
               | age < 0.8 -> lightBlue
               | otherwise -> darkBlue 
    in
    circle bullet.r
    |> filled color
    |> move (bullet.x,bullet.y)

viewRocks : List Rock -> Form
viewRocks rocks = 
    List.map drawRock rocks |> group

drawRock : Rock -> Form
drawRock rock =
    circle rock.r 
    |> filled lightBrown
    |> move (rock.x,rock.y)

viewPlayer : Player -> Form
viewPlayer player =
    if player.dead then 
        [ drawText "You have died.  So it goes." 
            |> color darkBlue |> opacity 0.8 |> toForm |> move (0,200)
        , drawText "Press control to reset and play again!" 
            |> color darkBlue |> opacity 0.8 |> toForm |> move (0,180)
        ] |> group
    else
    image 20 20 "images/Medic1.png"
    |> toForm |> rotate (player.ang - degrees 90)
    |> move (player.x,player.y)

viewExplosions : List Explosion -> Form
viewExplosions explosions = 
    List.map drawExplosion explosions |> group

drawExplosion : Explosion -> Form
drawExplosion explosion =
    let
    ageRatio = explosion.age / explosionMaxAge
    color = if | ageRatio < 0.2 -> lightBlue
               | ageRatio < 0.5 -> white
               | ageRatio < 0.8 -> lightRed
               | otherwise -> darkRed 
    in
    circle explosion.r 
    |> filled color
    |> move (explosion.x,explosion.y)

viewLoot : List Loot -> Form
viewLoot loot =    
    List.map drawLoot loot |> group

drawLoot : Loot -> Form
drawLoot loot = 
    let
    ageRatio = loot.age / lootMaxAge
    imageName = case loot.kind of
            Crystal -> if | ageRatio < 0.2 -> "images/Small3.png"
                          | ageRatio < 0.5 -> "images/Small2.png"
                          | ageRatio < 0.8 -> "images/Small1.png"
                          | otherwise -> "images/Small0.png"
            Lifeboat -> "images/Lifeboat1.png" 
    in
    image 6 6 imageName |> toForm
    |> rotate loot.ang |> move (loot.x,loot.y)

viewTransports : List Transport -> Form
viewTransports transports =
    List.map drawTransport transports |> group

drawTransport : Transport -> Form
drawTransport transport =
    image 16 16 transport.imageName |> toForm
    |> rotate (transport.ang - degrees 90) |> move (transport.x,transport.y)    


viewBase : Base -> Form
viewBase base =
    let diameter = base.r * 2
    in
    --image diameter diameter "images/Grey2.png" |> toForm 
    image diameter diameter base.imageName |> toForm 
    |> rotate (degrees base.ang)
    |> move (base.x,base.y)


viewText : Game -> Form
viewText game =
    let
    (crystal,lifeboat) = game.lootSaved
    string = "Crystals delivered: " ++ (toString crystal) 
             ++ "    Survivors rescued: " ++ (toString lifeboat)
             ++ "    Transports protected: " ++ ( toString game.transportsAway)
    in
    drawText string 
    |> color darkBlue |> opacity 0.8 
    |> toForm |> move (0,halfH-30)

viewPauseText : Game -> Form
viewPauseText game =
    (if game.state == Play then [drawText "" |> toForm] else
    [ rect 280 140 |> filled darkBlue 
    , drawText "LOOK OUT! SPACE ROCKS!" |> toForm |> move (0,50)
    , drawText "arrow keys to move" |> toForm |> move (0,30)
    , drawText "space bar to unpause and fire" |> toForm |> move (0,10)
    , drawText "deliver crystals to the bottom right base" 
        |> toForm |> move (0,-10)
    , drawText "bring survivors to the top left base" 
        |> toForm |> move (0,-30)
    , drawText "control key to pause or reset" |> toForm |> move (0,-50)
    ]) |> group |> move (0,170)  

-- given a string message, make an element of it    
drawText : String -> Element
drawText message =
    centered 
    ( Text.fromString message |> Text.color lightBlue) 



viewHiscores : Game -> Form
viewHiscores game =
    let 
    (x,y,z) = game.hiscores
    string = if (x,y,z)==(0,0,0) then "" else
            "High scores - Crystals: " ++ (toString x)
             ++ "    Survivors: " ++ (toString y)
             ++ "    Transports: " ++ (toString z)
    in
    drawText string 
    |> color darkBlue |> opacity 0.8
    |> toForm |> move (0,halfH-10)




