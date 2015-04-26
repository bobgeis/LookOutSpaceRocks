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

-- Glory on their wings


-- This is an attempt to make the game Asteroids in elm.


{--
Some things learned:

1) Always make sure your multiway ifs includes an "otherwise" and your
    ifs include an "else".  The compiler will not catch this!!!!
    And if you miss it, your program will hang!


--}


{--

Four Sections:
1) Signals
2) Models
3) Updates
4) Views

--}





-- 1) Signals

main = view <~ Window.dimensions ~ foldGame


foldGame : Signal Game
foldGame = Signal.foldp updateGame startGame allInputs


tick : Signal Time.Time
tick = Time.inSeconds <~ Time.fps 35

allInputs : Signal Input
allInputs = Signal.sampleOn tick <|
               Input <~ tick 
                      ~ Keyboard.arrows 
                      ~ Keyboard.space 
                      ~ Keyboard.ctrl 
                      ~ Time.every (Time.second * 3)





-- 2) Models


type alias Input =
    { tick : Time.Time
    , arrows : {x:Int,y:Int}
    , space : Bool
    , ctrl : Bool
    , currentTime : Time.Time
    }

-- some constants
-- the size of the game board
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

-- asteroid sizes    
type Size = Small | Medium | Large | Huge

rockVChange = 50

-- bullet
type alias Bullet =
    Object { age: Time.Time }

bulletSpeed = 500
bulletMaxAge = 1.0
bulletReload = 0.1

-- explosion
type alias Explosion =
    Object { age: Time.Time }

explosionMaxAge = 0.2
explosionGrowthRate = 100

-- loot
type alias Loot =
    Object { kind : LootKind , ang : Float , vang : Float, age : Time.Time }

type LootKind = Lifeboat | Crystal

lootMaxAge = 20
lootRadius = 3

-- the star base
type alias Base =
    Object {ang : Float}

-- transports    
type alias Transport =
    Object { ang : Float, imageName : String }

transportSpeed = 50    

-- this must contain all the stuff in the game!
type alias Game =
    { state : State 
    , player : Player
    , rocks : List Rock 
    , bullets : List Bullet
    , time : Time.Time
    , explosions : List Explosion
    , loot : List Loot
    , lootCaught : (Int,Int)
    , lootSaved : (Int,Int)
    , base : Base 
    , seed : Random.Seed
    , transportsAway : Int
    , transports : List Transport
    , hiscores : (Int,Int,Int)
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
    { x=-200,y=200,vx=0,vy=0,r=40,ang=37}


initialSeed : Random.Seed
initialSeed = Random.initialSeed 5


-- 3) Updates

quickProb : Float -> Float
quickProb junk = 
    getSeed junk |> getProbFromSeed |> fst 

getRandFromFloat : (Float,Float) -> Float -> Float
getRandFromFloat tuple junk =
    getRandRange (getSeed junk) tuple |> fst     

getProbFromSeed : Random.Seed -> (Float,Random.Seed)
getProbFromSeed seed =
    Random.generate (Random.float 0 1) seed

getRandRange : Random.Seed -> (Float,Float) -> (Float,Random.Seed)
getRandRange seed (minim,maxim) =
    Random.generate (Random.float minim maxim) seed    

getSeed : Float -> Random.Seed
getSeed junk =
    floor junk |> Random.initialSeed

wrapX : Float -> Float
wrapX x =
    if | x > halfW -> x - gameW
       | x < 0-halfW -> x + gameW 
       | otherwise -> x

wrapY : Float -> Float
wrapY y =
    if | y > halfH -> y - gameH
       | y < 0-halfH -> y + gameH
       | otherwise -> y
        

moveObj : Time.Time -> Object a -> Object a
moveObj t obj =
    { obj | x <- obj.x + t*obj.vx |> wrapX 
          , y <- obj.y + t*obj.vy |> wrapY 
          }

isOffscreen : Object a -> Bool
isOffscreen obj =
    if | abs obj.x > halfW -> True
       | abs obj.y > halfH -> True
       | otherwise -> False 

isColliding : Object a -> Object b -> Bool
isColliding obj1 obj2 =
    (obj1.x - obj2.x)^2 + (obj1.y - obj2.y)^2
    <= (obj1.r + obj2.r)^2

listCollide : List (Object b) -> Object a ->  List (Object a, Object b)
listCollide list a  =
    let
    filter a b = 
        if isColliding a b then Just (a,b) else Nothing
    in
    List.filterMap (filter a) list

getVec : Float -> { x:Float, y:Float}
getVec ang =
    { x = cos ang
    , y = sin ang
    }          
    

getRadius : Size -> Float
getRadius size =
    case size of
        Small -> 8
        Medium -> 15
        Large -> 25
        Huge -> 35

getSmallerSize : Size -> Size
getSmallerSize size =
    case size of 
        Small -> Small
        Medium -> Small
        Large -> Medium
        Huge -> Large



updateGame : Input -> Game -> Game    
updateGame input game =
    if game.state == Pause then maybeUnpause input game else
    if input.ctrl then maybePause input game else
    game 
    |> updatePlayer input                       -- update the player
    |> updateBullets input                      -- update the bullets
    |> updateRocks input                        -- update the asteroids
    |> updateExplosions input                   -- update the explosions
    |> updateLoot input                         -- update the loot items
    |> updateTransports input                   -- update the transports
    |> updateCollisions input                   -- collide bullets and rocks
    |> timerSpawns input                         -- maybe create new rocks



maybeUnpause : Input -> Game -> Game
maybeUnpause input game =
    if input.space == False then game else
    { game | state <- Play }

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
            if isColliding player game.base then
                let 
                (x1,y1) = game.lootCaught
                (x2,y2) = game.lootSaved
                in
                ((0,0),(x1+x2,y1+y2))
            else (game.lootCaught,game.lootSaved)
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
    base = game.base
    base' = { base | ang <- base.ang + 20 * t}
    in
    { game | rocks <- rocks' 
            , base <- base'}

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
    {x = obj.x, y = obj.y, vx = 0, vy = 0, r = 10, age = 0}

createCrystal : Object a -> Maybe Loot
createCrystal obj = 
    if quickProb (obj.x * obj.y) > 0.3 then Nothing else
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



--}




-- 4) Views


view : (Int,Int) -> Game -> Element
view (w,h) game = 
    container w h middle <|             -- a container for our game :)
    collage gameW gameH                 -- this makes the game board
    [ viewSky                           -- draw the stars
    , viewBase game.base                -- draw the starbase
    , viewBullets game.bullets          -- draw the bullets
    , viewLoot game.loot                -- draw the loot items
    , viewTransports game.transports    -- draw the transports
    , viewPlayer game.player            -- draw the player
    , viewRocks game.rocks              -- draw the asteroids
    , viewExplosions game.explosions    -- draw the explosions
    , viewText game                     -- draw screen text
    , viewPauseText game                -- draw pause text
    , viewHiscores game                 -- draw the high scores
    ]
    


viewBackground : Form
viewBackground =
    rect gameW gameH |> filled black
    

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

-- remember Collage.group : List Form -> Form
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
            |> toForm |> move (0,200)
        , drawText "Press control to reset and play again!" 
            |> toForm |> move (0,180)
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
    o = 1
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
    image diameter diameter "images/Grey2.png" |> toForm 
    |> rotate (degrees base.ang)
    |> move (base.x,base.y)


viewText : Game -> Form
viewText game =
    let
    (crystalS,lifeboatS) = game.lootSaved
    stringS = "Crystals delivered: " ++ (toString crystalS) 
             ++ "    Survivors rescued: " ++ (toString lifeboatS)
             ++ "    Transports escaped: " ++ ( toString game.transportsAway)
    in
    drawText stringS |> toForm |> move (0,-270)

viewPauseText : Game -> Form
viewPauseText game =
    (if game.state == Play then [drawText " " |> toForm] else
    [ drawText "LOOK OUT! SPACE ROCKS!" |> toForm |> move (0,80)
    , drawText "arrow keys to move" |> toForm |> move (0,60)
    , drawText "space bar to unpause and fire" |> toForm |> move (0,40)
    , drawText "deliver loot to the base" |> toForm |> move (0,20)
    , drawText "control key to pause or reset" |> toForm |> move (0,0)
    ]) |> group |> move (0,180)  

    
drawText : String -> Element
drawText message =
    centered 
    ( Text.fromString message |> Text.color lightBlue) 


viewSky : Form
viewSky = 
    image gameW gameH "images/stars.jpg"
    |> toForm

viewHiscores : Game -> Form
viewHiscores game =
    let 
    (x,y,z) = game.hiscores
    string = if (x,y,z)==(0,0,0) then " " else
            "High scores - Crystals: " ++ (toString x)
             ++ "    Survivors: " ++ (toString y)
             ++ "    Transports: " ++ (toString z)
    in
    drawText string |> toForm |> move (0,-290)




