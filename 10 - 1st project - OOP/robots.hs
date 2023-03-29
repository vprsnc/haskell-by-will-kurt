robot (name, attack, hitpoints) = \message ->
                                    message (name, attack, hitpoints)

bender = robot ("Bender Bending Rodriguez", 25, 200)

name (n, _, _) = n
attack(_, a, _) = a
hp(_, _, hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(n, a, h) ->
                                   robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, a, h) ->
                                   robot (n, newAttack, h))
setHP aRobot newHP = aRobot (\(n, a, h) ->
                                   robot (n, a, newHP))

genderBender = setAttack weakBender 5
  where weakBender = setHP softBender 50
        softBender = setName bender "Gender Bender"

printRobot aRobot = aRobot (\(n, a, h) ->
                              n ++
                              "\n attack:"  ++ show a ++
                              "\n hitpoint" ++ show h)

damage aRobot attackDamage =
  aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight attacker defender = damage defender attack
  where attack = if getHP attacker > 0
                 then getAttack attacker
                 else 0

beefyTank = robot ("Tiny", 10, 300)

beefyTankRound1 = fight bender beefyTank
benderRound1 = fight beefyTank bender

beefyTankRound2 = fight benderRound1 beefyTankRound1
benderRound2 = fight beefyTankRound1 benderRound1

beefyTankRound3 = fight benderRound1 beefyTankRound2
benderRound3 = fight beefyTankRound1 benderRound2

-- task 1: using ~map~ get hitpoints of each robot in the list
getAllRobotsHP [] = []
getAllRobotsHP robots =
  map printRobot robots
-- task 2: make one robot attack all
attackAll attacker [] = []
attackAll attacker defenders =
  map  ( fight attacker ) defenders

allAttacked = attackAll genderBender [ bender, beefyTank ]
