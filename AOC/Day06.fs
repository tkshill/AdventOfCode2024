module Day06

let parse input =
    let rows= split "\n" input
    let maxX, maxY = lastidx rows, lastidx rows[0]

    seq { for x in 0..maxX do for y in 0..maxY do if rows[x][y] = '#' then yield x, y elif rows[x][y] = '^' then yield -x, -y } 
    |> partition (fst >> lessThan 0)
    |> fun (guards, obstructions) -> head guards |> spread (mul -1), Set obstructions, (maxX, maxY)

let rotate (deltaX, deltaY) = deltaY, deltaX * -1
let moveGuard (deltaX, deltaY) (guardX, guardY) = guardX + deltaX, guardY + deltaY

let patrolRoute obstructions (aim, guard) =
    let potentialGuard = moveGuard aim guard

    if Set.contains potentialGuard obstructions then 
        let nextTranslation = rotate aim
        let nextGuard = moveGuard nextTranslation guard

        Some (nextGuard, (nextTranslation, nextGuard))
    else Some (potentialGuard, (aim, potentialGuard))

let generatePath router seed obstructions xMax yMax = 
    let stillIn (x, y) = not (x < 0 || y < 0 || x > xMax || y > yMax)

    unfold (router obstructions) seed |> takeWhile stillIn 

let part1 input =
    let guard, obstructions, (xMax, yMax) = parse input

    generatePath patrolRoute ((-1, 0), guard) obstructions xMax yMax |> Set |> Set.add guard |> length

let isOncoming (deltaX, deltaY) (guardX, guardY) (obstacleX, obstacleY) = 
    let polarity x1 x = (float x1 - float x) / (abs (float x1 - float x))

    if float deltaX = 0.0 && float guardX = float obstacleX then polarity obstacleY guardY = float deltaY
    elif float deltaY = 0.0 && float guardY = float obstacleY then polarity obstacleX guardX = float deltaX
    else false

let rec willLoop obstructions (collisions, aim, guard) =
    let distance (x, y: int) (x1, y1) = (abs (x - x1)) + (abs (y - y1))
    let incomingObstructions = Set.filter (isOncoming aim guard) obstructions

    if isEmpty incomingObstructions then 0
    else
        let closestObstruction = minBy (distance guard) incomingObstructions
        let beforeCollision = moveGuard (spread (mul -1) aim) closestObstruction
        let collision = closestObstruction, beforeCollision

        if contains collision collisions then 1
        else willLoop obstructions (collision :: collisions, rotate aim, beforeCollision)

let part2 input =
    let guard, obstructions, (xMax, yMax) = parse input

    generatePath patrolRoute ((-1, 0), guard) obstructions xMax yMax |> Set |> Set.remove guard
    |> sumBy (flip Set.add obstructions >> flip willLoop ([], (-1, 0), guard)) 
