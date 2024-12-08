module Day06

let parse input =
    let rows= split "\n" input
    let maxX, maxY = lastidx rows, lastidx rows[0]

    seq { for x in 0..maxX do for y in 0..maxY do if rows[x][y] = '#' then yield x, y elif rows[x][y] = '^' then yield -x, -y } 
    |> partition (fst >> lessThan 0)
    |> fun (guards, obstructions) -> head guards |> spread (mul -1), Set obstructions, (maxX, maxY)

let rotate (x, y) = y, x * -1
let moveGuard (moveX, moveY) (guardX, guardY) = guardX + moveX, guardY + moveY
let stillIn xMax yMax (x, y) = not (x < 0 || y < 0 || x > xMax || y > yMax)

let patrolRoute obstructions (aim, guard) =
    let potentialGuard = moveGuard aim guard

    if Set.contains potentialGuard obstructions then 
        let nextTranslation = rotate aim
        let nextGuard = moveGuard nextTranslation guard

        Some (nextGuard, (nextTranslation, nextGuard))
    else Some (potentialGuard, (aim, potentialGuard))

let guardPath router seed obstructions xMax yMax = unfold (router  obstructions) seed |> takeWhile (stillIn xMax yMax)

let part1 input =
    let guard, obstructions, (xMax, yMax) = parse input

    guardPath patrolRoute ((-1, 0), guard) obstructions xMax yMax |> Set |> Set.add guard |> length

let sameGradient (deltaX, deltaY) (x, y) (x1, y1) = 

    if float deltaX = 0.0 && float x = float x1
    then ((float y1 - float y) / (abs (float y1 - float y))) = float deltaY
    elif float deltaY = 0.0 && float y = float y1 then ((float x1 - float x) / (abs (float x1 - float x))) = float deltaX
    else false

let closest (x, y: int) (x1, y1) = (abs (x - x1)) + (abs (y - y1))
let rec patrolRoute2 obstructions (collisions, aim, guard) =
    let incomingObstructions = Set.filter (sameGradient aim guard) obstructions

    if isEmpty incomingObstructions then 0
    else
        let closestObstruction = minBy (closest guard) incomingObstructions
        let beforeCollision = moveGuard (spread (mul -1) aim) closestObstruction
        let collision = closestObstruction, beforeCollision

        if contains collision collisions then 1
        else patrolRoute2 obstructions (collision :: collisions, rotate aim, beforeCollision)

let part2 input =
    let guard, obstructions, (xMax, yMax) = parse input

    guardPath patrolRoute ((-1, 0), guard) obstructions xMax yMax |> Set |> Set.remove guard
    |> sumBy (flip Set.add obstructions >> flip patrolRoute2 ([], (-1, 0), guard)) 
